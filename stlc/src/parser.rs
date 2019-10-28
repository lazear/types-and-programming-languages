use crate::lexer::{Lexer, Token};
use crate::term::{RecordField, Term};
use crate::typing::Type;
use std::collections::VecDeque;
use std::iter::Peekable;
use std::rc::Rc;
use util::diagnostic::Diagnostic;
use util::span::*;

#[derive(Clone, Debug, Default)]
pub struct DeBruijnIndexer {
    inner: VecDeque<String>,
}

impl DeBruijnIndexer {
    pub fn push(&mut self, hint: String) -> usize {
        if self.inner.contains(&hint) {
            self.push(format!("{}'", hint))
        } else {
            let idx = self.inner.len();
            self.inner.push_front(hint);
            idx
        }
    }

    pub fn pop(&mut self) {
        self.inner.pop_front();
    }

    pub fn lookup(&self, key: &str) -> Option<usize> {
        for (idx, s) in self.inner.iter().enumerate() {
            if key == s {
                return Some(idx);
            }
        }
        None
    }
}

pub struct Parser<'s> {
    ctx: DeBruijnIndexer,
    diagnostic: Diagnostic<'s>,
    /// [`Lexer`] impls [`Iterator`] over [`TokenSpan`],
    /// so we can just directly wrap it in a [`Peekable`]
    lexer: Peekable<Lexer<'s>>,
    span: Span,
}

pub enum ParseError {
    ExpectedAtom,
    ExpectedIdent,
    ExpectedToken(Token),
}

impl<'s> Parser<'s> {
    /// Create a new [`Parser`] for the input `&str`
    pub fn new(input: &'s str) -> Parser<'s> {
        Parser {
            ctx: DeBruijnIndexer::default(),
            diagnostic: Diagnostic::new(input),
            lexer: Lexer::new(input.chars()).peekable(),
            span: Span::default(),
        }
    }

    fn consume(&mut self) -> Option<Spanned<Token>> {
        let ts = self.lexer.next()?;
        self.span = ts.span;
        Some(ts)
    }

    fn expect(&mut self, token: Token) -> Option<Spanned<Token>> {
        let spanned = self.consume()?;
        match &spanned.data {
            t if t == &token => Some(spanned),
            t => {
                self.diagnostic.push(
                    format!("Expected token {:?}, found {:?}", token, t),
                    spanned.span,
                );
                None
            }
        }
    }

    fn expect_term(&mut self) -> Option<Rc<Term>> {
        match self.term() {
            Some(term) => Some(term),
            None => {
                let sp = self.peek_span();
                self.diagnostic.push("Expected term".to_string(), sp);
                None
            }
        }
    }

    fn peek(&mut self) -> Option<Token> {
        self.lexer.peek().map(|s| s.data.clone())
    }

    fn peek_span(&mut self) -> Span {
        self.lexer.peek().map(|s| s.span).unwrap_or(self.span)
    }

    fn lambda(&mut self) -> Option<Rc<Term>> {
        let start = self.expect(Token::Lambda)?;

        // Bind variable into a new context before parsing the body
        let var = self.ident()?.data;
        self.ctx.push(var);

        let _ = self.expect(Token::Colon)?;
        let ty = self.ty()?;
        let _ = self.expect(Token::Proj)?;
        let body = self.term()?;

        // Return to previous context
        self.ctx.pop();
        Some(Term::Abs(ty, body).into())
    }

    fn let_expr(&mut self) -> Option<Rc<Term>> {
        let start = self.expect(Token::Let)?;
        let var = self.ident()?.data;
        self.ctx.push(var);
        let _ = self.expect(Token::Equals)?;
        let bind = self.expect_term()?;
        let _ = self.expect(Token::In)?;
        let body = self.expect_term()?;
        self.ctx.pop();
        Some(Term::Let(bind, body).into())
    }

    fn ty_record_field(&mut self) -> Option<(Rc<String>, Type)> {
        let label = self.ident()?;
        self.expect(Token::Colon)?;
        let data = self.ty()?;
        Some((label.data.into(), data))
    }

    fn ty_atom(&mut self) -> Option<Type> {
        match &self.peek()? {
            Token::TyBool => {
                self.consume()?;
                Some(Type::Bool)
            }
            Token::TyNat => {
                self.consume()?;
                Some(Type::Nat)
            }
            Token::TyUnit => {
                self.consume()?;
                Some(Type::Unit)
            }
            Token::LBrace => {
                self.consume()?;
                let mut fields = vec![self.ty_record_field()?];
                while let Some(Token::Comma) = self.peek() {
                    self.expect(Token::Comma)?;
                    fields.push(self.ty_record_field()?);
                }
                self.expect(Token::RBrace)?;
                Some(Type::Record(fields))
            }
            Token::Ident(s) if s.starts_with(|ch: char| ch.is_ascii_uppercase()) => {
                self.ident();
                Some(Type::Var(s.clone()))
            }
            _ => None,
        }
    }

    fn ty(&mut self) -> Option<Type> {
        let span = self.span;
        let mut lhs = match self.ty_atom() {
            Some(ty) => ty,
            None => {
                let sp = self.peek_span();
                self.diagnostic.push("Expected type".to_string(), sp);
                return None;
            }
        };

        if let Some(Token::TyArrow) = self.peek() {
            self.consume()?;
        }
        while let Some(rhs) = self.ty_atom() {
            lhs = Type::Arrow(Box::new(lhs), Box::new(rhs));
            if let Some(Token::TyArrow) = self.peek() {
                self.consume()?;
            } else {
                break;
            }
        }
        Some(lhs)
    }

    /// Parse an application of form:
    /// application = atom application' | atom
    /// application' = atom application' | empty
    fn application(&mut self) -> Option<Rc<Term>> {
        let mut lhs = self.atom()?;
        let span = self.span;
        while let Some(rhs) = self.atom() {
            lhs = Term::App(lhs, rhs).into();
        }

        if let Some(Token::Proj) = self.peek() {
            self.expect(Token::Proj)?;
            let accessor = self.ident()?;
            lhs = Term::Projection(lhs, accessor.data.into()).into();
        }
        Some(lhs)
    }

    fn ident(&mut self) -> Option<Spanned<String>> {
        let spanned = self.consume()?;
        match spanned.data {
            Token::Ident(s) => Some(Spanned::new(spanned.span, s)),
            x => {
                self.diagnostic
                    .push(format!("Expected identifier, found {:?}", x), spanned.span);
                None
            }
        }
    }

    fn record_field(&mut self) -> Option<RecordField> {
        let label = self.ident()?;
        self.expect(Token::Colon)?;
        let data = self.expect_term()?;
        Some(RecordField {
            label: label.data.into(),
            data,
        })
    }

    fn record(&mut self) -> Option<Rc<Term>> {
        let mut fields = vec![self.record_field()?];
        let span = self.span;
        while let Some(Token::Comma) = self.peek() {
            self.expect(Token::Comma)?;
            fields.push(self.record_field()?);
        }
        Some(Term::Record(fields).into())
    }

    fn if_expr(&mut self) -> Option<Rc<Term>> {
        let _ = self.expect(Token::If)?;
        let guard = self.expect_term()?;
        let _ = self.expect(Token::Then)?;
        let csq = self.expect_term()?;
        let _ = self.expect(Token::Else)?;
        let alt = self.expect_term()?;
        Some(Term::If(guard, csq, alt).into())
    }

    /// Parse an atomic term
    /// LPAREN term RPAREN | var
    fn atom(&mut self) -> Option<Rc<Term>> {
        match self.peek()? {
            Token::True => {
                self.expect(Token::True)?;
                Some(Term::True.into())
            }
            Token::False => {
                self.expect(Token::False)?;
                Some(Term::False.into())
            }
            Token::If => self.if_expr(),
            Token::Let => self.let_expr(),
            Token::Nat(i) => {
                self.consume()?;
                Some(Term::Zero.into())
            }
            Token::Succ => {
                self.expect(Token::Succ)?;
                Some(Term::Succ(self.term()?).into())
            }
            Token::Pred => {
                self.expect(Token::Pred)?;
                Some(Term::Pred(self.term()?).into())
            }
            Token::IsZero => {
                self.expect(Token::IsZero)?;
                Some(Term::IsZero(self.term()?).into())
            }
            Token::LParen => {
                self.expect(Token::LParen)?;
                let term = self.term()?;
                self.expect(Token::RParen)?;
                Some(term)
            }
            Token::LBrace => {
                self.expect(Token::LBrace)?;
                let term = self.record()?;
                self.expect(Token::RBrace)?;
                Some(term)
            }
            Token::Unit => {
                self.expect(Token::Unit)?;
                Some(Term::Unit.into())
            }
            Token::Lambda => self.lambda(),
            Token::Ident(s) => {
                let sp = self.consume()?.span;
                match self.ctx.lookup(&s) {
                    Some(idx) => Some(Term::Var(idx).into()),
                    None => {
                        self.diagnostic.push(format!("Unbound variable {}", s), sp);
                        None
                    }
                }
            }
            _ => None,
        }
    }

    fn type_decl(&mut self) -> Option<Rc<Term>> {
        self.expect(Token::TypeDecl)?;
        let name = self.ident()?;
        self.expect(Token::Equals)?;
        let ty = self.ty()?;

        Some(Term::TypeDecl(name.data.into(), ty).into())
    }

    fn term(&mut self) -> Option<Rc<Term>> {
        match self.peek()? {
            // Token::Lambda => self.lambda(),
            Token::TypeDecl => self.type_decl(),
            _ => self.application(),
        }
    }

    pub fn parse_term(&mut self) -> Option<Rc<Term>> {
        self.term()
    }

    pub fn diagnostic(self) -> Diagnostic<'s> {
        self.diagnostic
    }
}
