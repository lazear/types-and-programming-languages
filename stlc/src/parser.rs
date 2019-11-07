use crate::lexer::{Lexer, Token, TokenKind};
use crate::term::{Field, Term};
use crate::typing::{Record, RecordField, Type};
use std::collections::VecDeque;
use std::iter::Peekable;
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

impl<'s> Parser<'s> {
    /// Create a new [`Parser`] for the input `&str`
    pub fn new(input: &'s str) -> Parser<'s> {
        Parser {
            ctx: DeBruijnIndexer::default(),
            diagnostic: Diagnostic::new(input),
            lexer: Lexer::new(input.chars()).peekable(),
            span: Span::dummy(),
        }
    }

    fn consume(&mut self) -> Option<Token> {
        let ts = self.lexer.next()?;
        self.span = ts.span;
        Some(ts)
    }

    fn expect(&mut self, kind: TokenKind) -> Option<Token> {
        let tk = self.consume()?;
        match &tk.kind {
            t if t == &kind => Some(tk),
            _ => {
                self.diagnostic.push(
                    format!("Expected token {:?}, found {:?}", kind, tk.kind),
                    tk.span,
                );
                None
            }
        }
    }

    fn expect_term(&mut self) -> Option<Box<Term>> {
        match self.term() {
            Some(term) => Some(term),
            None => {
                let sp = self.peek_span();
                self.diagnostic.push("Expected term".to_string(), sp);
                None
            }
        }
    }

    fn peek(&mut self) -> Option<TokenKind> {
        self.lexer.peek().map(|tk| tk.kind.clone())
    }

    fn peek_span(&mut self) -> Span {
        self.lexer.peek().map(|s| s.span).unwrap_or(self.span)
    }

    fn lambda(&mut self) -> Option<Box<Term>> {
        let start = self.expect(TokenKind::Lambda)?;

        // Bind variable into a new context before parsing the body
        let var = self.ident()?;
        self.ctx.push(var);

        let _ = self.expect(TokenKind::Colon)?;
        let ty = self.ty()?;
        let _ = self.expect(TokenKind::Proj)?;
        let body = self.term()?;

        // Return to previous context
        self.ctx.pop();
        Some(Term::Abs(ty, body).into())
    }

    fn let_expr(&mut self) -> Option<Box<Term>> {
        let start = self.expect(TokenKind::Let)?;
        let var = self.ident()?;
        self.ctx.push(var);
        let _ = self.expect(TokenKind::Equals)?;
        let bind = self.expect_term()?;
        let _ = self.expect(TokenKind::In)?;
        let body = self.expect_term()?;
        self.ctx.pop();
        Some(Term::Let(bind, body).into())
    }

    fn ty_record_field(&mut self) -> Option<RecordField> {
        let ident = self.ident()?;
        self.expect(TokenKind::Colon)?;
        let ty = self.ty()?;
        Some(RecordField {
            ident,
            ty: Box::new(ty),
        })
    }

    fn ty_atom(&mut self) -> Option<Type> {
        match &self.peek()? {
            TokenKind::TyBool => {
                self.consume()?;
                Some(Type::Bool)
            }
            TokenKind::TyNat => {
                self.consume()?;
                Some(Type::Nat)
            }
            TokenKind::TyUnit => {
                self.consume()?;
                Some(Type::Unit)
            }
            TokenKind::LBrace => {
                self.consume()?;
                let mut fields = vec![self.ty_record_field()?];
                while let Some(TokenKind::Comma) = self.peek() {
                    self.expect(TokenKind::Comma)?;
                    fields.push(self.ty_record_field()?);
                }
                self.expect(TokenKind::RBrace)?;
                Some(Type::Record(Record {
                    // span,
                    ident: String::new(),
                    fields,
                }))
            }
            TokenKind::LParen => {
                self.consume()?;
                let r = self.ty()?;
                self.expect(TokenKind::RParen)?;
                Some(r)
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

        if let Some(TokenKind::TyArrow) = self.peek() {
            self.consume()?;
        }
        while let Some(rhs) = self.ty_atom() {
            lhs = Type::Arrow(Box::new(lhs), Box::new(rhs));
            if let Some(TokenKind::TyArrow) = self.peek() {
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
    fn application(&mut self) -> Option<Box<Term>> {
        let mut lhs = self.atom()?;
        let span = self.span;
        while let Some(rhs) = self.atom() {
            lhs = Term::App(lhs, rhs).into();
        }

        if let Some(TokenKind::Proj) = self.peek() {
            self.expect(TokenKind::Proj)?;
            let accessor = self.ident()?;
            lhs = Term::Projection(lhs, accessor.into()).into();
        }
        Some(lhs)
    }

    fn ident(&mut self) -> Option<String> {
        let Token { kind, span } = self.consume()?;
        match kind {
            TokenKind::Ident(s) => Some(s),
            _ => {
                self.diagnostic
                    .push(format!("Expected identifier, found {:?}", kind), span);
                None
            }
        }
    }

    fn record_field(&mut self) -> Option<Field> {
        let span = self.span;
        let ident = self.ident()?;
        self.expect(TokenKind::Colon)?;
        let term = self.expect_term()?;

        Some(Field {
            span: span + self.span,
            ident,
            term,
        })
    }

    fn record(&mut self) -> Option<Box<Term>> {
        let mut fields = vec![self.record_field()?];
        let span = self.span;
        while let Some(TokenKind::Comma) = self.peek() {
            self.expect(TokenKind::Comma)?;
            fields.push(self.record_field()?);
        }
        Some(Term::Record(fields).into())
    }

    fn if_expr(&mut self) -> Option<Box<Term>> {
        let _ = self.expect(TokenKind::If)?;
        let guard = self.expect_term()?;
        let _ = self.expect(TokenKind::Then)?;
        let csq = self.expect_term()?;
        let _ = self.expect(TokenKind::Else)?;
        let alt = self.expect_term()?;
        Some(Term::If(guard, csq, alt).into())
    }

    /// Parse an atomic term
    /// LPAREN term RPAREN | var
    fn atom(&mut self) -> Option<Box<Term>> {
        match self.peek()? {
            TokenKind::True => {
                self.expect(TokenKind::True)?;
                Some(Term::True.into())
            }
            TokenKind::False => {
                self.expect(TokenKind::False)?;
                Some(Term::False.into())
            }
            TokenKind::If => self.if_expr(),
            TokenKind::Let => self.let_expr(),
            TokenKind::Nat(i) => {
                self.consume()?;
                Some(Term::Zero.into())
            }
            TokenKind::Succ => {
                self.expect(TokenKind::Succ)?;
                Some(Term::Succ(self.term()?).into())
            }
            TokenKind::Pred => {
                self.expect(TokenKind::Pred)?;
                Some(Term::Pred(self.term()?).into())
            }
            TokenKind::IsZero => {
                self.expect(TokenKind::IsZero)?;
                Some(Term::IsZero(self.term()?).into())
            }
            TokenKind::LParen => {
                self.expect(TokenKind::LParen)?;
                let term = self.term()?;
                self.expect(TokenKind::RParen)?;
                Some(term)
            }
            TokenKind::LBrace => {
                self.expect(TokenKind::LBrace)?;
                let term = self.record()?;
                self.expect(TokenKind::RBrace)?;
                Some(term)
            }
            TokenKind::Unit => {
                self.expect(TokenKind::Unit)?;
                Some(Term::Unit.into())
            }
            TokenKind::Lambda => self.lambda(),
            TokenKind::Ident(s) => {
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

    fn term(&mut self) -> Option<Box<Term>> {
        match self.peek()? {
            // TokenKind::Lambda => self.lambda(),
            _ => self.application(),
        }
    }

    pub fn parse_term(&mut self) -> Option<Box<Term>> {
        self.term()
    }

    pub fn diagnostic(self) -> Diagnostic<'s> {
        self.diagnostic
    }
}
