use crate::lexer::{Lexer, Token};
use crate::term::Term;
use crate::typing::Type;
use std::collections::VecDeque;
use std::iter::Peekable;
use std::rc::Rc;
use util::diagnostic::Diagnostic;
use util::span::*;

#[derive(Clone, Debug, Default)]
pub struct NameContext {
    inner: VecDeque<String>,
}

impl NameContext {
    pub fn bind(&mut self, hint: String) -> (NameContext, usize) {
        if self.inner.contains(&hint) {
            self.bind(format!("{}'", hint))
        } else {
            let mut ctx = self.clone();
            let idx = ctx.size();
            ctx.inner.push_front(hint);
            (ctx, idx)
        }
    }

    pub fn lookup(&self, key: String) -> Option<usize> {
        for (idx, s) in self.inner.iter().enumerate() {
            if key == *s {
                return Some(idx);
            }
        }
        None
    }

    pub fn size(&self) -> usize {
        self.inner.len()
    }
}

pub struct Parser<'s> {
    ctx: NameContext,
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
            ctx: NameContext::default(),
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

    fn peek(&mut self) -> Option<Token> {
        self.lexer.peek().map(|s| s.data.clone())
    }

    fn lambda(&mut self) -> Option<Rc<Term>> {
        let start = self.expect(Token::Lambda)?.span;

        let var = self.consume()?;

        // Bind variable into a new context before parsing the body
        // of the lambda abstraction
        let prev_ctx = self.ctx.clone();
        let (ctx, var) = match var.data {
            Token::Ident(s) => {
                let (ctx, idx) = self.ctx.bind(format!("{}", s));
                (ctx, Term::Var(idx))
            }
            x => {
                self.diagnostic
                    .push(format!("Expected variable, found {:?}", x), var.span);
                return None;
            }
        };

        self.ctx = ctx;
        let _ = self.expect(Token::Colon)?;
        let ty = self.ty()?;
        let _ = self.expect(Token::Proj)?;
        let body = self.term()?;
        let end = self.span;

        // Return to previous context
        self.ctx = prev_ctx;
        Some(Term::Abs(ty, body.into()).into())
    }

    fn term(&mut self) -> Option<Rc<Term>> {
        match self.peek()? {
            Token::Lambda => self.lambda(),
            _ => self.application(),
        }
    }

    fn ty_atom(&mut self) -> Option<Type> {
        match self.peek()? {
            Token::TyBool => {
                self.consume()?;
                Some(Type::Bool)
            }
            Token::TyNat => {
                self.consume()?;
                Some(Type::Nat)
            }
            _ => None,
        }
    }

    fn ty(&mut self) -> Option<Type> {
        let span = self.span;
        let mut lhs = match self.ty_atom() {
            Some(ty) => ty,
            None => {
                self.diagnostic.push(format!("Expected type"), span);
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
        Some(lhs.into())
    }

    fn if_expr(&mut self) -> Option<Rc<Term>> {
        let _ = self.expect(Token::If)?;
        let guard = self.term()?;
        let _ = self.expect(Token::Then)?;
        let csq = self.term()?;
        let _ = self.expect(Token::Else)?;
        let alt = self.term()?;
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
            Token::Lambda => self.lambda(),
            Token::Ident(s) => {
                let sp = self.consume()?.span;
                match self.ctx.lookup(format!("{}", s)) {
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

    pub fn parse_term(&mut self) -> Option<Rc<Term>> {
        self.term()
    }

    pub fn diagnostic(self) -> Diagnostic<'s> {
        self.diagnostic
    }
}
