use super::lexer::Lexer;
use super::{Token, TokenKind};

use std::collections::VecDeque;
use std::iter::Peekable;
use util::diagnostic::Diagnostic;
use util::span::*;

use crate::terms::{Kind, Literal, Term};
use crate::types::Type;

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
    tmvar: DeBruijnIndexer,
    tyvar: DeBruijnIndexer,
    diagnostic: Diagnostic<'s>,
    lexer: Lexer<'s>,
    span: Span,
    token: Token,
}

#[derive(Clone, Debug)]
pub struct Error {
    span: Span,
    tok: Token,
    kind: ErrorKind,
}

#[derive(Clone, Debug)]
pub enum ErrorKind {
    ExpectedAtom,
    ExpectedIdent,
    ExpectedType,
    ExpectedToken(TokenKind),
    UnboundTypeVar,
    UnboundVar,
    Unknown,
    Eof,
}

pub enum Either<L, R> {
    Constr(L),
    Ident(R),
}

impl<'s> Parser<'s> {
    /// Create a new [`Parser`] for the input `&str`
    pub fn new(input: &'s str) -> Parser<'s> {
        let mut p = Parser {
            tmvar: DeBruijnIndexer::default(),
            tyvar: DeBruijnIndexer::default(),
            diagnostic: Diagnostic::new(input),
            lexer: Lexer::new(input.chars()),
            span: Span::default(),
            token: Token::dummy(),
        };
        p.bump();
        p
    }

    pub fn diagnostic(self) -> Diagnostic<'s> {
        self.diagnostic
    }
}

impl<'s> Parser<'s> {
    fn error<T>(&self, kind: ErrorKind) -> Result<T, Error> {
        Err(Error {
            span: self.token.span,
            tok: self.token.clone(),
            kind: kind,
        })
    }

    fn bump(&mut self) -> TokenKind {
        let prev = std::mem::replace(&mut self.token, self.lexer.lex());
        self.span = prev.span;
        prev.kind
    }

    fn expect(&mut self, kind: TokenKind) -> Result<(), Error> {
        if self.token.kind == kind {
            self.bump();
            Ok(())
        } else {
            self.diagnostic.push(
                format!("expected token {:?}, found {:?}", kind, self.token.kind),
                self.span,
            );
            self.error(ErrorKind::ExpectedToken(kind))
        }
    }

    fn kind(&self) -> &TokenKind {
        &self.token.kind
    }

    fn ty_atom(&mut self) -> Result<Type, Error> {
        match self.kind() {
            TokenKind::TyBool => {
                self.bump();
                Ok(Type::Bool)
            }
            TokenKind::TyNat => {
                self.bump();
                Ok(Type::Nat)
            }
            TokenKind::TyUnit => {
                self.bump();
                Ok(Type::Unit)
            }
            // TokenKind::LBrace => {
            //     let mut span = self.span;
            //     self.bump();
            //     let mut fields = vec![self.ty_record_field()?];
            //     while let Ok(TokenKind::Comma) = self.peek() {
            //         self.expect(TokenKind::Comma)?;
            //         fields.push(self.ty_record_field()?);
            //     }
            //     self.expect(TokenKind::RBrace)?;
            //     span = span + self.span;
            //     Ok(Type::Record(Record {
            //         // span,
            //         ident: String::new(),
            //         fields,
            //     }))
            // }
            TokenKind::LParen => {
                self.bump();
                let r = self.ty()?;
                self.expect(TokenKind::RParen)?;
                Ok(r)
            }
            TokenKind::Ident(s) => match self.ident()? {
                Either::Constr(ty) => match self.tyvar.lookup(&ty) {
                    Some(idx) => Ok(Type::Var(idx)),
                    None => Ok(Type::Alias(ty)),
                },
                Either::Ident(i) => self.error(ErrorKind::ExpectedType),
            },
            _ => self.error(ErrorKind::ExpectedType),
        }
    }

    fn ty(&mut self) -> Result<Type, Error> {
        let span = self.span;
        let mut lhs = self.ty_atom()?;

        if let TokenKind::TyArrow = self.kind() {
            self.bump();
            while let Ok(rhs) = self.ty_atom() {
                lhs = Type::Arrow(Box::new(lhs), Box::new(rhs));
                if let TokenKind::TyArrow = self.kind() {
                    self.bump();
                } else {
                    break;
                }
            }
        }

        Ok(lhs)
    }

    fn tyabs(&mut self, tyvar: String) -> Result<Term, Error> {
        let sp = self.span;
        let ty = Box::new(Type::Var(self.tyvar.push(tyvar)));
        // self.expect(TokenKind::Proj)?;
        let body = self.parse()?;
        self.tyvar.pop();
        Ok(Term::new(
            Kind::TyAbs(Box::new(Type::Unit), Box::new(body)),
            sp + self.span,
        ))
    }

    fn tmabs(&mut self, tmvar: String) -> Result<Term, Error> {
        let sp = self.span;
        self.tmvar.push(tmvar);
        // let tm = Box::new(Term::new(Kind::Var(self.tmvar.push(tmvar)), sp));
        self.expect(TokenKind::Colon)?;
        let ty = self.ty()?;
        self.expect(TokenKind::Proj)?;
        let body = self.parse()?;
        self.tmvar.pop();
        Ok(Term::new(
            Kind::Abs(Box::new(ty), Box::new(body)),
            sp + self.span,
        ))
    }

    fn fix(&mut self) -> Result<Term, Error> {
        let sp = self.span;
        self.expect(TokenKind::Fix)?;
        let t = self.parse()?;
        Ok(Term::new(Kind::Fix(Box::new(t)), sp + self.span))
    }

    fn letexpr(&mut self) -> Result<Term, Error> {
        let sp = self.span;
        self.expect(TokenKind::Let)?;
        let id = match self.ident()? {
            Either::Constr(ty) => {
                self.diagnostic.push(
                    format!("expected lowercase identifier in let expr, found {}", ty),
                    self.span,
                );
                return self.error(ErrorKind::ExpectedIdent);
            }
            Either::Ident(id) => id,
        };
        self.expect(TokenKind::Equals)?;
        self.tmvar.push(id);
        let t1 = self.parse()?;
        self.expect(TokenKind::In)?;
        let t2 = self.parse()?;
        Ok(Term::new(
            Kind::Let(Box::new(t1), Box::new(t2)),
            sp + self.span,
        ))
    }

    fn lambda(&mut self) -> Result<Term, Error> {
        self.expect(TokenKind::Lambda)?;
        match self.ident()? {
            Either::Constr(ty) => self.tyabs(ty),
            Either::Ident(term) => self.tmabs(term),
        }
    }

    fn paren(&mut self) -> Result<Term, Error> {
        self.expect(TokenKind::LParen)?;
        let n = self.parse()?;
        self.expect(TokenKind::RParen)?;
        Ok(n)
    }

    fn ident(&mut self) -> Result<Either<String, String>, Error> {
        match self.bump() {
            TokenKind::Ident(ident) => {
                if ident.starts_with(|ch: char| ch.is_ascii_uppercase()) {
                    Ok(Either::Constr(ident))
                } else {
                    Ok(Either::Ident(ident))
                }
            }
            tk => {
                self.diagnostic
                    .push(format!("expected identifier, found {:?}", tk), self.span);
                self.error(ErrorKind::ExpectedIdent)
            }
        }
    }

    fn literal(&mut self) -> Result<Term, Error> {
        let lit = match self.bump() {
            TokenKind::Nat(x) => Literal::Nat(x),
            TokenKind::True => Literal::Bool(true),
            TokenKind::False => Literal::Bool(false),
            _ => return self.error(ErrorKind::Unknown),
        };
        Ok(Term::new(Kind::Lit(lit), self.span))
    }

    fn atom(&mut self) -> Result<Term, Error> {
        match self.kind() {
            TokenKind::LParen => self.paren(),
            TokenKind::Lambda => self.lambda(),
            TokenKind::Let => self.letexpr(),
            TokenKind::Fix => self.fix(),
            TokenKind::Ident(s) => match self.ident()? {
                Either::Constr(ty) => {
                    self.diagnostic
                        .push(format!("unbound type {}", ty), self.span);
                    self.error(ErrorKind::UnboundTypeVar)
                }
                Either::Ident(tm) => match self.tmvar.lookup(&tm) {
                    Some(idx) => Ok(Term::new(Kind::Var(idx), self.span)),
                    None => {
                        self.diagnostic
                            .push(format!("unbound variable {}", tm), self.span);
                        self.error(ErrorKind::UnboundTypeVar)
                    }
                },
            },
            TokenKind::Nat(_) | TokenKind::True | TokenKind::False => self.literal(),
            _ => self.error(ErrorKind::ExpectedAtom),
        }
    }

    /// Parse an application of form:
    /// application = atom application' | atom
    /// application' = atom application' | empty
    fn application(&mut self) -> Result<Term, Error> {
        let mut app = self.atom()?;
        loop {
            // dbg!(self.kind());
            let sp = app.span;
            if let Ok(ty) = self.ty() {
                // Full type inference for System F is undecidable
                // Additionally, even partial type reconstruction,
                // where only type application types are erased is also
                // undecidable, see TaPL 23.6.2, Boehm 1985, 1989
                //
                // Partial erasure rules:
                // erasep(x) = x
                // erasep(λx:T. t) = λx:T. erasep(t)
                // erasep(t1 t2) = erasep(t1) erasep(t2)
                // erasep(λX. t) = λX. erasep(t)
                // erasep(t T) = erasep(t) []      <--- erasure of TyApp
                app = Term::new(Kind::TyApp(Box::new(app), Box::new(ty)), sp + self.span);
            } else if let Ok(term) = self.atom() {
                app = Term::new(Kind::App(Box::new(app), Box::new(term)), sp + self.span);
            } else {
                break;
            }

            // Explicitly end an application
            if self.kind() == &TokenKind::RParen {
                self.bump();
            }
        }
        Ok(app)
    }

    pub fn parse(&mut self) -> Result<Term, Error> {
        self.application()
    }
}
