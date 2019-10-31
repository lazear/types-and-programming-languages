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
    ExpectedToken(TokenKind),
    Unknown,
}

pub enum Either<L, R> {
    Left(L),
    Right(R),
}

impl<'s> Parser<'s> {
    /// Create a new [`Parser`] for the input `&str`
    pub fn new(input: &'s str) -> Parser<'s> {
        Parser {
            tmvar: DeBruijnIndexer::default(),
            tyvar: DeBruijnIndexer::default(),
            diagnostic: Diagnostic::new(input),
            lexer: Lexer::new(input.chars()),
            span: Span::dummy(),
            token: Token::dummy(),
        }
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
        self.span = self.token.span;
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

    fn lambda(&mut self) -> Result<Term, Error> {
        match self.ident()? {
            Either::Left(ty) => {}
            Either::Right(term) => {}
        }
        self.error(ErrorKind::ExpectedToken(TokenKind::Lambda))
    }

    fn paren(&mut self) -> Result<Term, Error> {
        self.expect(TokenKind::LParen)?;
        let n = self.try_next()?;
        self.expect(TokenKind::RParen)?;
        Ok(n)
    }

    fn ident(&mut self) -> Result<Either<String, String>, Error> {
        match self.bump() {
            TokenKind::Ident(ident) => {
                if ident.starts_with(|ch: char| ch.is_ascii_uppercase()) {
                    Ok(Either::Left(ident))
                } else {
                    Ok(Either::Right(ident))
                }
            }
            tk => {
                self.diagnostic
                    .push(format!("expected identifier, found {:?}", tk), self.span);
                self.error(ErrorKind::ExpectedIdent)
            }
        }
    }

    // fn ty_var(&mut self) -> Result<Type, Error> {

    // }

    // fn tm_var(&mut self) -> Result<Term, Error> {

    // }

    pub fn try_next(&mut self) -> Result<Term, Error> {
        match self.kind() {
            TokenKind::Lambda => self.lambda(),
            TokenKind::LParen => self.paren(),
            _ => self.error(ErrorKind::Unknown),
        }
    }
}
