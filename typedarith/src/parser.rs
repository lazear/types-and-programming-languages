use crate::ast::{RcTerm, Term};
use crate::lexer::{Lexer, Token};
use std::iter::Peekable;
use util::diagnostic::Diagnostic;
use util::span::Span;

pub struct Parser<'s> {
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
            diagnostic: Diagnostic::new(input),
            lexer: Lexer::new(input.chars()).peekable(),
            span: Span::default(),
        }
    }

    fn consume(&mut self) -> Option<Token> {
        let ts = self.lexer.next()?;
        self.span = ts.span;
        Some(ts.kind)
    }

    fn expect(&mut self, token: Token) -> Option<Token> {
        match self.consume()? {
            t if t == token => Some(t),
            _ => None,
        }
    }

    fn parse_paren(&mut self) -> Option<RcTerm> {
        let e = self.parse_term();
        self.expect(Token::RParen);
        e
    }

    fn parse_if(&mut self) -> Option<RcTerm> {
        let cond = self.parse_term()?;
        let _ = self.expect(Token::Then)?;
        let csq = self.parse_term()?;
        let _ = self.expect(Token::Else)?;
        let alt = self.parse_term()?;
        Some(Term::TmIf(cond, csq, alt).into())
    }

    pub fn parse_term(&mut self) -> Option<RcTerm> {
        let kind = match self.consume()? {
            Token::False => Term::TmFalse,
            Token::True => Term::TmTrue,
            Token::Succ => Term::TmSucc(self.parse_term()?),
            Token::Pred => Term::TmPred(self.parse_term()?),
            Token::IsZero => Term::TmIsZero(self.parse_term()?),
            Token::If => return self.parse_if(),
            Token::LParen => return self.parse_paren(),
            Token::Semicolon => return self.parse_term(),
            Token::Int(x) => baptize(x),
            Token::Then | Token::Else | Token::RParen => {
                self.diagnostic.push("Out of place token", self.span);
                return self.parse_term();
            }
            Token::Invalid => {
                self.diagnostic.push("Invalid token", self.span);
                return self.parse_term();
            }
        };
        Some(kind.into())
    }

    pub fn diagnostic(self) -> Diagnostic<'s> {
        self.diagnostic
    }
}

/// Convert from natural number to church encoding
fn baptize(int: u32) -> Term {
    let mut num = Term::TmZero;
    for _ in 0..int {
        num = Term::TmSucc(num.into());
    }
    num
}
