use crate::lexer::{Lexer, Token};
use std::iter::Peekable;
use util::diagnostic::Diagnostic;
use util::span::Span;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Term {
    TmVar(char),
    TmAbs(Box<Term>),
    TmApp(Box<Term>, Box<Term>),
}

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

    /// Return the last parsing error as a formatted message, if it exists
    pub fn last_error(&mut self) -> Option<String> {
        self.diagnostic.pop()
    }

    fn consume(&mut self) -> Option<Token> {
        let ts = self.lexer.next()?;
        self.span = ts.span;
        Some(ts.kind)
    }

    fn expect(&mut self, token: Token) -> Option<Token> {
        match self.consume()? {
            t if t == token => Some(t),
            t => {
                self.diagnostic.push(
                    format!("Expected token {:?}, found {:?}", token, t),
                    self.span,
                );
                None
            }
        }
    }

    fn parse_paren(&mut self) -> Option<Term> {
        let e = self.parse_term();
        self.expect(Token::RParen);
        e
    }

    fn parse_lambda(&mut self) -> Option<Term> {
        let var = self.parse_term()?;
        let _ = self.expect(Token::Dot)?;
        let body = self.parse_term()?;
        Some(Term::TmAbs(Box::new(body)))
    }

    pub fn parse_term(&mut self) -> Option<Term> {
        match self.consume()? {
            Token::LParen => Some(Term::TmApp(
                Box::new(self.parse_paren()?),
                Box::new(self.parse_term()?),
            )),
            Token::Lambda => self.parse_lambda(),
            Token::Var(ch) => Some(Term::TmVar(ch)),
            Token::Invalid | Token::RParen | Token::Dot => {
                self.diagnostic.push("Invalid token", self.span);
                self.parse_term()
            }
        }
    }

    pub fn diagnostic(self) -> Diagnostic<'s> {
        self.diagnostic
    }
}
