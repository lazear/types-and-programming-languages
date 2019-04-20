use crate::lexer::{Lexer, Token, TokenSpan};
use crate::span::Span;
use std::iter::Peekable;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Term {
    TmTrue,
    TmFalse,
    TmIf(Box<Term>, Box<Term>, Box<Term>),
    TmZero,
    TmSucc(Box<Term>),
    TmPred(Box<Term>),
    TmIsZero(Box<Term>),
}

pub struct Parser<'s> {
    src: &'s str,
    diagnostic: Diagnostic<'s>,
    lexer: Peekable<Lexer<'s>>,
    span: Span,
}

pub struct Diagnostic<'s> {
    src: &'s str,
    messages: Vec<String>,
    spans: Vec<Span>,
}

impl Diagnostic<'_> {
    pub fn new(src: &str) -> Diagnostic<'_> {
        Diagnostic {
            src,
            messages: Vec::new(),
            spans: Vec::new(),
        }
    }

    pub fn push<S: Into<String>>(&mut self, msg: S, span: Span) {
        self.messages.push(msg.into());
        self.spans.push(span);
    }

    pub fn error_count(&self) -> usize {
        self.messages.len()
    }

    pub fn emit(mut self) -> String {
        let mut s = String::new();
        assert_eq!(self.messages.len(), self.spans.len());

        let lines = self.src.lines().collect::<Vec<&str>>();
        for i in 0 .. self.messages.len() {
            let msg = &self.messages[i];
            let span = self.spans[i];

            s.push_str(&format!("Error occuring at line {}, col: {}: {}\n{}\n{}^~~~\n", span.start.line, span.start.col, msg, 
                &lines[span.start.line as usize],
                (0..span.start.col).map(|_| ' ').collect::<String>(),
            ));
        }
        self.messages.clear();
        self.spans.clear();
        s
    }
}

impl<'s> Parser<'s> {
    pub fn new(input: &'s str) -> Parser<'s> {
        Parser {
            src: input,
            diagnostic: Diagnostic::new(input),
            lexer: Lexer::new(input.chars()).peekable(),
            span: Span::default(),
        }
    }

    fn peek(&mut self) -> Option<Token> {
        let ts = self.lexer.peek()?;
        self.span = ts.span;
        Some(ts.kind)
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

    fn parse_paren(&mut self) -> Option<Term> {
        let e = self.parse_term();
        self.expect(Token::RParen);
        e
    }

    fn parse_if(&mut self) -> Option<Term> {
        let cond = self.parse_term()?;
        let _ = self.expect(Token::Then)?;
        let csq = self.parse_term()?;
        let _ = self.expect(Token::Else)?;
        let alt = self.parse_term()?;
        Some(Term::TmIf(Box::new(cond), Box::new(csq), Box::new(alt)))
    }

    pub fn parse_term(&mut self) -> Option<Term> {
        let kind = match self.consume()? {
            Token::False => Term::TmFalse,
            Token::True => Term::TmTrue,
            Token::Succ => Term::TmSucc(Box::new(self.parse_term()?)),
            Token::Pred => Term::TmPred(Box::new(self.parse_term()?)),
            Token::IsZero => Term::TmIsZero(Box::new(self.parse_term()?)),
            Token::If => return self.parse_if(),
            Token::LParen => return self.parse_paren(),
            Token::Semicolon => return self.parse_term(),
            Token::Int(x) => baptize(x),
            Token::Then | Token::Else | Token::RParen => {
                //panic!("out of place token! @ {:?}", self.span)
                self.diagnostic.push("Out of place token", self.span);
                return self.parse_term()
            }
            Token::Invalid => {
                self.diagnostic.push("Invalid token", self.span);
                return self.parse_term()
            }
        };
        Some(kind)
    }

    pub fn diagnostic(self) -> Diagnostic<'s> {
        self.diagnostic
    }
}

/// Convert from natural number to church encoding
fn baptize(int: u32) -> Term {
    let mut num = Term::TmZero;
    for _ in 0..int {
        num = Term::TmSucc(Box::new(num));
    }
    num
}

/// Convert from church encoding to natural number
fn sin(term: Term) -> Option<u32> {
    let mut n = 0;
    let mut ptr = term;
    while let Term::TmSucc(t) = ptr {
        n += 1;
        ptr = *t;
        if let Term::TmZero = ptr {
            return Some(n);
        }
    }
    None
}

impl Drop for Diagnostic<'_> {
    fn drop(&mut self) {
        if self.error_count() != 0 {
            panic!("parser::Diagnostic dropped without handling {} errors!", self.error_count());
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use Term::*;

    macro_rules! succ {
        ($e:expr) => {
            TmSucc(Box::new($e))
        };
    }

    #[test]
    fn baptism_by_fire() {
        let s = succ!(succ!(succ!(succ!(TmZero))));
        assert_eq!(baptize(4), s);
        assert_eq!(sin(baptize(4)), Some(4));
    }

}
