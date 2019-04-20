use crate::span::{Location, Span, Spanned};

use std::char;
use std::fmt;
use std::iter::Peekable;
use std::rc::Rc;
use std::str::Chars;

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
pub enum Token {
    Int(u32),
    Succ,
    Pred,
    If,
    Then,
    Else,
    Semicolon,
    True,
    False,
    IsZero,
    LParen,
    RParen,

    Invalid,
    EOF,
}

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
pub struct TokenSpan {
    pub kind: Token,
    pub span: Span,
}

impl std::ops::Deref for TokenSpan {
    type Target = Token;
    fn deref(&self) -> &Self::Target {
        &self.kind
    }
}

pub struct Lexer<'s> {
    input: Peekable<Chars<'s>>,
    current: Location,
}

impl<'s> Lexer<'s> {
    pub fn new(input: Chars<'s>) -> Lexer<'s> {
        Lexer {
            input: input.peekable(),
            current: Location {
                line: 0,
                col: 0,
                abs: 0,
            },
        }
    }

    pub fn peek(&mut self) -> Option<char> {
        self.input.peek().cloned()
    }

    /// Consume the next [`char`] and advance internal source position
    fn consume(&mut self) -> Option<char> {
        match self.input.next() {
            Some('\n') => {
                self.current.line += 1;
                self.current.col = 0;
                self.current.abs += 1;
                Some('\n')
            }
            Some(ch) => {
                self.current.col += 1;
                self.current.abs += 1;
                Some(ch)
            }
            None => None,
        }
    }

    fn consume_while<F: Fn(char) -> bool>(&mut self, pred: F) -> Spanned<String> {
        let mut s = String::new();
        let start = self.current;
        while let Some(n) = self.peek() {
            if pred(n) {
                match self.consume() {
                    Some(ch) => s.push(ch),
                    None => break,
                }
            } else {
                break;
            }
        }
        Spanned::new(Span::new(start, self.current), s)
    }

    fn consume_delimiter(&mut self) {
        let _ = self.consume_while(|ch| ch.is_whitespace());
    }

    fn number(&mut self) -> Option<TokenSpan> {
        let Spanned { data, span } = self.consume_while(char::is_numeric);
        let kind = Token::Int(
            data.parse::<u32>().expect("only numeric chars")
        );
        Some(TokenSpan { kind, span })
    }

    fn keyword(&mut self) -> Option<TokenSpan> {
        let Spanned { data, span } = self.consume_while(|ch| ch.is_ascii_alphanumeric());
        let kind = match data.as_ref() {
            "if" => Token::If,
            "then" => Token::Then,
            "else" => Token::Else,
            "true" => Token::True,
            "false" => Token::False,
            "succ" => Token::Succ,
            "pred" => Token::Pred,
            "iszero" => Token::IsZero,
            _ => Token::Invalid
        };
        Some(TokenSpan { kind, span })
    }


}