use util::span::{Location, Span, Spanned};

use std::char;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
pub enum Token {
    Var(char),
    LParen,
    RParen,
    Lambda,
    Dot,
    Invalid,
}

#[derive(Clone)]
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

    fn peek(&mut self) -> Option<char> {
        self.input.peek().copied()
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

    /// Eat whitespace
    fn consume_delimiter(&mut self) {
        let _ = self.consume_while(char::is_whitespace);
    }

    fn eat(&mut self, ch: char, token: Token) -> Option<Spanned<Token>> {
        let loc = self.current;
        let n = self.consume()?;
        let kind = if n == ch { token } else { Token::Invalid };
        Some(Spanned::new(Span::new(loc, self.current), kind))
    }

    fn lex(&mut self) -> Option<Spanned<Token>> {
        self.consume_delimiter();
        match self.peek()? {
            '(' => self.eat('(', Token::LParen),
            ')' => self.eat(')', Token::RParen),
            'λ' => self.eat('λ', Token::Lambda),
            '.' => self.eat('.', Token::Dot),
            ch => self.eat(ch, Token::Var(ch)),
        }
    }
}

impl<'s> Iterator for Lexer<'s> {
    type Item = Spanned<Token>;
    fn next(&mut self) -> Option<Self::Item> {
        self.lex()
    }
}
