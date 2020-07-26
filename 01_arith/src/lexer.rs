use util::span::{Location, Span, Spanned};

use std::char;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
pub enum Token {
    Int(u32),
    Succ,
    Pred,
    If,
    Then,
    Else,
    True,
    False,
    IsZero,

    Semicolon,
    LParen,
    RParen,

    Invalid,
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

    /// Eat whitespace
    fn consume_delimiter(&mut self) {
        let _ = self.consume_while(char::is_whitespace);
    }

    fn number(&mut self) -> Option<TokenSpan> {
        let Spanned { data, span } = self.consume_while(char::is_numeric);
        let kind = Token::Int(data.parse::<u32>().expect("only numeric chars"));
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
            "zero" => Token::Int(0),
            _ => Token::Invalid,
        };
        Some(TokenSpan { kind, span })
    }

    fn eat(&mut self, ch: char, token: Token) -> Option<TokenSpan> {
        let loc = self.current;
        let n = self.consume()?;
        let kind = if n == ch { token } else { Token::Invalid };
        Some(TokenSpan {
            span: Span::new(loc, self.current),
            kind,
        })
    }

    fn lex(&mut self) -> Option<TokenSpan> {
        self.consume_delimiter();
        match self.peek()? {
            x if x.is_ascii_alphabetic() => self.keyword(),
            x if x.is_numeric() => self.number(),
            '(' => self.eat('(', Token::LParen),
            ')' => self.eat(')', Token::RParen),
            ';' => self.eat(';', Token::Semicolon),
            _ => self.eat(' ', Token::Invalid),
        }
    }
}

impl<'s> Iterator for Lexer<'s> {
    type Item = TokenSpan;
    fn next(&mut self) -> Option<Self::Item> {
        self.lex()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use Token::*;
    #[test]
    fn valid() {
        let input = "succ(succ(succ(0)))";
        let expected = vec![Succ, LParen, Succ, LParen, Succ, LParen, Int(0), RParen, RParen, RParen];
        let output = Lexer::new(input.chars())
            .into_iter()
            .map(|t| t.kind)
            .collect::<Vec<Token>>();
        assert_eq!(expected, output);
    }

    #[test]
    fn invalid() {
        let input = "succ(succ(succ(xyz)))";
        let expected = vec![
            Succ, LParen, Succ, LParen, Succ, LParen, Invalid, RParen, RParen, RParen,
        ];
        let output = Lexer::new(input.chars())
            .into_iter()
            .map(|t| t.kind)
            .collect::<Vec<Token>>();
        assert_eq!(expected, output);
    }
}
