use super::tokens::*;
use std::char;
use std::iter::Peekable;
use std::str::Chars;
use util::span::{Location, Span, Spanned};

#[derive(Clone, Debug)]
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

    /// Peek at the next [`char`] in the input stream
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

    /// Consume characters from the input stream while pred(peek()) is true,
    /// collecting the characters into a string.
    fn consume_while<F: Fn(char) -> bool>(&mut self, pred: F) -> (String, Span) {
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
        (s, Span::new(start, self.current))
    }

    /// Eat whitespace
    fn consume_delimiter(&mut self) {
        let _ = self.consume_while(char::is_whitespace);
    }

    fn id(&mut self) -> (String, Span) {
        let (mut s, mut span) = self.consume_while(|ch| ch.is_ascii_alphanumeric());
        while let Some(ch) = self.peek() {
            if ch != '.' {
                break;
            }
            self.consume();
            s.push('.');
            let (s2, sp2) = self.consume_while(|ch| ch.is_ascii_alphanumeric());
            s += &s2;
            span += sp2;
        }
        (s, span)
    }

    fn valid_id_char(c: char) -> bool {
        match c {
            '.' => false,
            x if x.is_alphanumeric() => true,
            '`' | '~' | '!' | '$' | '%' | '^' | '&' | '-' | '_' | '+' | '?' | '<' | '>' => true,
            _ => false,
        }
    }

    /// Lex a reserved keyword or identifier
    fn keyword(&mut self) -> Spanned<Token> {
        let (word, sp) = self.consume_while(Self::valid_id_char);
        let kind = match word.as_ref() {
            "fun" => Token::Function,
            "fn" => Token::Lambda,
            "val" => Token::Val,
            "let" => Token::Let,
            "in" => Token::In,
            "case" => Token::Case,
            "of" => Token::Of,
            "end" => Token::End,
            "as" => Token::As,
            "if" => Token::If,
            "then" => Token::Then,
            "else" => Token::Else,
            "fix" => Token::Fix,
            "rec" => Token::Rec,
            "exists" => Token::Exists,
            "forall" => Token::Forall,
            "type" => Token::Type,
            "datatype" => Token::Datatype,
            "and" => Token::And,
            "int" => Token::TyInt,
            "unit" => Token::TyUnit,
            "bool" => Token::TyBool,
            s if s.starts_with(char::is_uppercase) => Token::UpperId(word),
            _ => Token::LowerId(word),
        };
        Spanned::new(sp, kind)
    }

    /// Consume the next input character, expecting to match `ch`.
    /// Return a [`TokenKind::Invalid`] if the next character does not match,
    /// or the argument `kind` if it does
    fn eat(&mut self, ch: char, kind: Token) -> Spanned<Token> {
        let loc = self.current;
        // Lexer::eat() should only be called internally after calling peek()
        // so we know that it's safe to unwrap the result of Lexer::consume()
        let n = self.consume().unwrap();
        let kind = if n == ch { kind } else { Token::Invalid(n) };
        Spanned::new(Span::new(loc, self.current), kind)
    }

    /// Lex a natural number
    fn number(&mut self) -> Spanned<Token> {
        // Since we peeked at least one numeric char, we should always
        // have a string containing at least 1 single digit, as such
        // it is safe to call unwrap() on str::parse<u32>
        let (data, span) = self.consume_while(char::is_numeric);
        let n = data.parse::<usize>().unwrap();
        Spanned::new(span, Token::Int(n))
    }

    pub fn lex(&mut self) -> Spanned<Token> {
        self.consume_delimiter();
        let next = match self.peek() {
            Some(ch) => ch,
            None => return Spanned::new(Span::new(self.current, self.current), Token::EOF),
        };

        macro_rules! disamb {
            ($ch:expr, $($ch2:expr, $p:expr),+) => {{
                self.consume();
                match self.peek() {
                    Some(ch) => match ch {
                        $($ch2 => self.eat($ch2, $p)),+,
                        _ => Spanned::new(Span::new(self.current, self.current), Token::Invalid($ch))
                    },
                    None => Spanned::new(Span::new(self.current, self.current), Token::Invalid($ch)),
                }
            }};
            ($ch:expr, $p1:expr, $($ch2:expr, $p:expr),+) => {{
                let fail = self.eat($ch, $p1);
                match self.peek() {
                    Some(ch) => match ch {
                        $($ch2 => self.eat($ch2, $p)),+,
                        _ => fail,
                    },
                    None => fail,
                }
            }};
        }

        match next {
            '.' => self.eat('.', Token::Dot),
            ':' => disamb!(':', Token::Colon, '>', Token::Opaque),
            ';' => self.eat(';', Token::Semicolon),
            ',' => self.eat(',', Token::Comma),
            '\'' => self.eat('\'', Token::Apostrophe),
            '|' => self.eat('|', Token::Bar),
            '-' => disamb!('-', '>', Token::SingleArrow),
            '=' => disamb!('=', Token::Equals, '>', Token::DoubleArrow),
            '_' => self.eat('_', Token::Wildcard),
            '*' => self.eat('*', Token::Asterisk),
            '(' => disamb!('(', Token::LParen, ')', Token::Unit),
            ')' => self.eat(')', Token::RParen),
            '{' => self.eat('{', Token::LBrace),
            '}' => self.eat('}', Token::RBrace),
            '@' => self.eat('@', Token::TypeAppSigil),
            '\\' => self.eat('\\', Token::Lambda),
            'λ' => self.eat('λ', Token::Lambda),
            '∀' => self.eat('∀', Token::Forall),
            '∃' => self.eat('∃', Token::Exists),
            x if x.is_ascii_alphabetic() => self.keyword(),
            x if x.is_numeric() => self.number(),
            _ => self.eat(' ', Token::EOF),
            // _ => Spanned::new(Token::Invalid(next), Span::new(self.current, self.current)),
        }
    }
}

impl<'s> Iterator for Lexer<'s> {
    type Item = Spanned<Token>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.lex() {
            Spanned {
                data: Token::EOF, ..
            } => None,
            tok => Some(tok),
        }
    }
}
