use super::{Token, TokenKind};
use std::char;
use std::iter::Peekable;
use std::str::Chars;
use util::span::{Location, Span};

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

    /// Peek at the next [`char`] in the input stream
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

    /// Lex a natural number
    fn number(&mut self) -> Token {
        // Since we peeked at least one numeric char, we should always
        // have a string containing at least 1 single digit, as such
        // it is safe to call unwrap() on str::parse<u32>
        let (data, span) = self.consume_while(char::is_numeric);
        let n = data.parse::<u32>().unwrap();
        Token::new(TokenKind::Nat(n), span)
    }

    /// Lex a reserved keyword or an identifier
    fn keyword(&mut self) -> Token {
        let (data, span) = self.consume_while(|ch| ch.is_ascii_alphanumeric());
        let kind = match data.as_ref() {
            "if" => TokenKind::If,
            "then" => TokenKind::Then,
            "else" => TokenKind::Else,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "succ" => TokenKind::Succ,
            "pred" => TokenKind::Pred,
            "iszero" => TokenKind::IsZero,
            "zero" => TokenKind::Nat(0),
            "Bool" => TokenKind::TyBool,
            "Nat" => TokenKind::TyNat,
            "Unit" => TokenKind::TyUnit,
            "unit" => TokenKind::Unit,
            "let" => TokenKind::Let,
            "in" => TokenKind::In,
            "fix" => TokenKind::Fix,
            "case" => TokenKind::Case,
            "of" => TokenKind::Of,
            "fold" => TokenKind::Fold,
            "unfold" => TokenKind::Unfold,
            "rec" => TokenKind::Rec,
            "lambda" => TokenKind::Lambda,
            "forall" => TokenKind::Forall,
            "exists" => TokenKind::Exists,
            "pack" => TokenKind::Pack,
            "unpack" => TokenKind::Unpack,
            "as" => TokenKind::As,

            _ => {
                if data.starts_with(|ch: char| ch.is_ascii_uppercase()) {
                    TokenKind::Uppercase(data)
                } else {
                    TokenKind::Lowercase(data)
                }
            }
        };
        Token::new(kind, span)
    }

    /// Consume the next input character, expecting to match `ch`.
    /// Return a [`TokenKind::Invalid`] if the next character does not match,
    /// or the argument `kind` if it does
    fn eat(&mut self, ch: char, kind: TokenKind) -> Token {
        let loc = self.current;
        // Lexer::eat() should only be called internally after calling peek()
        // so we know that it's safe to unwrap the result of Lexer::consume()
        let n = self.consume().unwrap();
        let kind = if n == ch { kind } else { TokenKind::Invalid(n) };
        Token::new(kind, Span::new(loc, self.current))
    }

    /// Return the next lexeme in the input as a [`Token`]
    pub fn lex(&mut self) -> Token {
        self.consume_delimiter();
        let next = match self.peek() {
            Some(ch) => ch,
            None => return Token::new(TokenKind::Eof, Span::new(self.current, self.current)),
        };
        match next {
            x if x.is_ascii_alphabetic() => self.keyword(),
            x if x.is_numeric() => self.number(),
            '(' => self.eat('(', TokenKind::LParen),
            ')' => self.eat(')', TokenKind::RParen),
            ';' => self.eat(';', TokenKind::Semicolon),
            ':' => self.eat(':', TokenKind::Colon),
            ',' => self.eat(',', TokenKind::Comma),
            '{' => self.eat('{', TokenKind::LBrace),
            '}' => self.eat('}', TokenKind::RBrace),
            '[' => self.eat('[', TokenKind::LSquare),
            ']' => self.eat(']', TokenKind::RSquare),
            '\\' => self.eat('\\', TokenKind::Lambda),
            'λ' => self.eat('λ', TokenKind::Lambda),
            '∀' => self.eat('∀', TokenKind::Forall),
            '∃' => self.eat('∃', TokenKind::Exists),
            '.' => self.eat('.', TokenKind::Proj),
            '=' => self.eat('=', TokenKind::Equals),
            '|' => self.eat('|', TokenKind::Bar),
            '_' => self.eat('_', TokenKind::Wildcard),
            '>' => self.eat('>', TokenKind::Gt),
            '-' => {
                self.consume();
                self.eat('>', TokenKind::TyArrow)
            }
            ch => self.eat(' ', TokenKind::Invalid(ch)),
        }
    }
}

impl<'s> Iterator for Lexer<'s> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        match self.lex() {
            Token {
                kind: TokenKind::Eof, ..
            } => None,
            tok => Some(tok),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use TokenKind::*;
    #[test]
    fn nested() {
        let input = "succ(succ(succ(0)))";
        let expected = vec![Succ, LParen, Succ, LParen, Succ, LParen, Nat(0), RParen, RParen, RParen];
        let output = Lexer::new(input.chars())
            .into_iter()
            .map(|t| t.kind)
            .collect::<Vec<TokenKind>>();
        assert_eq!(expected, output);
    }

    #[test]
    fn case() {
        let input = "case x of | A _ => true | B x => (\\y: Nat. x)";
        let expected = vec![
            Case,
            Lowercase("x".into()),
            Of,
            Bar,
            Uppercase("A".into()),
            Wildcard,
            Equals,
            Gt,
            True,
            Bar,
            Uppercase("B".into()),
            Lowercase("x".into()),
            Equals,
            Gt,
            LParen,
            Lambda,
            Lowercase("y".into()),
            Colon,
            TyNat,
            Proj,
            Lowercase("x".into()),
            RParen,
        ];
        let output = Lexer::new(input.chars())
            .into_iter()
            .map(|t| t.kind)
            .collect::<Vec<TokenKind>>();
        assert_eq!(expected, output);
    }
}
