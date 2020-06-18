use super::Term;
use std::char;
use std::collections::VecDeque;
use std::iter::Peekable;
use std::str::Chars;
use util::span::{Location, Span};

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum TokenKind {
    Ident(String),
    Int(u32),
    Unit,
    Lambda,
    Let,
    Equals,
    In,
    Dot,
    If,
    Then,
    Else,
    True,
    False,
    LParen,
    RParen,
    Invalid(char),
    Eof,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub const fn new(kind: TokenKind, span: Span) -> Token {
        Token { kind, span }
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
        Token::new(TokenKind::Int(n), span)
    }

    /// Lex a reserved keyword or an identifier
    fn keyword(&mut self) -> Token {
        let (data, span) = self.consume_while(|ch: char| ch.is_ascii_alphanumeric());
        let kind = match data.as_ref() {
            "unit" => TokenKind::Unit,
            "let" => TokenKind::Let,
            "in" => TokenKind::In,
            "fn" => TokenKind::Lambda,
            "if" => TokenKind::If,
            "then" => TokenKind::Then,
            "else" => TokenKind::Else,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            _ => TokenKind::Ident(data),
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
            None => return Token::new(TokenKind::Eof, Span::dummy()),
        };
        match next {
            x if x.is_ascii_alphabetic() => self.keyword(),
            x if x.is_numeric() => self.number(),
            '(' => self.eat('(', TokenKind::LParen),
            ')' => self.eat(')', TokenKind::RParen),
            '\\' => self.eat('\\', TokenKind::Lambda),
            'λ' => self.eat('λ', TokenKind::Lambda),
            '.' => self.eat('.', TokenKind::Dot),
            '=' => self.eat('=', TokenKind::Equals),
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
    ctx: DeBruijnIndexer,
    /// [`Lexer`] impls [`Iterator`] over [`TokenSpan`],
    /// so we can just directly wrap it in a [`Peekable`]
    lexer: Peekable<Lexer<'s>>,
    span: Span,
}

impl<'s> Parser<'s> {
    /// Create a new [`Parser`] for the input `&str`
    pub fn new(input: &'s str) -> Parser<'s> {
        Parser {
            ctx: DeBruijnIndexer::default(),
            lexer: Lexer::new(input.chars()).peekable(),
            span: Span::dummy(),
        }
    }

    fn consume(&mut self) -> Option<Token> {
        let ts = self.lexer.next()?;
        self.span = ts.span;
        Some(ts)
    }

    fn expect(&mut self, kind: TokenKind) -> Option<Token> {
        let tk = self.consume()?;
        match &tk.kind {
            t if t == &kind => Some(tk),
            _ => {
                eprintln!("Expected token {:?}, found {:?}", kind, tk.kind);
                None
            }
        }
    }

    fn expect_term(&mut self) -> Option<Box<Term>> {
        match self.term() {
            Some(term) => Some(term),
            None => {
                let sp = self.peek_span();
                eprintln!("Expected term  at {:?}", sp);
                None
            }
        }
    }

    fn peek(&mut self) -> Option<TokenKind> {
        self.lexer.peek().map(|tk| tk.kind.clone())
    }

    fn peek_span(&mut self) -> Span {
        self.lexer.peek().map(|s| s.span).unwrap_or(self.span)
    }

    fn lambda(&mut self) -> Option<Box<Term>> {
        let start = self.expect(TokenKind::Lambda)?;

        // Bind variable into a new context before parsing the body
        let var = self.ident()?;
        self.ctx.push(var);
        let _ = self.expect(TokenKind::Dot)?;
        let body = self.term()?;

        // Return to previous context
        self.ctx.pop();
        Some(Term::Abs(body).into())
    }

    fn let_expr(&mut self) -> Option<Box<Term>> {
        let start = self.expect(TokenKind::Let)?;
        let var = self.ident()?;

        let _ = self.expect(TokenKind::Equals)?;
        let bind = self.expect_term()?;
        self.ctx.push(var);
        let _ = self.expect(TokenKind::In)?;
        let body = self.expect_term()?;
        self.ctx.pop();
        Some(Term::Let(bind, body).into())
    }

    /// Parse an application of form:
    /// application = atom application' | atom
    /// application' = atom application' | empty
    fn application(&mut self) -> Option<Box<Term>> {
        let mut lhs = self.atom()?;
        let span = self.span;
        while let Some(rhs) = self.atom() {
            lhs = Term::App(lhs, rhs).into();
        }
        Some(lhs)
    }

    fn ident(&mut self) -> Option<String> {
        let Token { kind, span } = self.consume()?;
        match kind {
            TokenKind::Ident(s) => Some(s),
            _ => {
                eprintln!("Expected identifier, found {:?}", kind);
                None
            }
        }
    }

    fn if_expr(&mut self) -> Option<Box<Term>> {
        let _ = self.expect(TokenKind::If)?;
        let guard = self.expect_term()?;
        let _ = self.expect(TokenKind::Then)?;
        let csq = self.expect_term()?;
        let _ = self.expect(TokenKind::Else)?;
        let alt = self.expect_term()?;
        Some(Term::If(guard, csq, alt).into())
    }

    /// Parse an atomic term
    /// LPAREN term RPAREN | var
    fn atom(&mut self) -> Option<Box<Term>> {
        match self.peek()? {
            TokenKind::Let => self.let_expr(),
            TokenKind::Int(i) => {
                self.consume()?;
                Some(Term::Int(i as usize).into())
            }
            TokenKind::True => {
                self.consume();
                Some(Term::Bool(true).into())
            }
            TokenKind::False => {
                self.consume();
                Some(Term::Bool(false).into())
            }
            TokenKind::LParen => {
                self.expect(TokenKind::LParen)?;
                let term = self.term()?;
                self.expect(TokenKind::RParen)?;
                Some(term)
            }
            TokenKind::Unit => {
                self.expect(TokenKind::Unit)?;
                Some(Term::Unit.into())
            }
            TokenKind::If => self.if_expr(),
            TokenKind::Lambda => self.lambda(),
            TokenKind::Ident(s) => {
                let sp = self.consume()?.span;
                match self.ctx.lookup(&s) {
                    Some(idx) => Some(Term::Var(idx).into()),
                    None => {
                        eprintln!("Unbound variable {}", s);
                        None
                    }
                }
            }
            _ => None,
        }
    }

    fn term(&mut self) -> Option<Box<Term>> {
        match self.peek()? {
            // TokenKind::Lambda => self.lambda(),
            _ => self.application(),
        }
    }

    pub fn parse_term(&mut self) -> Option<Box<Term>> {
        self.term()
    }
}
