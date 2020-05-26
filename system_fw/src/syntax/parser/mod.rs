pub mod decls;
pub mod exprs;
pub mod infix;
pub mod patterns;
pub mod types;

use super::ast::*;
use super::lexer::Lexer;
use super::tokens::*;
// use super::stack::Stack;

use infix::Infix;
use util::span::{Span, Spanned};

pub struct Parser<'s> {
    tokens: Lexer<'s>,
    current: Spanned<Token>,
    prev: Span,
    infix: Infix,

    next_ast_id: AstId,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum ErrorKind {
    ExpectedToken(Token),
    ExpectedIdentifier,
    ExpectedType,
    ExpectedExpr,
    ExpectedPattern,
    ExpectedDeclaration,
    ExpectedSpecification,
    ExpectedSignature,
    ExpectedStructure,
    UnboundTypeVar,
    UnboundExprVar,
    FunctionIdMismatch,
    EOF,
}

#[derive(Default)]
pub struct InfixState(Infix);

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Error {
    pub span: Span,
    pub token: Token,
    pub kind: ErrorKind,
}

impl<'s> Parser<'s> {
    pub fn new(input: &'s str) -> Parser<'s> {
        Parser::with_infix_state(input, InfixState::default())
    }

    pub fn with_infix_state(input: &'s str, state: InfixState) -> Parser<'s> {
        let mut p = Parser {
            tokens: Lexer::new(input.chars()),
            current: Spanned::new(Span::zero(), Token::Placeholder),
            infix: state.0,
            prev: Span::zero(),
            next_ast_id: AstId(0),
        };
        p.bump();
        p
    }

    pub fn top_level(&mut self) -> Result<Vec<Decl>, Error> {
        let mut v = Vec::new();
        while self.current() != &Token::EOF {
            v.push(self.parse_decl()?);
            self.bump_if(&Token::Semicolon);
        }
        Ok(v)
    }

    pub fn state(&self) -> InfixState {
        InfixState(self.infix.clone())
    }

    fn allocate_ast_id(&mut self) -> AstId {
        let id = self.next_ast_id;
        self.next_ast_id = AstId(self.next_ast_id.0 + 1);
        id
    }

    /// Generate a parsing error. These are not necessarily fatal
    fn error<T>(&self, k: ErrorKind) -> Result<T, Error> {
        Err(Error {
            span: self.current.span,
            token: self.current().clone(),
            kind: k,
        })
    }

    fn current(&self) -> &Token {
        &self.current.data
    }

    /// Bump the current token, returning it, and pull a new token
    /// from the lexer
    fn bump(&mut self) -> Token {
        match self.tokens.next() {
            Some(t) => {
                #[cfg(test)]
                {
                    let t = std::mem::replace(&mut self.current, t).data();
                    self.current.span = Span::default();
                    self.prev = Span::default();

                    t
                }
                #[cfg(not(test))]
                {
                    self.prev = self.current.span;
                    std::mem::replace(&mut self.current, t).data()
                }
            }
            None => std::mem::replace(&mut self.current.data, Token::EOF),
        }
    }

    /// Ignore a token matching `kind`
    fn bump_if(&mut self, kind: &Token) -> bool {
        if &self.current.data == kind {
            self.bump();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, kind: Token) -> Result<(), Error> {
        if self.current() == &kind {
            self.bump();
            Ok(())
        } else {
            self.error(ErrorKind::ExpectedToken(kind))
        }
    }

    fn expect_lower_id(&mut self) -> Result<String, Error> {
        match self.current() {
            Token::LowerId(_) => Ok(self.bump().extract_string()),
            _ => self.error(ErrorKind::ExpectedIdentifier),
        }
    }

    fn expect_upper_id(&mut self) -> Result<String, Error> {
        match self.current() {
            Token::UpperId(_) => Ok(self.bump().extract_string()),
            _ => self.error(ErrorKind::ExpectedIdentifier),
        }
    }

    /// Call `func` once, returning the `Result<T,E>` of the function.
    /// A failure of `func` may have side effects, including emitting
    /// diagnostics containing `message`
    ///
    /// Generally, this is just used to give better error messages
    fn once<T, E, F>(&mut self, func: F, message: &str) -> Result<T, E>
    where
        F: Fn(&mut Parser) -> Result<T, E>,
    {
        match func(self) {
            Ok(t) => Ok(t),
            Err(e) => {
                eprintln!("[Parser] {}", message);
                Err(e)
            }
        }
    }

    /// Collect the result of `func` into a `Vec<T>` as long as `func` returns
    /// an `Ok(T)`. A call to `func` must succeed on the first try, or an error
    /// is immediately returned. Subsequent calls to `func` may fail, in which
    /// case the error is discarded, and the results are returned. If `delimit`
    /// is supplied, the parser will discard matching tokens between each call
    /// to `func`
    fn plus<T, E, F>(&mut self, func: F, delimit: Option<&Token>) -> Result<Vec<T>, E>
    where
        F: Fn(&mut Parser) -> Result<T, E>,
    {
        let mut v = vec![func(self)?];
        if let Some(t) = delimit {
            if !self.bump_if(t) {
                return Ok(v);
            }
        }
        while let Ok(x) = func(self) {
            v.push(x);
            if let Some(t) = delimit {
                if !self.bump_if(t) {
                    break;
                }
            }
        }
        Ok(v)
    }

    /// Collect the result of `func` into a `Vec<T>` as long as `func` returns
    /// an `Ok(T)`. If an error is encountered, it is discarded and the results
    /// are immediately returned. If `delimit` is supplied, the parser will
    /// discard matching tokens between each call to `func`
    fn star<T, E, F>(&mut self, func: F, delimit: Option<&Token>) -> Vec<T>
    where
        F: Fn(&mut Parser) -> Result<T, E>,
    {
        let mut v = Vec::new();
        while let Ok(x) = func(self) {
            v.push(x);
            if let Some(t) = delimit {
                if !self.bump_if(t) {
                    break;
                }
            }
        }
        v
    }

    /// Identical semantics to `Parser::plus`, except `delimit` must be supplied
    fn delimited<T, E, F>(&mut self, func: F, delimit: Token) -> Result<Vec<T>, E>
    where
        F: Fn(&mut Parser) -> Result<T, E>,
    {
        let mut v = vec![func(self)?];
        while self.bump_if(&delimit) {
            v.push(func(self)?);
        }
        Ok(v)
    }
}
