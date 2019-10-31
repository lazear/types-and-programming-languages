pub mod lexer;
pub mod parser;
use util::span::Span;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum TokenKind {
    Ident(String),
    Nat(u32),
    TyNat,
    TyBool,
    TyArrow,
    TyUnit,
    TypeDecl,
    Unit,
    True,
    False,
    Lambda,
    Succ,
    Pred,
    If,
    Then,
    Else,
    Let,
    In,
    IsZero,
    Semicolon,
    Colon,
    Comma,
    Proj,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Equals,
    Bar,
    Invalid(char),
    Dummy,
    Eof,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub const fn dummy() -> Token {
        Token {
            kind: TokenKind::Dummy,
            span: Span::dummy(),
        }
    }

    pub const fn new(kind: TokenKind, span: Span) -> Token {
        Token { kind, span }
    }
}
