#[allow(dead_code)]
#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Token {
    Dot,
    Colon,
    Opaque,
    Semicolon,
    Comma,
    Apostrophe,
    Bar,
    SingleArrow,
    DoubleArrow,
    Wildcard,
    Asterisk,
    Equals,

    LParen,
    RParen,
    LBrace,
    RBrace,

    And,
    Function,
    Lambda,
    Val,
    Let,
    In,
    Case,
    Of,
    End,
    As,
    If,
    Then,
    Else,
    Type,
    Datatype,
    Fix,
    Rec,
    Exists,
    Forall,
    TyInt,
    TyBool,
    TyUnit,

    TypeAppSigil,

    LowerId(String),
    UpperId(String),

    Comment(String),
    Int(usize),
    Unit,
    Placeholder,
    Invalid(char),
    EOF,
}

impl Token {
    pub fn extract_string(self) -> String {
        match self {
            Token::LowerId(s) | Token::UpperId(s) | Token::Comment(s) => s,
            _ => panic!("Invalid token {:?}", self),
        }
    }
}
