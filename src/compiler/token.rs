#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    LiteralNull,
    LiteralBool(bool),
    LiteralNum(f64),
    LiteralStr(String),
    Name(String),

    Plus,
    Minus,
    Star,
    Slash,
    Power,

    Eq,
    NotEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    And,
    Or,
    Not,

    Assign,
    Let,

    If,
    Then,
    Else,
    For,
    In,
    Return,

    Arrow,

    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,

    Eval,
    Quote,

    Semicolon,
    Newline,

    EOF,
    Illegal(char)
}

// Tokens carry line/col information to aid in error reporting and debugging.
#[derive(Debug, Clone)]
pub struct Token {
    pub typ: TokenType,
    pub line: u32,
    pub col: u32
}

// As far as equality comparison, lineno/colno don't matter
impl PartialEq for Token {
    fn eq(&self, other: &Token) -> bool {
        self.typ == other.typ
    }
}