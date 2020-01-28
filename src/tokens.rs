#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Word(String),
    LiteralStr(String),
    LiteralInt(i64),
    LiteralBool(bool),

    Plus,
    Minus,
    Star,
    Slash,
    Power,

    And,
    Or,
    Not,

    LParen,
    RParen,

    EOF,
    Illegal(char)
}

