#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Name(String),
    LiteralStr(String),
    LiteralInt(i32),
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

    EOF
}

