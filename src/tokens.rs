#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Name(String),
    LiteralStr(String),
    LiteralInt(i32),

    Plus,
    Minus,
    Star,
    Slash,
    Power,

    LParen,
    RParen,

    EOF
}

