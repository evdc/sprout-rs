#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Name(String),
    Literal(String),

    Plus,
    Minus,
    Star,
    Slash,
    Power,

    LParen,
    RParen,

    EOF
}

