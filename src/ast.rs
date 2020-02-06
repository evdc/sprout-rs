use crate::token::Token;

#[derive(Debug)]
pub enum Expression {
    Literal(Token),
    Prefix(Token, Box<Expression>),
    Infix(Token, Box<Expression>, Box<Expression>)
}