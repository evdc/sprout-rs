use crate::token::Token;

#[derive(Debug)]
pub enum Expression {
    Literal(Token),
    Unary(Token, Box<Expression>),
    Infix(Token, Box<Expression>, Box<Expression>)
}