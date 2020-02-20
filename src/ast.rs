use crate::token::Token;

#[derive(Debug)]
pub enum Expression {
    Literal(Token),
    Variable(Token),
    Assign(String, Box<Expression>),
    Unary(Token, Box<Expression>),
    Infix(Token, Box<Expression>, Box<Expression>)
}