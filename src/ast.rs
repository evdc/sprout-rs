use crate::token::Token;

#[derive(Debug)]
pub enum Expression {
    Literal(Token),
    Unary(Token, Box<Expression>),
    Infix(Token, Box<Expression>, Box<Expression>)
}

#[derive(Debug)]
pub enum Statement {
    Assign(String, Expression),
    Expression(Expression)
}
