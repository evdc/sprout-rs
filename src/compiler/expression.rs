use crate::compiler::token::Token;

// AST nodes (currently only Expressions) carry Tokens (owned),
// so that they can carry line/col info through to the runtime, for error reporting.

#[derive(Debug)]
pub struct LiteralExpr {
    pub token: Token
}

#[derive(Debug)]
pub struct AssignExpr {
    pub token: Token,
    pub name: String,
    pub value: Box<Expression>
}

#[derive(Debug)]
pub struct UnaryExpr {
    pub token: Token,
    pub value: Box<Expression>
}

#[derive(Debug)]
pub struct BinaryExpr {
    pub token: Token,
    pub left: Box<Expression>,
    pub right: Box<Expression>
}

#[derive(Debug)]
pub enum Expression {
    Literal(LiteralExpr),
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    Assignment(AssignExpr)
}

impl Expression {
    // convenience constructors
    pub fn literal(token: Token) -> Self {
        Expression::Literal(LiteralExpr { token })
    }

    pub fn unary(token: Token, value: Expression) -> Self {
        Expression::Unary(UnaryExpr { token, value: Box::new(value) })
    }

    pub fn binary(token: Token, left: Expression, right: Expression) -> Self {
        Expression::Binary(BinaryExpr { token, left: Box::new(left), right: Box::new(right) })
    }

    pub fn assign(token: Token, name: String, right: Expression) -> Self {
        Expression::Assignment(AssignExpr { token, name, value: Box::new(right) })
    }
}

// A series of statements makes up your program
#[derive(Debug)]
pub enum Statement {
    Expression(Box<Expression>),
    Block(Vec<Box<Statement>>)
}