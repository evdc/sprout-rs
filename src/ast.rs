use crate::token::Token;

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
    Assign(AssignExpr),
    Unary(UnaryExpr),
    Binary(BinaryExpr)
}

// These constructor functions just reduce some of the boilerplate of instantiating an enum variant
// + a struct of the same name. It could be a macro
impl Expression {
    pub fn literal(token: Token) -> Expression { Expression::Literal(LiteralExpr { token }) }

    pub fn assign(token: Token, name: String, value: Expression) -> Expression {
        Expression::Assign(AssignExpr { token, name, value: Box::new(value) })
    }

    pub fn unary(token: Token, value: Expression) -> Expression {
        Expression::Unary(UnaryExpr { token, value: Box::new(value) })
    }

    pub fn binary(token: Token, left: Expression, right: Expression) -> Expression {
        Expression::Binary(BinaryExpr { token, left: Box::new(left), right: Box::new(right) })
    }
}
