use crate::compiler::token::Token;

// AST nodes (currently only Expressions) carry Tokens (owned),
// so that they can carry line/col info through to the runtime, for error reporting.

#[derive(Debug, Clone, PartialEq)]
pub struct LiteralExpr {
    pub token: Token
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignExpr {
    pub token: Token,
    pub target: Box<Expression>,
    pub value: Box<Expression>
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr {
    pub token: Token,
    pub value: Box<Expression>
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpr {
    pub token: Token,
    pub left: Box<Expression>,
    pub right: Box<Expression>
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConditionalExpr {
    pub token: Token,
    pub condition_expr: Box<Expression>,
    pub true_expr: Box<Expression>,
    pub false_expr: Box<Option<Expression>>     // should this really be a Option<Box<Expression>>?
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleExpr {
    pub token: Token,
    pub items: Vec<Expression>
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionExpr {
    pub token: Token,
    pub name: String,
    pub arguments: Vec<String>,
    pub body: Box<Expression>
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpr {
    pub token: Token,
    pub callee: Box<Expression>,
    pub arguments: Vec<Expression>
}

#[derive(Debug, Clone, PartialEq)]
pub struct QuotedExpr {
    pub token: Token,
    pub subexpr: Box<Expression>
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockExpr {
    pub token: Token,
    pub exprs: Vec<Expression>
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Literal(LiteralExpr),
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    Assignment(AssignExpr),
    Conditional(ConditionalExpr),
    Function(FunctionExpr),
    Call(CallExpr),
    Tuple(TupleExpr),
    Quoted(QuotedExpr),
    Block(BlockExpr)
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

    pub fn assign(token: Token, target: Expression, value: Expression) -> Self {
        Expression::Assignment(AssignExpr { token, target: Box::new(target), value: Box::new(value) })
    }

    pub fn conditional(token: Token, condition_expr: Expression, true_expr: Expression, false_expr: Option<Expression>) -> Self {
        Expression::Conditional(ConditionalExpr {
            token,
            condition_expr: Box::new(condition_expr),
            true_expr: Box::new(true_expr),
            false_expr: Box::new(false_expr)
        })
    }

    pub fn function(token: Token, name: String, arguments: Vec<String>, body: Expression) -> Self {
        Expression::Function(FunctionExpr {
            token,
            name,
            arguments,
            body: Box::new(body)
        })
    }

    pub fn call(token: Token, callee: Expression, arguments: Vec<Expression>) -> Self {
        Expression::Call(CallExpr { token, callee: Box::new(callee), arguments})
    }

    pub fn tuple(token: Token, items: Vec<Expression>) -> Self {
        Expression::Tuple(TupleExpr {token, items})
    }

    pub fn quoted(token: Token, subexpr: Expression) -> Self {
        Expression::Quoted(QuotedExpr { token, subexpr: Box::new(subexpr)})
    }

    pub fn block(token: Token, exprs: Vec<Expression>) -> Self {
        Expression::Block(BlockExpr { token, exprs })
    }
}
