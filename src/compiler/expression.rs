use std::fmt;

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
    pub body: BlockExpr
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
    Block(BlockExpr),
    // we can reuse the shape of a unary here ??
    Return(UnaryExpr)
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
        let body = match body {
            Expression::Block(b) => b,
            // todo: this points to the wrong token I think, but to get the token out we'd need to match every variant
            // because you can't say "ok, rust, you KNOW each variant contains a struct with a .token, come on"
            expr @ _ => BlockExpr { token: token.clone(), exprs: vec![expr] }
        };
        Expression::Function(FunctionExpr {
            token,
            name,
            arguments,
            body
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

    pub fn return_expr(token: Token, expr: Expression) -> Self {
        Expression::Return(UnaryExpr { token, value: Box::new(expr)})
    }
}


impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Literal(expr) => write!(f, "<{:?} @ {}:{}>", expr.token.typ, expr.token.line, expr.token.col),
            Expression::Unary(expr) => write!(f, "{:#?}", expr),
            Expression::Binary(expr) => write!(f, "{:#?}", expr),
            Expression::Assignment(expr) => write!(f, "{:#?}", expr),
            Expression::Conditional(expr) => write!(f, "{:#?}", expr),
            Expression::Function(expr) => write!(f, "{:#?}", expr),
            Expression::Call(expr) => write!(f, "{:#?}", expr),
            Expression::Tuple(expr) => write!(f, "{:#?}", expr),
            Expression::Quoted(expr) => write!(f, "{:#?}", expr),
            Expression::Block(expr) => write!(f, "{:#?}", expr),
            Expression::Return(expr) => write!(f, "{:#?}", expr)
        }
    }
}