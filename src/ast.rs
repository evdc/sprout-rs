use crate::token::Token;

// AST nodes (currently only Expressions) carry Tokens (owned),
// so that they can carry line/col info through to the runtime, for error reporting.
// It could perhaps be more useful to lift the type, line and col into attributes directly on the Expression?

#[derive(Debug)]
pub enum Expression {
    Literal(Token),
    Assign(Token, Box<Expression>),     // The token here is the name on the LHS.
    Unary(Token, Box<Expression>),
    Infix(Token, Box<Expression>, Box<Expression>)
}