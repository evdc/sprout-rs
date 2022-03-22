use super::expression::*;
use super::token::TokenType;

use crate::vm::value::Value;
use crate::vm::opcode::Op;


#[derive(Debug)]
pub enum CompileError {
    CompileError(String)
}

pub type Code = Vec<Op>;

type CompileResult = Result<Code, CompileError>;

// This pure-function variant may not work with the state I assume a Compiler needs to keep?
trait Compile {
    // Takes ownership of self, an Expression, and destroys it
    fn compile(self) -> CompileResult;
}

// todo: we need a new way to carry over line/col information into the VM

impl Compile for LiteralExpr {
    fn compile(self) -> CompileResult {
        let op = match self.token.typ {
            TokenType::LiteralNull => Op::LoadNull,
            TokenType::LiteralBool(b) => {
                if b == true {
                    Op::LoadTrue
                } else {
                    Op::LoadFalse
                }
            },
            TokenType::LiteralNum(n) => Op::LoadConstant(Value::Num(n)),
            TokenType::LiteralStr(s) => Op::LoadConstant(Value::Str(s.clone())),

            TokenType::Name(name) => Op::GetGlobal(name),

            _ => return Err(CompileError::CompileError("Unexpected literal token".to_string()))
        };
        Ok(vec![op])
    }
}

impl Compile for AssignExpr {
    fn compile(self) -> CompileResult {
        let mut code = self.value.compile()?;
        code.push(Op::SetGlobal(self.name.clone()));
        Ok(code)
    }
}

impl Compile for UnaryExpr {
    fn compile(self) -> CompileResult {
        let mut code = self.value.compile()?;
        let op = match self.token.typ {
            TokenType::Minus => Op::Negate,
            TokenType::Not   => Op::Not,

            _ => return Err(CompileError::CompileError("Unexpected unary token".to_string()))
        };
        code.push(op);
        Ok(code)
    }
}

impl Compile for BinaryExpr {
    fn compile(self) -> CompileResult {
        let mut code = self.left.compile()?;
        let right_code = self.right.compile()?;
        let op = match self.token.typ {
            TokenType::Plus     => Op::Add,
            TokenType::Minus    => Op::Sub,
            TokenType::Star     => Op::Mul,
            TokenType::Slash    => Op::Div,
            TokenType::Power    => Op::Pow,
            TokenType::Lt       => Op::Lt,
            TokenType::LtEq     => Op::LtEq,
            TokenType::Gt       => Op::Gt,
            TokenType::GtEq     => Op::GtEq,
            TokenType::Eq       => Op::Eq,
            TokenType::NotEq    => Op::NotEq,
            TokenType::And      => {
                // skip the right code if falsey
                code.push(Op::JumpIfFalse(right_code.len() + 1));
                code.push(Op::Pop);
                code.extend(right_code);
                return Ok(code);
            },
            TokenType::Or       => {
                code.push(Op::JumpIfFalse(1));  // skip jump
                code.push(Op::Jump(right_code.len() + 1));  // skip RHS + pop
                code.push(Op::Pop);
                code.extend(right_code);
                return Ok(code);
            }

            _ => return Err(CompileError::CompileError("Unexpected binary token".to_string()))
        };
        code.extend(right_code);
        code.push(op);
        Ok(code)
    }
}

impl Compile for ConditionalExpr {
    fn compile(self) -> CompileResult {
        // We can avoid patching jumps here by determining their lengths ahead of time.
        let mut code = self.condition_expr.compile()?;

        let true_code = self.true_expr.compile()?;
        code.push(Op::JumpIfFalse(true_code.len() + 1)); // skip the true branch + the pop
        code.push(Op::Pop);
        code.extend(true_code);

        if let Some(false_branch) = *self.false_expr {
            let false_code = false_branch.compile()?;
            // append an absolute jump to the end of the true branch
            code.push(Op::Jump(false_code.len()));
            code.extend(false_code);
        } else {
            // implicit "else null" at the end
            code.push(Op::Jump(1));
            code.push(Op::LoadNull);
        }

        Ok(code)
    }
}

impl Compile for FunctionExpr {
    fn compile(self) -> CompileResult {
        println!("{:#?}", self);
        todo!("yeah no")
    }
}

impl Compile for TupleExpr {
    fn compile(self) -> CompileResult {
        todo!("yeah no")
    }
}

impl Compile for Expression {
    fn compile(self) -> CompileResult {
        match self {
            Expression::Literal(expr) => expr.compile(),
            Expression::Unary(expr)   => expr.compile(),
            Expression::Binary(expr)  => expr.compile(),
            Expression::Assignment(expr)    => expr.compile(),
            Expression::Conditional(expr)   => expr.compile(),
            Expression::Function(expr)      => expr.compile(),
            Expression::Tuple(expr)   => expr.compile()
        }
    }
}

pub fn compile(statement: Statement) -> CompileResult {
    let mut code = Vec::new();

    match statement {
        Statement::Block(statements) => {
            for st in statements {
                code.extend(compile(*st)?);
            }
        },

        Statement::Expression(expr) => {
            code.extend(expr.compile()?);
        }
    }

    code.push(Op::Return);
    Ok(code)
}


#[cfg(test)]
mod test {
    use crate::compiler::lexer::Lexer;
    use crate::compiler::parser::Parser;
    use super::compile;

    #[test]
    fn test_compile() {
        let input = "let foo = 3 + 4;";
        let ast = Parser::parse(input).unwrap();

        let result = compile(ast);
        println!("{:?}", result);
    }
}