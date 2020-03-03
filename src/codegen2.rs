use crate::ast::*;
use crate::bytecode::Bytecode;
use crate::opcode::Op;
use crate::token::TokenType;
use crate::value::Value;

#[derive(Debug)]
pub enum CompileError {
    CompileError(String)
}

type CompileResult = Result<(), CompileError>;

fn write_constant(bytecode: &mut Bytecode, value: Value, line: u32) {
    let idx = bytecode.add_constant(value);
    bytecode.add_bytes(Op::LoadConstant as u8, idx, line);
}

// This pure-function variant may not work with the state I assume a Compiler needs to keep?
trait Compile {
    // Takes ownership of self, an Expression, and destroys it
    fn compile_into(self, bytecode: &mut Bytecode) -> CompileResult;
}


// note: we need to have an "emit", "emit_constant" etc. fn in scope
impl Compile for LiteralExpr {
    fn compile_into(self, bytecode: &mut Bytecode) -> CompileResult {
        let line = self.token.line;
        match self.token.typ {
            TokenType::LiteralNull => bytecode.add_byte(Op::LoadNull as u8, line),
            TokenType::LiteralBool(b) => {
                if b == true {
                    bytecode.add_byte(Op::LoadTrue as u8, line)
                } else {
                    bytecode.add_byte(Op::LoadFalse as u8, line)
                }
            },
            TokenType::LiteralNum(n) => write_constant(bytecode, Value::Num(n), line),
            TokenType::LiteralStr(s) => write_constant(bytecode, Value::Str(s.clone()), line),

            TokenType::Name(name) => {
                let global_idx = bytecode.add_constant(Value::Str(name.clone()));
                bytecode.add_bytes(Op::GetGlobal as u8, global_idx, line);
            }

            _ => return Err(CompileError::CompileError("Unexpected literal token".to_string()))
        }
        Ok(())
    }
}

impl Compile for UnaryExpr {
    fn compile_into(self, bytecode: &mut Bytecode) -> CompileResult {
        compile_into(*self.value, bytecode)?;
        let op = match self.token.typ {
            TokenType::Minus => Op::Negate,
            TokenType::Not   => Op::Not,

            _ => return Err(CompileError::CompileError("Unexpected unary token".to_string()))
        };
        bytecode.add_byte(op as u8, self.token.line);
        Ok(())
    }
}

impl Compile for BinaryExpr {
    fn compile_into(self, bytecode: &mut Bytecode) -> CompileResult {
        let line = self.token.line;
        compile_into(*self.left, bytecode)?;
        compile_into(*self.right, bytecode)?;
        match self.token.typ {
            TokenType::Plus     => bytecode.add_byte(Op::Add as u8, line),
            TokenType::Minus    => bytecode.add_byte(Op::Sub as u8, line),
            TokenType::Star     => bytecode.add_byte(Op::Mul as u8, line),
            TokenType::Slash    => bytecode.add_byte(Op::Div as u8, line),
            TokenType::Power    => bytecode.add_byte(Op::Pow as u8, line),
            TokenType::Lt       => bytecode.add_byte(Op::Lt as u8, line),
            TokenType::LtEq     => bytecode.add_byte(Op::LtEq as u8, line),
            TokenType::Gt       => bytecode.add_byte(Op::Gt as u8, line),
            TokenType::GtEq     => bytecode.add_byte(Op::GtEq as u8, line),
            TokenType::Eq       => bytecode.add_byte(Op::Eq as u8, line),
            TokenType::NotEq    => bytecode.add_byte(Op::NotEq as u8, line),

            _ => return Err(CompileError::CompileError("Unexpected binary token".to_string()))
        }
        Ok(())
    }
}

fn compile_into(expr: Expression, bytecode: &mut Bytecode) -> CompileResult {
    match expr {
        Expression::Literal(expr) => expr.compile_into(bytecode),
        Expression::Unary(expr) => expr.compile_into(bytecode),
        Expression::Assign(_expr) => unimplemented!(),
        Expression::Binary(expr) => expr.compile_into(bytecode)
    }
}

pub fn compile(expr: Expression) -> Result<Bytecode, CompileError> {
    let mut code = Bytecode::new();
    compile_into(expr, &mut code)?;
    Ok(code)
}

#[cfg(test)]
mod test {
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use super::compile;

    #[test]
    fn test_compile() {
        let input = "2 < 3";
        let mut l = Lexer::new(input);
        let mut p = Parser::new(&mut l);
        let expr = p.expression(0).unwrap();

        let result = compile(expr);
        println!("{:?}", result);
    }
}