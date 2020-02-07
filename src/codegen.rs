use crate::ast::Expression;
use crate::bytecode::Bytecode;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::opcode::Op;
use crate::token::TokenType;
use crate::value::Value;

#[derive(Debug)]
pub enum CompileError {
    CompileError(String)
}

pub struct Compiler {
    pub current_chunk: Bytecode
}

impl Compiler {
    pub fn new() -> Self {
        Compiler { current_chunk: Bytecode::new() }
    }

    pub fn compile(&mut self, expression: &Expression) -> Result<(), CompileError> {
        self.compile_expression(expression)?;
        self.emit(Op::Return, 0);       // todo: how does it know this lineno
        Ok(())
    }

    fn compile_expression(&mut self, expression: &Expression) -> Result<(), CompileError> {
        match expression {
            Expression::Literal(token) => {
                let line = token.line;
                match token.typ {
                    TokenType::LiteralNull => self.emit(Op::LoadNull, line),
                    TokenType::LiteralBool(b) => {
                        if b == true {
                            self.emit(Op::LoadTrue, line)
                        } else {
                            self.emit(Op::LoadFalse, line)
                        }
                    },
                    TokenType::LiteralNum(n) => self.emit_constant(Value::Num(n), line),
                    _ => return Err(CompileError::CompileError("Unexpected literal token".to_string()))
                }
            },

            Expression::Unary(token, subexpr) => {
                let line = token.line;
                match token.typ {
                    TokenType::Minus => {
                        self.compile_expression(subexpr)?;
                        self.emit(Op::Negate, line);
                    },
                    _ => return Err(CompileError::CompileError("Unexpected unary token".to_string()))
                }
            },

            Expression::Infix(token, left, right) => {
                let line = token.line;
                self.compile_expression(&left)?;
                self.compile_expression(&right)?;
                match token.typ {
                    TokenType::Plus => self.emit(Op::Add, line),
                    TokenType::Minus => self.emit(Op::Sub, line),
                    TokenType::Star => self.emit(Op::Mul, line),
                    TokenType::Slash => self.emit(Op::Div, line),
                    TokenType::Power => self.emit(Op::Pow, line),
                    _ => return Err(CompileError::CompileError("Unexpected binary token".to_string()))
                }
            }
        };

        Ok(())
    }

    fn emit(&mut self, op: Op, line: u32) -> () {
        self.current_chunk.add_code(op, line)
    }

    fn emit_constant(&mut self, v: Value, line: u32) -> () {
        self.current_chunk.add_constant(v, line)
    }
}

#[test]
fn test_compiler() {
    let input = "1 + 2 * 3 - 4 / -5";
    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);
    let mut c = Compiler::new();

    let expr = p.expression(0).unwrap();

    let result = c.compile_expression(&expr);
    println!("{:?}", result);

    println!("{:#?}", c.current_chunk);

}