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
                self.compile_expression(subexpr)?;
                match token.typ {
                    TokenType::Minus => self.emit(Op::Negate, line),
                    TokenType::Not   => self.emit(Op::Not, line),

                    _ => return Err(CompileError::CompileError("Unexpected unary token".to_string()))
                }
            },

            Expression::Infix(token, left, right) => {
                let line = token.line;
                self.compile_expression(&left)?;
                self.compile_expression(&right)?;
                match token.typ {
                    TokenType::Plus     => self.emit(Op::Add, line),
                    TokenType::Minus    => self.emit(Op::Sub, line),
                    TokenType::Star     => self.emit(Op::Mul, line),
                    TokenType::Slash    => self.emit(Op::Div, line),
                    TokenType::Power    => self.emit(Op::Pow, line),
                    TokenType::Lt       => self.emit(Op::Lt, line),
                    TokenType::LtEq     => self.emit(Op::LtEq, line),
                    TokenType::Gt       => self.emit(Op::Gt, line),
                    TokenType::GtEq     => self.emit(Op::GtEq, line),
                    TokenType::Eq       => self.emit(Op::Eq, line),
                    TokenType::NotEq    => self.emit(Op::NotEq, line),

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

// experimentally you could define a trait CompileInfix which TokenType variants impl
// and then `token.compile_infix()` replaces the match on L62 above
// doing all those impls is more verbose than a match expr but more extensible,
// but the Expression Problem doesn't really matter here as you own the whole codebase

#[test]
fn test_compiler() {
    let input = "2 < 3";
    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);
    let mut c = Compiler::new();

    let expr = p.expression(0).unwrap();

    let result = c.compile_expression(&expr);
    println!("{:?}", result);

    println!("{:#?}", c.current_chunk);

}