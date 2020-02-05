use crate::ast::ASTNode;
use crate::bytecode::Bytecode;
use crate::opcode::Op;
use crate::tokens::Token;
use crate::value::Value;

#[derive(Debug)]
pub enum CompileError {
    CompileError(String)
}

pub struct Compiler {
    current_chunk: Bytecode
}

impl Compiler {
    fn new(&mut self) -> Self {
        Compiler { current_chunk: Bytecode::new() }
    }

    // TODO: everything is on line 0, fight me
    fn compile_expression(&mut self, expression: &ASTNode) -> Result<(), CompileError> {
        match expression.token {

            // Literal expressions emit literal opcodes
            Token::LiteralNull => self.emit(Op::LoadNull, 0),
            Token::LiteralBool(b) => {
                if b == true {
                    self.emit(Op::LoadTrue, 0)
                } else {
                    self.emit(Op::LoadFalse, 0)
                }
            },
            Token::LiteralInt(i) => self.emit_constant(Value::Num(i as f64), 0),

            // Binary ops
            // I think go in post order?
            Token::Plus
            | Token::Minus      // breaks unary negate, which is an argument in favor of ASTNode types being different from tokens
            | Token::Slash
            | Token::Star
            | Token::And
            | Token::Or => {
                self.compile_expression(&expression.children[1]);
                self.compile_expression(&expression.children[0]);
                self.emit();    // yeah fix it
            }

            _ => return Err(CompileError::CompileError(format!("Invalid token {:?}", expression.token)))
        };
        Ok(())
    }

    // TODO: these should be collapsed, to something that just emits 1 op + n bytes operand
    fn emit(&mut self, op: Op, line: u32) -> () {
        self.current_chunk.add_code(op, line)
    }

    fn emit_constant(&mut self, v: Value, line: u32) -> () {
        self.current_chunk.add_constant(v, line)
    }
}