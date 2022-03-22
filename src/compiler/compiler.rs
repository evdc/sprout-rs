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


pub struct Compiler {
    local_count: u32,
    scope_depth: u32
}

impl Compiler {
    pub fn new() -> Self {
        Compiler { local_count: 0, scope_depth: 0 }
    }
}

