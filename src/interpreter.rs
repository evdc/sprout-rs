use crate::compiler::parser::{Parser, ParseError};
use crate::compiler::codegen2::{Compiler, CompileError};
use crate::vm::vm::{VM, VMError};
use crate::vm::value::Value;

use std::io;
use std::io::Write;

#[derive(Debug)]
pub enum AnyErr {
    ParseError(ParseError),
    CompileError(CompileError),
    VMError(VMError)
}

pub struct Interpreter {
    vm: VM,
    compiler: Compiler,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter { vm: VM::new(), compiler: Compiler::new() }
    }

    pub fn run(&mut self, source: &str) -> Result<Value, AnyErr> {
        let block = Parser::parse(source, &mut self.compiler, &mut self.vm).map_err(AnyErr::ParseError)?;
        
        let function = self.compiler.compile(block).map_err(AnyErr::CompileError)?;
    
        println!("Code: {:?}", function.code);
    
        let res = self.vm.run(function).map_err(AnyErr::VMError)?;
    
        Ok(res)
    }
}

pub fn repl() {
    let mut interpreter = Interpreter::new();
    loop {
        let input = get_input("🌱>> ");
        // todo: handle control chars like up-arrow before we run it
        let trimmed = input.trim();
        if trimmed.is_empty() {
            continue;
        }

        let res = interpreter.run(&trimmed);

        println!("{:?}", res)
    }
}

fn get_input(prompt: &str) -> String {
    let mut stdout = io::stdout();
    let stdin = io::stdin();
    let mut input = String::new();

    print!("{}", prompt);
    stdout.flush().expect("Failed to flush stdout");
    stdin.read_line(&mut input).expect("Failed to read line from stdin");

    input
}