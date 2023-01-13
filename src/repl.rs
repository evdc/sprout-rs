use crate::compiler::parser::{Parser, ParseError};
use crate::compiler::codegen2::{Compiler, CompileError};
use crate::vm::vm::{VM, VMError};
use crate::vm::value::Value;

use std::io;
use std::io::Write;

#[derive(Debug)]
enum AnyErr {
    ParseError(ParseError),
    CompileError(CompileError),
    VMError(VMError)
}



pub fn run() {
    let mut vm = VM::new();
    loop {
        let input = get_input("🌱>> ");
        // todo: handle control chars like up-arrow before we run it
        let trimmed = input.trim();
        if trimmed.is_empty() {
            continue;
        }

        let res = run_one(&mut vm, &trimmed);

        println!("{:?}", res)
    }
}

fn run_one(vm: &mut VM, input: &str) -> Result<Value, AnyErr> {
    let block = Parser::parse(input).map_err(AnyErr::ParseError)?;
    let mut compiler = Compiler::new();
    let code = compiler.compile(block).map_err(AnyErr::CompileError)?;

    println!("Code: {:?}", code);

    let res = vm.run(code).map_err(AnyErr::VMError)?;

    Ok(res)
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