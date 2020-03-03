use crate::compiler::lexer::Lexer;
use crate::compiler::parser::{Parser, ParseError};
use crate::compiler::codegen2::{compile, CompileError};
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
    loop {
        let input = get_input("🌱>> ");

        match run_one(input) {
            Ok(res) => println!("{:?}", res),
            Err(err) => println!("Error: {:?}", err)
        }
    }
}

fn run_one(input: String) -> Result<Value, AnyErr> {
    let mut l = Lexer::new(&input);
    let mut p = Parser::new(&mut l);

    let expr = p.expression(0).map_err(AnyErr::ParseError)?;
    let code = compile(expr).map_err(AnyErr::CompileError)?;

    let vm = VM::new(code);
    let res = vm.run().map_err(AnyErr::VMError)?;

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