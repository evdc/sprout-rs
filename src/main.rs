pub mod token;
pub mod lexer;
pub mod ast;
pub mod parser;

pub mod bytecode;
pub mod opcode;
pub mod value;
pub mod vm;
pub mod codegen;

use lexer::Lexer;
use parser::Parser;
use codegen::Compiler;
use vm::VM;


fn main() {
    let input = "1 + 2 * 3 - 4 / -5";
    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);
    let mut c = Compiler::new();

    let expr = p.expression(0).unwrap();
    println!("AST: {:#?}", expr);

    let result = c.compile(&expr);

    let vm = VM::new(c.current_chunk);
    let res = vm.run();
    println!("VM result: {:#?}", res);
}
