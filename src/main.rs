pub mod token;
pub mod lexer;
pub mod ast;
pub mod parser;

//pub mod bytecode;
//pub mod opcode;
//pub mod value;
//pub mod vm;
// pub mod codegen;

use token::TokenType;
use lexer::Lexer;
// use ast::ASTNode;
// use parser::Parser;

fn main() {
    let input = "true and not false + foo + \"bar\" * 3";
    let mut lex = Lexer::new(input);

    loop {
        let t = lex.next_token();
        if t.typ == TokenType::EOF {
            break;
        }
        println!("{:?}", t)
    }
}
