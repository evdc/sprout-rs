pub mod lexer;
pub mod tokens;
pub mod ast;

use tokens::Token;
use lexer::Lexer;
use ast::ASTNode;

fn main() {
    let input = "true and not false + foo + \"bar\" * 3";
    let mut lex = Lexer::new(input);

    loop {
        let t = lex.next_token();
        if t == Token::EOF {
            break;
        }
        println!("{:?}", t)
    }
}
