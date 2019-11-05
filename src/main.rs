pub mod lexer;
pub mod tokens;

use tokens::Token;
use lexer::Lexer;

fn main() {
    let input = "1 + \"2 * 3\" * 4";
    let mut lex = Lexer::new(input);

    loop {
        let t = lex.next_token();
        if t == Token::EOF {
            break;
        }
        println!("{:?}", t)
    }
}
