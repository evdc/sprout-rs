use sprout_rs::compiler::lexer::Lexer;
use sprout_rs::compiler::parser::Parser;
use sprout_rs::compiler::codegen2::compile;
use sprout_rs::vm::vm::VM;

fn main() {
    //let input = "1 + 2 * 3 - 4 / -5";
    //let input = "not 2 <= 3";
    let input = "\"foo\" + \"bar\"";

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    let expr = p.expression(0).unwrap();
    println!("AST: {:#?}", expr);

    let bytecode = compile(expr).unwrap();

    println!("Bytecode: {:#?}", bytecode);

    let vm = VM::new(bytecode);
    let res = vm.run();
    println!("VM result: {:#?}", res);
}
