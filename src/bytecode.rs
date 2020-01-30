use crate::opcode::Op;
use crate::value::Value;

#[derive(Debug)]
pub struct Bytecode {
    pub code: Vec<u8>,
    constants: Vec<Value>,
    lines: Vec<i32>
}

impl Bytecode {
    pub fn new() -> Self {
        Bytecode { code: Vec::new(), constants: Vec::new(), lines: Vec::new() }
    }

    pub fn add_code(&mut self, opcode: u8, line: i32) -> () {
        self.code.push(opcode);
        self.lines.push(line);
    }

    pub fn add_constant(&mut self, val: Value) -> usize {
        self.constants.push(val);
        return self.constants.len() - 1;

    }
}

#[test]
fn test_bytecode() {
    let mut code = Bytecode::new();

    code.add_code(Op::Return as u8, 0);

    println!("{:#?}", code);
}