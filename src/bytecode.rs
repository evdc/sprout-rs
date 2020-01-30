use crate::opcode::Op;
use crate::value::Value;

#[derive(Debug)]
pub struct Bytecode {
    pub code: Vec<u8>,
    pub constants: Vec<Value>,
    lines: Vec<u32>,
}

impl Bytecode {
    pub fn new() -> Self {
        Bytecode {
            code: Vec::new(),
            constants: Vec::new(),
            lines: Vec::new(),
        }
    }

    pub fn add_code(&mut self, opcode: Op, line: u32) -> () {
        self.code.push(opcode as u8);
        self.lines.push(line);
    }

    // Write a constant to the constants array,
    // instruction to load it, and the constant's idx as the operand to that instruction.
    pub fn add_constant(&mut self, val: Value, line: u32) -> () {
        self.constants.push(val);
        let idx = self.constants.len() - 1;
        self.code.push(Op::LoadConstant as u8);
        self.code.push(idx as u8);
        self.lines.push(line);
    }
}

#[test]
fn test_bytecode() {
    let mut code = Bytecode::new();

    code.add_constant(42.0, 0);
    code.add_code(Op::Return, 0);

    println!("{:#?}", code);
}