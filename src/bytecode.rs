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

    pub fn add_code(&mut self, opcode: u8, line: u32) -> () {
        self.code.push(opcode);
        self.lines.push(line);
    }

    // truncates length to u8, as the LOAD_CONST operation works on a single byte
    pub fn add_constant(&mut self, val: Value, line: u32) -> u8 {
        self.constants.push(val);
        self.lines.push(line);
        return (self.constants.len() - 1) as u8;
    }
}

#[test]
fn test_bytecode() {
    let mut code = Bytecode::new();

    // Equivalent to the following assembly:
    // 0    LOAD_CONST      True
    // 0    RETURN
    let constant_idx = code.add_constant(Value::True, 0);
    code.add_code(Op::LoadConstant as u8, 0);
    code.add_code(constant_idx, 0);
    code.add_code(Op::Return as u8, 0);

    println!("{:#?}", code);
}