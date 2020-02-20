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

    pub fn add_byte(&mut self, byte: u8, line: u32) -> () {
        self.code.push(byte);
        self.lines.push(line);
    }

    pub fn add_bytes(&mut self, b1: u8, b2: u8, line: u32) -> () {
        self.code.push(b1);
        self.code.push(b2);
        self.lines.push(line);
    }

    // Write a constant to the constants array. Check if it exists first.
    // (This is slow; we could use a HashSet)
    // Return its index in the constant array, for use with a LOAD_CONSTANT instruction.
    pub fn add_constant(&mut self, val: Value) -> u8 {
        self.constants.iter().position(|x| *x == val)
            .unwrap_or_else(|| { self.constants.push(val); self.constants.len() - 1}) as u8
    }
}

#[test]
fn test_bytecode() {
    let mut code = Bytecode::new();

    code.add_constant(Value::Num(42.0));
    code.add_byte(Op::Return as u8, 0);

    println!("{:#?}", code);
}

#[test]
fn test_bytecode_no_duplicate_constants() {
    let mut code = Bytecode::new();

    let idx1 = code.add_constant(Value::Str("Hello world!".to_owned()));
    let idx2 = code.add_constant(Value::Str("A different string".to_owned()));
    let idx3 = code.add_constant(Value::Str("Hello world!".to_owned()));
    code.add_byte(Op::Return as u8, 0);

    assert_eq!(idx1, idx3);
    assert_ne!(idx1, idx2);

    println!("{:#?}", code);
}
