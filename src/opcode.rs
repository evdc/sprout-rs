#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Op {
    Return,
    LoadConstant
}

impl Op {
    pub fn from_byte(byte: u8) -> Option<Op> {
        match byte {
            v if v == Op::Return as u8 => Some(Op::Return),
            v if v == Op::LoadConstant as u8 => Some(Op::LoadConstant),
            _ => None
        }
    }
}

#[test]
fn test_opcode() {
    let code = Op::from_byte(0u8);

    println!("{:#?}", code);
}