#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Op {
    Return,

    LoadConstant,
    LoadNull,
    LoadTrue,
    LoadFalse,

    Negate,
    Add,
    Sub,
    Mul,
    Div,
    Pow,

    // A hack that allows us to avoid wrapping all Op in an Option/Result;
    // we can make converting from bytes infallible.
    Invalid
}

impl From<u8> for Op {
    fn from(byte: u8) -> Op {
        match byte {
            v if v == Op::Return as u8 => Op::Return,
            v if v == Op::LoadConstant as u8 => Op::LoadConstant,
            v if v == Op::LoadTrue as u8 => Op::LoadTrue,
            v if v == Op::LoadFalse as u8 => Op::LoadFalse,
            v if v == Op::LoadNull as u8 => Op::LoadNull,
            v if v == Op::Negate as u8 => Op::Negate,
            v if v == Op::Add as u8 => Op::Add,
            v if v == Op::Sub as u8 => Op::Sub,
            v if v == Op::Mul as u8 => Op::Mul,
            v if v == Op::Div as u8 => Op::Div,
            v if v == Op::Pow as u8 => Op::Pow,
            _ => Op::Invalid
        }
    }
}

#[test]
fn test_opcode() {
    let code = Op::from(0u8);

    println!("{:#?}", code);
}