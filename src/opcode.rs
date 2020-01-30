#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Op {
    Return,
    LoadConstant,

    // A hack that allows us to avoid wrapping all Op in an Option/Result;
    // we can make converting from bytes infallible.
    Invalid
}

impl From<u8> for Op {
    fn from(byte: u8) -> Op {
        match byte {
            v if v == Op::Return as u8 => Op::Return,
            v if v == Op::LoadConstant as u8 => Op::LoadConstant,
            _ => Op::Invalid
        }
    }
}

#[test]
fn test_opcode() {
    let code = Op::from(0u8);

    println!("{:#?}", code);
}