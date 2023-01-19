use crate::vm::value::Value;

#[derive(Clone, Debug, PartialEq)]
pub enum Op {
    Return,
    Pop,

    LoadConstant(Value),
    LoadNull,
    LoadTrue,
    LoadFalse,

    Negate,
    Add,
    Sub,
    Mul,
    Div,
    Pow,

    Not,

    Lt,
    LtEq,
    Gt,
    GtEq,
    Eq,
    NotEq,

    SetGlobal(String),  // directly stores the name to get/set
    GetGlobal(String),
    GetLocal(usize),
    SetLocal(usize),

    Jump(usize),
    JumpIfFalse(usize),
    Iter(usize),

    Call(usize),
}
