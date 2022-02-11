use crate::vm::opcode::Op;
use crate::compiler::codegen2::Code;
use crate::vm::value::Value;

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum VMError {
    InvalidOpcode(u8),
    StackEmpty,
    TypeError(String),
    UndefinedVariable(String),
    IncorrectBytecode
}

pub type VMResult = Result<Value, VMError>;

pub struct VM {
    code: Code,
    ip: usize,
    stack: Vec<Value>,
    globals: HashMap<String, Value>,
}

impl VM {
    pub fn new() -> Self {
        VM { code: Vec::new(), ip: 0, stack: Vec::new(), globals: HashMap::new() }
    }

    pub fn run(&mut self, code: Code) -> VMResult {
        self.code = code;
        self.ip = 0;

        loop {
            let op = self.code[self.ip].clone();
            self.ip += 1;

            // println!("Op: {:?} Stack: {:#?}", op, self.stack);

            match op {
                Op::Return => return Ok(self.pop()?),

                Op::LoadConstant(val) => self.push(val.clone()),
                Op::LoadFalse => self.push(Value::Bool(false)),
                Op::LoadTrue => self.push(Value::Bool(true)),
                Op::LoadNull => self.push(Value::Null),

                Op::Negate => {
                    match self.pop()? {
                        Value::Num(x) => self.push(Value::Num(-x)),
                        v => return Err(VMError::TypeError(format!("Invalid operand type for -{:?}", v)))
                    }
                },

                Op::Not => {
                    match self.pop()? {
                        Value::Bool(b) => self.push(Value::Bool(!b)),
                        v => return Err(VMError::TypeError(format!("Invalid operand type for `not {:?}`", v)))
                    }
                },

                // these could probably be accomplished with a macro
                Op::Add => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    match (lhs, rhs) {
                        (Value::Num(l), Value::Num(r)) => self.push(Value::Num(l + r)),
                        (Value::Str(l), Value::Str(r)) => {
                            let v = Value::Str(format!("{}{}", l, r));
                            self.push(v);
                        }
                        (l, r) => return Err(VMError::TypeError(format!("Invalid operand types for {:?} + {:?}", l, r)))
                    }
                },

                Op::Sub => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    match (lhs, rhs) {
                        (Value::Num(l), Value::Num(r)) => self.push(Value::Num(l - r)),
                        (l, r) => return Err(VMError::TypeError(format!("Invalid operand types for {:?} - {:?}", l, r)))
                    }
                },

                Op::Mul => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    match (lhs, rhs) {
                        (Value::Num(l), Value::Num(r)) => self.push(Value::Num(l * r)),
                        (l, r) => return Err(VMError::TypeError(format!("Invalid operand types for {:?} * {:?}", l, r)))
                    }
                },

                Op::Div => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    match (lhs, rhs) {
                        (Value::Num(l), Value::Num(r)) => self.push(Value::Num(l / r)),
                        (l, r) => return Err(VMError::TypeError(format!("Invalid operand types for {:?} / {:?}", l, r)))
                    }
                },

                Op::Pow => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    match (lhs, rhs) {
                        (Value::Num(l), Value::Num(r)) => self.push(Value::Num(l.powf(r))),
                        (l, r) => return Err(VMError::TypeError(format!("Invalid operand types for {:?} ^ {:?}", l, r)))
                    }
                },

                Op::Lt  => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    match (lhs, rhs) {
                        (Value::Num(l), Value::Num(r)) => self.push(Value::Bool(l < r)),
                        (l, r) => return Err(VMError::TypeError(format!("Invalid operand types for {:?} ^ {:?}", l, r)))
                    }
                },

                Op::LtEq  => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    match (lhs, rhs) {
                        (Value::Num(l), Value::Num(r)) => self.push(Value::Bool(l <= r)),
                        (l, r) => return Err(VMError::TypeError(format!("Invalid operand types for {:?} ^ {:?}", l, r)))
                    }
                },

                Op::Gt  => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    match (lhs, rhs) {
                        (Value::Num(l), Value::Num(r)) => self.push(Value::Bool(l > r)),
                        (l, r) => return Err(VMError::TypeError(format!("Invalid operand types for {:?} ^ {:?}", l, r)))
                    }
                },

                Op::GtEq  => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    match (lhs, rhs) {
                        (Value::Num(l), Value::Num(r)) => self.push(Value::Bool(l >= r)),
                        (l, r) => return Err(VMError::TypeError(format!("Invalid operand types for {:?} ^ {:?}", l, r)))
                    }
                },

                Op::Eq  => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    match (lhs, rhs) {
                        (Value::Num(l), Value::Num(r)) => self.push(Value::Bool(l == r)),
                        (l, r) => return Err(VMError::TypeError(format!("Invalid operand types for {:?} ^ {:?}", l, r)))
                    }
                },

                Op::NotEq  => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    match (lhs, rhs) {
                        (Value::Num(l), Value::Num(r)) => self.push(Value::Bool(l != r)),
                        (l, r) => return Err(VMError::TypeError(format!("Invalid operand types for {:?} ^ {:?}", l, r)))
                    }
                },

                // Crafting Interpreters unsafely assumes the constant referred to is a string
                // Rust won't let us assume this, so we have to `if let` which may slow us down
                // This may be a justification for unsafe code: we trust the compiler.
                // Anyway: This leaves the rvalue on the stack, because assigning is an expression
                Op::SetGlobal(name) => {
                    let val = self.peek()?;
                    let val = val.clone();
                    self.globals.insert(name.clone(), val);
                },
                Op::GetGlobal(name) => {
                    let val = self.globals.get(&name).ok_or(VMError::UndefinedVariable(name.clone()))?;
                    let val = val.clone();
                    self.push(val);
                },


                Op::Jump(how_high) => {
                    self.ip += how_high;
                }
                Op::JumpIfFalse(how_high) => {
                    if self.pop()? == Value::Bool(false) {
                        self.ip += how_high;
                    }
                }
            }
        }
    }

    // todo:
    // is it faster to use Vec::push() and Vec::pop() or implement it ourselves with a stack ptr?
    #[inline]
    fn push(&mut self, v: Value) -> () {
        self.stack.push(v);
    }

    #[inline]
    fn pop(&mut self) -> VMResult {
        self.stack.pop().ok_or(VMError::StackEmpty)
    }

    #[inline]
    fn peek(&self) -> Result<&Value, VMError> {
        self.stack.last().ok_or(VMError::StackEmpty)
    }
}

//#[test]
//fn test_vm() {
//    let mut code = Bytecode::new();
//
//    // Corresponds to the following source:
//    // 1 + 2 * 3 - 4 / -5
//    // which parses as
//    // (1 + (2 * 3)) - (4 / -5)
//    // which compiles to
//    // LOAD_CONST   4
//    // LOAD_CONST   5
//    // NEGATE
//    // DIV
//    // LOAD_CONST   2
//    // LOAD_CONST   3
//    // MUL
//    // LOAD_CONST   1
//    // ADD
//    // SUB
//    code.add_constant(Value::Num(4.0), 0);
//    code.add_constant(Value::Num(5.0), 0);
//    code.add_byte(Op::Negate, 0);
//    code.add_byte(Op::Div, 0);
//    code.add_constant(Value::Num(2.0), 0);
//    code.add_constant(Value::Num(3.0), 0);
//    code.add_byte(Op::Mul, 0);
//    code.add_constant(Value::Num(1.0), 0);
//    code.add_byte(Op::Add, 0);
//    code.add_byte(Op::Sub, 0);
//
//    code.add_byte(Op::Return, 0);
//
//    let vm = VM::new(code);
//
//    let res = vm.run();
//
//    println!("{:#?}", res)
//}
//
//#[test]
//fn test_vm_invalid_op() {
//    let mut code = Bytecode::new();
//    code.add_byte(Op::from(255), 0);
//    code.add_byte(Op::Return, 0);
//
//    let vm = VM::new(code);
//
//    let res = vm.run();
//
//    // todo: assert equals Err(InvalidOpcode(255))
//    println!("{:#?}", res)
//}
//
//#[test]
//fn test_vm_type_error() {
//    let mut code = Bytecode::new();
//    code.add_constant(Value::Num(42.0), 0);
//    code.add_byte(Op::LoadNull, 0);
//    code.add_byte(Op::Add, 0);
//
//    let vm = VM::new(code);
//
//    let res = vm.run();
//
//    println!("{:#?}", res)
//}