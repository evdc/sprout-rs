use crate::bytecode::Bytecode;
use crate::opcode::Op;
use crate::value::Value;

// The debug version
#[cfg(feature = "trace_exec")]
macro_rules! trace {
    ($( $args:expr ),*) => { println!( $( $args ),* ); }
}

// Non-debug version
#[cfg(not(feature = "trace_exec"))]
macro_rules! trace {
    ($( $args:expr ),*) => {}
}


#[derive(Debug, Clone, PartialEq)]
pub enum VMError {
    InvalidOpcode(u8),
    StackEmpty,
    TypeError(String)
}

pub type VMResult = Result<Value, VMError>;

pub struct VM {
    bytecode: Bytecode,
    ip: usize,
    stack: Vec<Value>
}

impl VM {
    // Takes ownership of a Bytecode.
    pub fn new(bytecode: Bytecode) -> Self {
        VM { bytecode, ip: 0, stack: Vec::new() }
    }

    pub fn run(mut self) -> VMResult {
        loop {
            let byte = self.read_byte();
            let op = Op::from(byte);
            trace!(op);
            trace!(stack);

            match op {
                Op::Return => return Ok(self.pop()?),

                Op::LoadConstant => {
                    let v = self.read_constant();
                    self.push(v)
                },
                Op::LoadFalse => self.push(Value::Bool(false)),
                Op::LoadTrue => self.push(Value::Bool(true)),
                Op::LoadNull => self.push(Value::Null),

                Op::Negate => {
                    match self.pop()? {
                        Value::Num(x) => self.push(Value::Num(-x)),
                        v @ _ => return Err(VMError::TypeError(format!("Invalid operand type for -{:?}", v)))
                    }
                },

                Op::Not => {
                    match self.pop()? {
                        Value::Bool(b) => self.push(Value::Bool(!b)),
                        v @ _ => return Err(VMError::TypeError(format!("Invalid operand type for `not {:?}`", v)))
                    }
                },

                // these could probably be accomplished with a macro
                Op::Add => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    match (lhs, rhs) {
                        (Value::Num(l), Value::Num(r)) => self.push(Value::Num(l + r)),
                        (l, r) => return Err(VMError::TypeError(format!("Invalid operand types for {:?} + {:?}", l, r)))
                    }
                }

                Op::Sub => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    match (lhs, rhs) {
                        (Value::Num(l), Value::Num(r)) => self.push(Value::Num(l - r)),
                        (l, r) => return Err(VMError::TypeError(format!("Invalid operand types for {:?} - {:?}", l, r)))
                    }
                }

                Op::Mul => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    match (lhs, rhs) {
                        (Value::Num(l), Value::Num(r)) => self.push(Value::Num(l * r)),
                        (l, r) => return Err(VMError::TypeError(format!("Invalid operand types for {:?} * {:?}", l, r)))
                    }
                }

                Op::Div => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    match (lhs, rhs) {
                        (Value::Num(l), Value::Num(r)) => self.push(Value::Num(l / r)),
                        (l, r) => return Err(VMError::TypeError(format!("Invalid operand types for {:?} / {:?}", l, r)))
                    }
                }

                Op::Pow => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    match (lhs, rhs) {
                        (Value::Num(l), Value::Num(r)) => self.push(Value::Num(l.powf(r))),
                        (l, r) => return Err(VMError::TypeError(format!("Invalid operand types for {:?} ^ {:?}", l, r)))
                    }
                }

                Op::Lt  => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    match (lhs, rhs) {
                        (Value::Num(l), Value::Num(r)) => self.push(Value::Bool(l < r)),
                        (l, r) => return Err(VMError::TypeError(format!("Invalid operand types for {:?} ^ {:?}", l, r)))
                    }
                }

                Op::LtEq  => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    match (lhs, rhs) {
                        (Value::Num(l), Value::Num(r)) => self.push(Value::Bool(l <= r)),
                        (l, r) => return Err(VMError::TypeError(format!("Invalid operand types for {:?} ^ {:?}", l, r)))
                    }
                }

                Op::Gt  => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    match (lhs, rhs) {
                        (Value::Num(l), Value::Num(r)) => self.push(Value::Bool(l > r)),
                        (l, r) => return Err(VMError::TypeError(format!("Invalid operand types for {:?} ^ {:?}", l, r)))
                    }
                }

                Op::GtEq  => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    match (lhs, rhs) {
                        (Value::Num(l), Value::Num(r)) => self.push(Value::Bool(l >= r)),
                        (l, r) => return Err(VMError::TypeError(format!("Invalid operand types for {:?} ^ {:?}", l, r)))
                    }
                }

                Op::Eq  => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    match (lhs, rhs) {
                        (Value::Num(l), Value::Num(r)) => self.push(Value::Bool(l == r)),
                        (l, r) => return Err(VMError::TypeError(format!("Invalid operand types for {:?} ^ {:?}", l, r)))
                    }
                }

                Op::NotEq  => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    match (lhs, rhs) {
                        (Value::Num(l), Value::Num(r)) => self.push(Value::Bool(l != r)),
                        (l, r) => return Err(VMError::TypeError(format!("Invalid operand types for {:?} ^ {:?}", l, r)))
                    }
                }

                Op::Invalid => return Err(VMError::InvalidOpcode(byte))
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
    fn read_byte(&mut self) -> u8 {
        let byte = self.bytecode.code[self.ip];
        self.ip += 1;
        byte
    }

    #[inline]
    fn read_constant(&mut self) -> Value {
        // todo: support more than 255 constants
        let idx = self.read_byte() as usize;
        self.bytecode.constants[idx]        // can avoid clone because Values are Copy
    }
}

#[test]
fn test_vm() {
    let mut code = Bytecode::new();

    // Corresponds to the following source:
    // 1 + 2 * 3 - 4 / -5
    // which parses as
    // (1 + (2 * 3)) - (4 / -5)
    // which compiles to
    // LOAD_CONST   4
    // LOAD_CONST   5
    // NEGATE
    // DIV
    // LOAD_CONST   2
    // LOAD_CONST   3
    // MUL
    // LOAD_CONST   1
    // ADD
    // SUB
    code.add_constant(Value::Num(4.0), 0);
    code.add_constant(Value::Num(5.0), 0);
    code.add_code(Op::Negate, 0);
    code.add_code(Op::Div, 0);
    code.add_constant(Value::Num(2.0), 0);
    code.add_constant(Value::Num(3.0), 0);
    code.add_code(Op::Mul, 0);
    code.add_constant(Value::Num(1.0), 0);
    code.add_code(Op::Add, 0);
    code.add_code(Op::Sub, 0);

    code.add_code(Op::Return, 0);

    let vm = VM::new(code);

    let res = vm.run();

    println!("{:#?}", res)
}

#[test]
fn test_vm_invalid_op() {
    let mut code = Bytecode::new();
    code.add_code(Op::from(255), 0);
    code.add_code(Op::Return, 0);

    let vm = VM::new(code);

    let res = vm.run();

    // todo: assert equals Err(InvalidOpcode(255))
    println!("{:#?}", res)
}

#[test]
fn test_vm_type_error() {
    let mut code = Bytecode::new();
    code.add_constant(Value::Num(42.0), 0);
    code.add_code(Op::LoadNull, 0);
    code.add_code(Op::Add, 0);

    let vm = VM::new(code);

    let res = vm.run();

    println!("{:#?}", res)
}