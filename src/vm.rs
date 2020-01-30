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
enum VMError {
    InvalidOpcode(u8)
}

type VMResult = Result<Value, VMError>;

pub struct VM {
    bytecode: Bytecode,
    ip: usize,
    stack: Vec<Value>,
    sp: usize
}

impl VM {
    // Takes ownership of a Bytecode.
    fn new(bytecode: Bytecode) -> Self {
        VM { bytecode, ip: 0, stack: Vec::new(), sp: 0}
    }

    fn run(mut self) -> VMResult {
        loop {
            let byte = self.read_byte();
            let op = Op::from(byte);
            trace!(op);
            trace!(stack);

            match op {
                Op::Return => return Ok(self.pop().unwrap_or(Value::Null)),

                Op::LoadConstant => {
                    let v = self.read_constant();
                    self.push(v);
                }

                Op::Invalid => return Err(VMError::InvalidOpcode(byte))
            }
        }
    }

    #[inline]
    fn push(&mut self, v: Value) -> () {
        self.stack.push(v);
        self.sp += 1;
    }

    #[inline]
    fn pop(&mut self) -> Option<Value> {
        self.sp -= 1;
        self.stack.pop()
    }

    #[inline]
    fn read_byte(&mut self) -> u8 {
        let byte = self.bytecode.code[self.ip];
        self.ip += 1;
        byte
    }

    #[inline]
    fn read_constant(&mut self) -> Value {
        let idx = self.read_byte() as usize;
        self.bytecode.constants[idx]        // can avoid clone because Values are Copy
    }
}

#[test]
fn test_vm() {
    let mut code = Bytecode::new();
    code.add_constant(Value::True, 0);
    code.add_code(Op::Return as u8, 0);

    let vm = VM::new(code);

    let res = vm.run();

    println!("{:#?}", res)
}

#[test]
fn test_vm_invalid_op() {
    let mut code = Bytecode::new();
    code.add_code(255u8, 0);
    code.add_code(0u8, 0);

    let vm = VM::new(code);

    let res = vm.run();

    // todo: assert equals Err(InvalidOpcode(255))
    println!("{:#?}", res)
}