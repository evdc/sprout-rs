use crate::bytecode::Bytecode;
use crate::opcode::Op;
use crate::value::Value;


#[derive(Debug, Clone, PartialEq)]
enum VMError {
    InvalidOpcode(u8)
}

type VMResult = Result<Value, VMError>;

pub struct VM {
    bytecode: Bytecode,
    ip: usize
}

impl VM {
    fn new(bytecode: Bytecode) -> Self {
        VM { bytecode, ip: 0}
    }

    fn run(&mut self) -> VMResult {
        loop {
            let byte = self.read_byte();
            match Op::from_byte(byte) {
                Some(Op::Return) => return Ok(Value::Null),
                _ => return Err(VMError::InvalidOpcode(byte))
            }
        }
    }

    #[inline]
    fn read_byte(&mut self) -> u8 {
        let byte = self.bytecode.code[self.ip];
        self.ip += 1;
        byte
    }
}

#[test]
fn test_vm() {
    let mut code = Bytecode::new();
    code.add_code(0u8, 0);

    let mut vm = VM::new(code);

    let res = vm.run();

    // todo: assert equals Ok(Value::Null)
    println!("{:#?}", res)
}

#[test]
fn test_vm_invalid_op() {
    let mut code = Bytecode::new();
    code.add_code(255u8, 0);
    code.add_code(0u8, 0);

    let mut vm = VM::new(code);

    let res = vm.run();

    // todo: assert equals Err(InvalidOpcode(255))
    println!("{:#?}", res)
}