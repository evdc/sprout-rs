use crate::vm::opcode::Op;
use crate::vm::value::Value;
use crate::vm::value::Function;
use crate::utils::format_vec;

use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum VMError {
    InvalidOpcode(u8),
    StackEmpty,
    TypeError(String),
    UndefinedVariable(String),
    IncorrectBytecode,
    NotCallable(Value),
    // first is expected, second is actual
    WrongNumberArgs(usize, usize)
}

pub type VMResult = Result<Value, VMError>;

#[derive(Debug)]
pub struct CallFrame {
    // This is probably inefficient. But let's see
    pub function: Box<Function>,
    pub ip: usize,
    pub base: usize
}

impl fmt::Display for CallFrame {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<CallFrame {} ip: {} base: {}>", self.function.name, self.ip, self.base)
    }
}

impl CallFrame {
    pub fn new(function: Function, base: usize) -> Self {
        CallFrame { function: Box::new(function), ip: 0, base }
    }
}

pub struct VM {
    stack: Vec<Value>,
    globals: HashMap<String, Value>
}

impl VM {
    pub fn new() -> Self {
        VM { stack: Vec::new(), globals: HashMap::new() }
    }

    pub fn run(&mut self, function: Function) -> VMResult {
        // Takes ownership of a Function object. Function/Code passed to Run will outlive the VM.
        //  (idk if Rust knows this, or how to communicate that)
        // TODO: create a CallFrame with a ref(?) to this Function.
        //  Then modify instructions to work w the current frame instead of VM directly owning ip.

        // Reset stack bewteen invocations
        self.stack = Vec::new();
        self.push(Value::Function(function.clone()));

        // If we put the call stack inside this method then rust doesn't complain about borrowing self ??
        let mut frames = vec![CallFrame::new(function, 1)];
        let mut current_frame = frames.last_mut().unwrap();

        loop {
            let op = current_frame.function.code[current_frame.ip].clone();
            current_frame.ip += 1;

            println!("STACK: {}", format_vec(&self.stack));
            println!("Op: {:?}", op);

            match op {
                Op::Return => { 
                    let result = self.pop().or(Ok(Value::Null));
                    // Below the result is the fn object: pop that too.
                    self.pop()?;
                    // Discard the top frame. If no more frames, then we were at toplevel: exit.
                    frames.pop();
                    if frames.len() == 0 { 
                        return result
                    }
                    // If not: reset current frame, re-push result
                    current_frame = frames.last_mut().unwrap();
                    self.push(result.unwrap()); // known to not be an error
                }

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

                // This leaves the rvalue on the stack, because assigning is an expression
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
                Op::GetLocal(idx) => {
                    let val = self.stack[current_frame.base + idx].clone();
                    self.push(val);
                }
                Op::SetLocal(idx) => {
                    let val = self.peek()?;
                    self.stack[current_frame.base + idx] = val.clone();
                }


                Op::Jump(how_high) => {
                    current_frame.ip += how_high;
                },
                Op::JumpIfFalse(how_high) => {
                    // Do we want to pop - check CI
                    if self.peek()?.falsey() {
                        current_frame.ip += how_high;
                    }
                },
                Op::Loop(n) => { current_frame.ip -= n; }
                Op::Pop    => { let _ = self.pop()?; },

                Op::Call(arity) => {
                    // Value to be called, presumably a Value::Function, is on stack below arguments
                    // todo: remove cloning!
                    let callee = &self.stack[self.stack.len() - arity - 1];
                    match callee {
                        Value::Function(func) => { 
                            if func.arity != arity { return Err(VMError::WrongNumberArgs(func.arity, arity)); }
                            // Do call. Create a new CallFrame
                            let frame = CallFrame::new(func.clone(), self.stack.len() - arity);
                            println!("NEW FRAME: {}", frame);
                            frames.push(frame);
                            current_frame = frames.last_mut().unwrap();
                         },
                        _ => { return Err(VMError::NotCallable(callee.clone())); }
                    };
                },

                Op::MakeTuple(size) => {
                    // Pop n items off the stack and make them into a tuple. 
                    // First element is on the bottom.
                    let idx = self.stack.len() - size;
                    let items = self.stack.drain(idx..).collect();
                    self.push(Value::Tuple(items))
                },
                Op::TupleAppend(n) => {
                    // TOS-n is a tuple. Pop TOS and append it to TOS-n.
                    let val = self.pop()?;
                    let idx = self.stack.len() - n;
                    let target = &mut self.stack[idx];
                    match target {
                        Value::Tuple(ref mut tpl) => tpl.push(val),
                        _ => return Err(VMError::TypeError(format!("Can't append to {}", target)))
                    }
                }

                Op::Iter(size) => {
                    // TOS is an iterable (currently only strings).
                    // If it's nonempty, pop its next value and place it above on TOS.
                    // If it's empty, pop it and jump forward by `size` to skip loop body.
                    let it = self.stack.last_mut().unwrap();
                    match it {
                        Value::Str(s) => {
                            if s.is_empty() {
                                current_frame.ip += size;
                            } else {
                                let new_s = Value::Str(s.remove(0).to_string());
                                self.push(new_s);
                            }
                        },
                        Value::Tuple(t) => {
                            if t.is_empty() {
                                current_frame.ip += size;
                            } else {
                                // this will be inefficient to keep popping from the front
                                let v = t.remove(0);
                                self.push(v);
                            }
                        }
                        _ => { return Err(VMError::TypeError(format!("{} is not iterable", it))); }
                    }
                },

                Op::Swap(size) => {
                    // PANICS if size >= stack.len(), which should not happen with correct codegen.
                    let n = self.stack.len() - 1;
                    self.stack.swap(n, n - size); 
                },
                Op::Popn(size) => {
                    // PANICS if size > stack.len(), which should not happen with correct codegen.
                    self.stack.truncate(self.stack.len() - size);
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