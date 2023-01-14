// Defines value types in the Sprout language, and their representation in memory.

// Defining strings this way (in a Box) is in line with the Crafting Interpreters tutorial, and others,
// but counter to the everything-in-a-database design hypothesis.

use crate::compiler::codegen2::Code;
use crate::compiler::expression::Expression;


// todo: can we put Function/String behind a single u64 or ptr for efficiency
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Null,
    Bool(bool),
    Num(f64),
    Str(String),
    Function(Function),
    Expression(Box<Expression>)
}

impl Value {
    pub fn expression(expr: Expression) -> Self {
        // takes ownership, not ref. is this the right move?
        Value::Expression(Box::new(expr))
    }
}

// so equality is same name/arity AND same code exactly ??
#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    code: Code,
    arity: u8,
    name: String
}


impl Function {
    pub fn new() -> Self {
        Function { code: Vec::new(), arity: 0, name: String::new() }
    }
}


impl Value {
    pub fn falsey(&self) -> bool {
        match self {
            Value::Null => true,
            Value::Bool(false) => true,
            Value::Num(0f64) => true,
            _ => false
        }
    }
}