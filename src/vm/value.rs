// Defines value types in the Sprout language, and their representation in memory.

// Defining strings this way (in a Box) is in line with the Crafting Interpreters tutorial, and others,
// but counter to the everything-in-a-database design hypothesis.
use std::fmt;

use crate::compiler::codegen2::Code;
use crate::compiler::expression::Expression;
use crate::utils::format_vec;


// todo: can we put Function/String behind a single u64 or ptr for efficiency
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Null,
    Bool(bool),
    Num(f64),
    Str(String),
    Function(Function),
    Tuple(Vec<Value>),
    Expression(Box<Expression>)
}

impl Value {
    pub fn expression(expr: Expression) -> Self {
        // takes ownership, not ref. is this the right move?
        Value::Expression(Box::new(expr))
    }

    pub fn falsey(&self) -> bool {
        match self {
            Value::Null => true,
            Value::Bool(false) => true,
            Value::Num(0f64) => true,
            _ => false
        }
    }
}

// so equality is same name/arity AND same code exactly ??
#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub code: Code,
    pub arity: usize,
    pub name: String
}


impl Function {
    pub fn new() -> Self {
        Function { code: Vec::new(), arity: 0, name: String::new() }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Null => write!(f, "Null"),
            Value::Bool(x) => write!(f, "{}", x),
            Value::Num(x) => write!(f, "{}", x),
            Value::Str(x) => write!(f, "\"{}\"", x),
            Value::Function(x) => write!(f, "{}", x),
            Value::Tuple(x) => write!(f, "({})", format_vec(x)),
            Value::Expression(x) => write!(f, "`{}`", x)
        }
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "function {}", self.name)
    }
}
