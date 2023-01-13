use super::expression::*;
use super::token::{Token, TokenType};

use crate::vm::value::Value;
use crate::vm::opcode::Op;


#[derive(Debug)]
pub enum CompileError {
    UnitializedLocal(Token),
    CompileError(String)
}

pub type Code = Vec<Op>;

type CompileResult = Result<Code, CompileError>;

#[derive(Debug)]
pub struct LocalVar {
    name: String,        // todo: this should be a token reference, so we can track error info
    depth: i32
}

impl LocalVar {
//    pub fn name_equals(&self, other: &String) -> bool {
//        if let TokenType::Name(n) = &self.name.typ {
//            n == other
//        } else {
//            false
//        }
//    }
}

pub struct Compiler {
    locals: Vec<LocalVar>,
    scope_depth: i32
}

impl Compiler {
    pub fn new() -> Self {
        Compiler { locals: Vec::new(), scope_depth: -1 }
    }

    pub fn local_count(&self) -> usize {
        self.locals.len()
    }

    pub fn compile(&mut self, statement: Statement) -> CompileResult {
        let mut code = Vec::new();

        match statement {
            Statement::Block(statements) => {
                self.enter_scope();
                for st in statements {
                    code.extend(self.compile(*st)?);
                }
                code = self.exit_scope(code);
            },

            Statement::Expression(expr) => {
                code.extend(expr.compile(self)?);
            }
        }

        // TODO: why was this here? did we get this from CI?
        if self.scope_depth == 0 {
            code.push(Op::Return);
        }
        Ok(code)
    }

    pub fn add_local(&mut self, name: String) -> () {
        self.locals.push(LocalVar {name, depth: self.scope_depth})
    }

    pub fn resolve_local(&self, name: &String) -> Option<usize> {
        // Given a name, return the index within the locals array
        // of the local by that name, if we have one defined.
        println!("resolve {:?} in {:?}", name, self.locals);
        self.locals.iter().rposition(|local| local.name == *name)
    }

    #[inline]
    fn enter_scope(&mut self) -> () {
        self.scope_depth += 1;
    }

    #[inline]
    fn exit_scope(&mut self, mut code: Code) -> Code {
        self.scope_depth -= 1;
        while !self.locals.is_empty() {
            if self.locals[self.locals.len() - 1].depth <= self.scope_depth {
                break;
            }
            code.push(Op::Pop);
            self.locals.pop();
        }
        code
    }
}


trait Compile {
    // Takes ownership of self, an Expression, and destroys it
    fn compile(self, compiler: &mut Compiler) -> CompileResult;
}

// todo: we need a new way to carry over line/col information into the VM

impl Compile for LiteralExpr {
    fn compile(self, compiler: &mut Compiler) -> CompileResult {
        let op = match self.token.typ {
            TokenType::LiteralNull => Op::LoadNull,
            TokenType::LiteralBool(b) => {
                if b == true {
                    Op::LoadTrue
                } else {
                    Op::LoadFalse
                }
            },
            TokenType::LiteralNum(n) => Op::LoadConstant(Value::Num(n)),
            TokenType::LiteralStr(s) => Op::LoadConstant(Value::Str(s.clone())),

            // If we try to read a name, first try to find it in the currently defined locals
            // (symbol table) otherwise check globals. This impl means that trying to read a local
            // in its own initializer (e.g. `let a = a;`) ends up trying to read a global named a
            // (and, if there is one, succeeds in shadowing it) instead of a descriptive error
            TokenType::Name(ref name) => {
                if let Some(idx) = compiler.resolve_local(name) {
                    Op::GetLocal(idx)
                } else {
                    Op::GetGlobal(name.to_string())
                }
            }

            _ => return Err(CompileError::CompileError("Unexpected literal token".to_string()))
        };
        Ok(vec![op])
    }
}

impl Compile for AssignExpr {
    fn compile(self, compiler: &mut Compiler) -> CompileResult {
        let mut code = self.value.compile(compiler)?;
        // Assignments at scope depth 0 (top-level) are globals
        // Otherwise, they're treated as locals and simply left on the stack
        if compiler.scope_depth == 0 {
            code.push(Op::SetGlobal(self.name.clone()));
        } else {
            compiler.add_local(self.name);
        }
        Ok(code)
    }
}

impl Compile for UnaryExpr {
    fn compile(self, _compiler: &mut Compiler) -> CompileResult {
        let mut code = self.value.compile(_compiler)?;
        let op = match self.token.typ {
            TokenType::Minus => Op::Negate,
            TokenType::Not   => Op::Not,

            _ => return Err(CompileError::CompileError("Unexpected unary token".to_string()))
        };
        code.push(op);
        Ok(code)
    }
}

impl Compile for BinaryExpr {
    fn compile(self, _compiler: &mut Compiler) -> CompileResult {
        let mut code = self.left.compile(_compiler)?;
        let right_code = self.right.compile(_compiler)?;
        let op = match self.token.typ {
            TokenType::Plus     => Op::Add,
            TokenType::Minus    => Op::Sub,
            TokenType::Star     => Op::Mul,
            TokenType::Slash    => Op::Div,
            TokenType::Power    => Op::Pow,
            TokenType::Lt       => Op::Lt,
            TokenType::LtEq     => Op::LtEq,
            TokenType::Gt       => Op::Gt,
            TokenType::GtEq     => Op::GtEq,
            TokenType::Eq       => Op::Eq,
            TokenType::NotEq    => Op::NotEq,
            TokenType::And      => {
                // skip the right code if falsey
                code.push(Op::JumpIfFalse(right_code.len() + 1));
                code.push(Op::Pop);
                code.extend(right_code);
                return Ok(code);
            },
            TokenType::Or       => {
                code.push(Op::JumpIfFalse(1));  // skip jump
                code.push(Op::Jump(right_code.len() + 1));  // skip RHS + pop
                code.push(Op::Pop);
                code.extend(right_code);
                return Ok(code);
            }

            _ => return Err(CompileError::CompileError("Unexpected binary token".to_string()))
        };
        code.extend(right_code);
        code.push(op);
        Ok(code)
    }
}

impl Compile for ConditionalExpr {
    fn compile(self, _compiler: &mut Compiler) -> CompileResult {
        // We can avoid patching jumps here by determining their lengths ahead of time.
        let mut code = self.condition_expr.compile(_compiler)?;

        let true_code = self.true_expr.compile(_compiler)?;
        code.push(Op::JumpIfFalse(true_code.len() + 1)); // skip the true branch + the pop
        code.push(Op::Pop);
        code.extend(true_code);

        if let Some(false_branch) = *self.false_expr {
            let false_code = false_branch.compile(_compiler)?;
            // append an absolute jump to the end of the true branch
            code.push(Op::Jump(false_code.len()));
            code.extend(false_code);
        } else {
            // implicit "else null" at the end
            code.push(Op::Jump(1));
            code.push(Op::LoadNull);
        }

        Ok(code)
    }
}

impl Compile for FunctionExpr {
    fn compile(self, _compiler: &mut Compiler) -> CompileResult {
        println!("{:#?}", self);
        todo!("yeah no")
    }
}

impl Compile for TupleExpr {
    fn compile(self, _compiler: &mut Compiler) -> CompileResult {
        todo!("yeah no")
    }
}

impl Compile for Expression {
    fn compile(self, compiler: &mut Compiler) -> CompileResult {
        match self {
            Expression::Literal(expr) => expr.compile(compiler),
            Expression::Unary(expr)   => expr.compile(compiler),
            Expression::Binary(expr)  => expr.compile(compiler),
            Expression::Assignment(expr)    => expr.compile(compiler),
            Expression::Conditional(expr)   => expr.compile(compiler),
            Expression::Function(expr)      => expr.compile(compiler),
            Expression::Tuple(expr)   => expr.compile(compiler)
        }
    }
}


#[cfg(test)]
mod test {
    use crate::compiler::lexer::Lexer;
    use crate::compiler::parser::Parser;
    use super::compile;

    #[test]
    fn test_compile() {
        let input = "let foo = 3 + 4;";
        let ast = Parser::parse(input).unwrap();

        let result = compile(ast);
        println!("{:?}", result);
    }
}