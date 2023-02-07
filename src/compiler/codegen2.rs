use std::fmt;

use super::expression::*;
use super::token::{Token, TokenType};
use super::interpolate::interpolate;

use crate::vm::vm::VM;
use crate::vm::value::Value;
use crate::vm::value::Function;
use crate::vm::opcode::Op;
use crate::vm::symbols::SymbolStore;
use crate::utils::format_vec;


#[derive(Debug)]
pub enum CompileError {
    UnitializedLocal(Token),
    CompileError(String)
}

pub type Code = Vec<Op>;

type CompileResult = Result<Function, CompileError>;
type CodeResult = Result<Code, CompileError>;

#[derive(Debug)]
pub struct LocalVar {
    name: String,        // todo: this should be a token reference, so we can track error info
    depth: i32
}

pub struct Compiler {
    // Sub-vectors represent fn scopes
    locals: Vec<Vec<LocalVar>>,
    scope_depth: i32,
    symbols: SymbolStore,
    pub vm: VM
}

impl Compiler {
    pub fn new(vm: VM) -> Self {
        let mut c = Compiler { locals: vec![Vec::new()], scope_depth: -1, symbols: SymbolStore::new(), vm };
        c.populate_symbols();
        c
    }

    pub fn local_count(&self) -> usize {
        self.locals.len()
    }

    pub fn compile(&mut self, expression: Expression) -> CompileResult {
        let mut code = expression.compile(self)?;

        if self.scope_depth <= 0 {
            code.push(Op::Return);
        }

        // Wrap the whole thing in a toplevel function.
        Ok(Function { code, arity: 0, name: "<script>".to_string() })
    }

    pub fn add_local(&mut self, name: String) -> () {
        // TODO: use depth=-1 to mark uninit
        // this stops undefined vars in their own initializer from shadowing
        let depth = self.scope_depth;
        self.current_scope().push(LocalVar {name, depth})
    }

    pub fn resolve_local(&mut self, name: &String) -> Option<usize> {
        // Given a name, return the index within the locals array
        // of the local by that name, if we have one defined.
        println!("resolve {:?} in {:?}", name, self.locals);
        self.current_scope().iter().rposition(|local| local.name == *name)
    }

    #[inline]
    fn current_scope(&mut self) -> &mut Vec<LocalVar> {
        // Stack of scopes is initialized with one entry at init so should never be empty.
        self.locals.last_mut().unwrap()
    }

    #[inline]
    fn enter_scope(&mut self) -> () {
        self.scope_depth += 1;
    }

    #[inline]
    fn enter_function_scope(&mut self) -> () {
        self.locals.push(Vec::new());
        self.scope_depth += 1;
    }

    #[inline]
    fn exit_scope(&mut self, mut code: Code) -> Code {
        // could just merge into block bc it is only used there, now
        self.scope_depth -= 1;
        let depth = self.scope_depth;
        let scope = self.current_scope();
        // translated from Crafting Interpreters, maybe not idiomatic
        let mut n = 0;
        while !scope.is_empty() {
            if scope[scope.len() - 1].depth <= depth {
                break;
            }
            n += 1;
            scope.pop();
        }
        // TOS is the result of evaluating the last expression in scope. 
        // Below it are any locals. Swap TOS below all the locals, then pop them all,
        // leaving the result on TOS again.
        if n > 0 {
            code.push(Op::Swap(n));
            code.push(Op::Popn(n));
        }
        code
    }

    fn exit_function_scope(&mut self) -> () {
        self.locals.pop();
    }

    fn populate_symbols(&mut self) -> () {
        // We have predefined symbols for key operators, 
        // in addition to those that will be created every time we compile a symbol literal.
        // basically for every TokenType ... feels like this could be a macro
        self.symbols.add("+".to_string());
        self.symbols.add("-".to_string());
        self.symbols.add("*".to_string());
        self.symbols.add("/".to_string());
        self.symbols.add("==".to_string());
        self.symbols.add("!=".to_string());
        self.symbols.add("<=".to_string());
        self.symbols.add(">=".to_string());
        self.symbols.add("<".to_string());
        self.symbols.add(">".to_string());
        self.symbols.add("and".to_string());
        self.symbols.add("or".to_string());
        self.symbols.add("not".to_string());
        self.symbols.add("=".to_string());
        self.symbols.add("let".to_string());
        self.symbols.add("if".to_string());
        self.symbols.add("then".to_string());
        self.symbols.add("else".to_string());
        self.symbols.add("->".to_string());
    }
}

impl fmt::Display for Compiler {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Compiler(depth: {}, locals: {:?})", self.scope_depth, self.locals)
    }
}

// ============================================================================

pub trait Compile {
    fn compile(&self, compiler: &mut Compiler) -> CodeResult;
}

// todo: we need a new way to carry over line/col information into the VM

impl Compile for LiteralExpr {
    fn compile(&self, compiler: &mut Compiler) -> CodeResult {
        let op = match &self.token.typ {
            TokenType::LiteralNull => Op::LoadNull,
            TokenType::LiteralBool(b) => {
                if *b == true {
                    Op::LoadTrue
                } else {
                    Op::LoadFalse
                }
            },
            TokenType::LiteralNum(n) => Op::LoadConstant(Value::Num(*n)),
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
            },

            TokenType::Symbol(s) => {
                let n = compiler.symbols.get_or_add(s.clone());
                Op::LoadConstant(Value::Symbol(n))
            },

            _ => return Err(CompileError::CompileError("Unexpected literal token".to_string()))
        };
        Ok(vec![op])
    }
}

impl Compile for AssignExpr {
    fn compile(&self, compiler: &mut Compiler) -> CodeResult {
        // validate assignment target - currently only a name literal
        // eventually allow a tuple expr for destructuring
        if let Expression::Literal(expr) = &*self.target {
            if let TokenType::Name(name) = &expr.token.typ {
                let mut code = self.value.compile(compiler)?;
                // Assignments at scope depth 0 (top-level) are globals
                // Otherwise, they're treated as locals and simply left on the stack
                if compiler.scope_depth == 0 {
                    code.push(Op::SetGlobal(name.clone()));
                } else {
                    compiler.add_local(name.to_string());
                }
                return Ok(code)
            } else {
                return Err(CompileError::CompileError("Invalid assignment target".to_string()));
            }
        } else {
            return Err(CompileError::CompileError("Invalid assignment target".to_string()));
        }
    }
}

impl Compile for UnaryExpr {
    fn compile(&self, _compiler: &mut Compiler) -> CodeResult {
        let mut code = self.value.compile(_compiler)?;
        let op = match self.token.typ {
            TokenType::Minus => Op::Negate,
            TokenType::Not   => Op::Not,
            TokenType::Return => Op::Return,

            _ => return Err(CompileError::CompileError("Unexpected unary token".to_string()))
        };
        code.push(op);
        Ok(code)
    }
}

impl Compile for BinaryExpr {
    fn compile(&self, _compiler: &mut Compiler) -> CodeResult {
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
            TokenType::In       => Op::In,
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
    fn compile(&self, _compiler: &mut Compiler) -> CodeResult {
        // We can avoid patching jumps here by determining their lengths ahead of time.
        let mut code = self.condition_expr.compile(_compiler)?;

        let true_code = self.true_expr.compile(_compiler)?;
        code.push(Op::JumpIfFalse(true_code.len() + 2)); // skip the true branch + the pop + the next jump
        code.push(Op::Pop);
        code.extend(true_code);

        if let Some(false_branch) = &*self.false_expr {
            let false_code = false_branch.compile(_compiler)?;
            // append an absolute jump to the end of the true branch
            code.push(Op::Jump(false_code.len() + 1));
            code.push(Op::Pop);
            code.extend(false_code);
        } else {
            // implicit "else null" at the end
            code.push(Op::Jump(2));
            code.push(Op::Pop);
            code.push(Op::LoadNull);
        }

        Ok(code)
    }
}

impl Compile for FunctionExpr {
    fn compile(&self, compiler: &mut Compiler) -> CodeResult {
        let arity = self.arguments.len();
        
        // We enter a new scope for the body and define the arguments as local variables available in that scope.
        // BUG: we aren't treating the locals as being at the same depth as the arguments w/ a block
        compiler.enter_function_scope();
        for arg in &self.arguments {
            // issue is that here we force add_local even when at scope depth 0 
            // but when we see a name ref within the block body, we treat it as global if at depth 0
            compiler.add_local(arg.to_string());
        }
        compiler.scope_depth -= 1;

        // if we declare any locals in this block they get treated as at a diff. depth than the args
        // bc we've done enter_scope twice now
        // and so they don't all get popped correctly
        // compiler.scope_depth -= 1;
        let mut code = self.body.compile(compiler)?;
        code.push(Op::Return);
        compiler.exit_function_scope();
        let func = Value::Function(Function { code, arity, name: self.name.clone() });
        
        // The only code that runs *when the function is defined* (not called)
        // is just loading the Function Value onto the stack.
        Ok(vec![Op::LoadConstant(func)])
    }
}

impl Compile for CallExpr {
    fn compile(&self, compiler: &mut Compiler) -> CodeResult {
        let arity = self.arguments.len();
        // Compile the expression for the callee
        let mut code = self.callee.compile(compiler)?;
        // Compile the expressions for the arguments. TODO this could be more rust idiomatic right
        for arg_expr in &self.arguments {
            code.extend(arg_expr.compile(compiler)?);
        }
        code.push(Op::Call(arity));
        Ok(code)
    }
}

// TODO: ReturnExpr should call exit_scope too
// Also debug what happens when you return from a deeply nested scope

impl Compile for TupleExpr {
    fn compile(&self, compiler: &mut Compiler) -> CodeResult {
        let mut code = Vec::new();
        let n = self.items.len();
        for expr in &self.items {
            code.extend(expr.compile(compiler)?);
        }
        code.push(Op::MakeTuple(n));
        Ok(code)
    }
}

impl Compile for ForExpr {
    fn compile(&self, compiler: &mut Compiler) -> CodeResult {
        // Options: 1) Python-style list comp, build an anon fn and call it,
        // 2) add Op::Map etc at the low level

        // First eval the iterable, usually a load name or a literal tuple
        let mut code = self.iterable.compile(compiler)?;

        // Compile the body code. Still in the scope entered above.
        let body_code = self.body.compile(compiler)?;
        let n = body_code.len();
        // Then emit the Op::Iter. We know how long it needs to be bc of the length of body code
        code.push(Op::Iter(n + 1));
        // And then the body ...
        code.extend(body_code);
        // Now TOS is the result of evaluating the body for one iteration.
        // We will pop it and append it to the result tuple being built.
        code.push(Op::TupleAppend(n));
        // At the end of the body is a backward jump to the iter instruction.
        code.push(Op::Loop(n));

        // Make sure to exit the scope.
        code = compiler.exit_scope(code);
        Ok(code)
    }
}

impl Compile for QuotedExpr {
    fn compile(&self, compiler: &mut Compiler) -> CodeResult {
        // TODO: So should we perform interpolation/unquoting in here?
        // let mut subexp = self.subexpr.clone();
        // interpolate(&mut subexp, compiler);
        let val = Value::Expression(self.subexpr.clone());
        let op = Op::LoadConstant(val);
        Ok(vec![op])
    }
}

impl Compile for UnquoteExpr {
    fn compile(&self, compiler: &mut Compiler) -> CodeResult {
        Err(CompileError::CompileError("Shouldn't be compiling an unquote outside a quoted expr".to_string()))
    }
}

impl Compile for MacroCallExpr {
    fn compile(&self, compiler: &mut Compiler) -> CodeResult {
        // Compile code to call the macro with arguments wrapped in Value::Expression,
        let mut code = self.callee.compile(compiler)?;
        let arity = self.arguments.len();
        for arg in &self.arguments {
            code.push(Op::LoadConstant(Value::Expression(Box::new(arg.clone()))));
        }
        code.push(Op::Call(arity));
        code.push(Op::Return);
        println!("EVAL: Will run: {:?}", code);

        // Then run that call immediately at compile time.
        // TODO: the QuotedExpr body of the macro fn still compiles prior to this call
        // so we don't want to do interpolation at "macro-definition" time we want to do it at "macro-invocation" time
        // so unless it is defined as an opcode ... we need to somehow like, jump out of call context into the compiler ??

        // Unless macros can't do calling but can only do interpolation. Or something.

        let func = Function { code, arity: 0, name: "<macro>".to_string() };
        let res = compiler.vm.run(func).unwrap();
        // result should be a Value::Expression. Now compile it in
        let expr = match res {
            Value::Expression(exp) => *exp,
            _ => { return Err(CompileError::CompileError("Expected macro to return an Expression".to_string())); }
        };
        // BY the time we get here, we SHOULD have no more $s left in the resulting expr.
        // The macro has to perform interpolation during its call. 
        let code = expr.compile(compiler)?;
        Ok(code)
    }
}


// impl Compile for UnquoteExpr {
//     fn compile(self, compiler: &mut Compiler) -> CodeResult {
//         // Compile inner expr in the compiler's current scope
//         println!("EVAL: Compile {} in state {}", self.subexpr, compiler);
//         let mut code = self.subexpr.compile(compiler)?;
//         code.push(Op::Return);
//         println!("EVAL: Will run: {:?}", code);
//         let func = Function { code, arity: 0, name: "<comptime>".to_string() };

//         let res = compiler.vm.run(func).unwrap();   // todo error handling
//         // The result of compilation is loading that result value as a constant ??
//         // todo: make sure we actually pull out the inner-expr appropriately?

//         // what we need is not necessarily a round trip via value_to_expr
//         // we want value -> (opcodes which produce that value)??
//         // which we COULD get by turning it into an expr and compiling ... but maybe don't have to
//         println!("EVAL: Compile result: {}", res);
//         let code = match res {
//             Value::Expression(expr) => expr.compile(compiler)?,
//             val @ _ => vec![Op::LoadConstant(val)]
//         };
//         // let code = vec![Op::LoadConstant(res)];
//         Ok(code)
//     }
// }

impl Compile for BlockExpr {
    fn compile(&self, compiler: &mut Compiler) -> CodeResult {        
        compiler.enter_scope();
        let mut code = Vec::new();
        for expr in &self.exprs {
            code.extend(expr.compile(compiler)?);
        }
        code = compiler.exit_scope(code);
        Ok(code)
    }
}

// === Overall impl. Could use a macro here ===

impl Compile for Expression {
    fn compile(&self, compiler: &mut Compiler) -> CodeResult {
        match self {
            Expression::Literal(expr) => expr.compile(compiler),
            Expression::Unary(expr)   => expr.compile(compiler),
            Expression::Binary(expr)  => expr.compile(compiler),
            Expression::Assignment(expr)    => expr.compile(compiler),
            Expression::Conditional(expr)   => expr.compile(compiler),
            Expression::Function(expr)      => expr.compile(compiler),
            Expression::Call(expr)    => expr.compile(compiler),
            Expression::MacroCall(expr) => expr.compile(compiler),
            Expression::Tuple(expr)   => expr.compile(compiler),
            Expression::For_(expr)     => expr.compile(compiler),
            Expression::Block(expr)   => expr.compile(compiler),
            Expression::Quoted(expr)  => expr.compile(compiler),
            Expression::Unquote(expr) => expr.compile(compiler),
            Expression::Return(expr)  => expr.compile(compiler)
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