## Currently

- Fix scoping, popping locals, globals etc.
    - because we start compiling with Statement::Block at top level we automatically enter_scope and compile at scope_depth=1 as if doing locals inside a block/function
        - but we only add an Op::Return at the end if we end at scope depth 0 *after* the last exit_scope; so if we init scope_depth to -1 then we don't have an instruction at the end to stop our VM
            - which would be fixed by bounds checking after every op but we would like to not pay that cost, right
- Let Blocks be Expressions which produce their last value
    - change semicolon to a binary infix op that discards its lhs and then evaluates its rhs ??
    - This may fix, or involve changing, the scoping rules as above (?)

- Function Calls
    - Parse infix `(` as a CallExpr
    - Add Op::Call
    - Add ability to push and pop CallFrames
    - Make Op::Return actually return from a function
    - Add Op::Exit to exit the program ??

---

## Eventually

Refactor memory mgmt
- Avoid boxing e.g. in Value::Function (which contains a `Vec<Op>` which is a heap type anyway), use references intelligently
    - may require unsafe: Correct compiled code will never let a CallFrame outlive the Function obj it points to, but incorrect opcodes could, but the Rust compiler ofc can't tell the diff
- Avoid cloning Values all the time
    - If they only carry references this isn't so bad but still
- Use a custom DataStore; Values carry indices into it
    - Simple Scalars (bool, num) should still be direct in the enum, "unboxed" as it were

## Nice-to-have / QoL

- print expressions succinctly (unparse)?
- change Token to carry a Span (line, col, line2, col2) 
- allow multiline in repl?
- allow reading in files


