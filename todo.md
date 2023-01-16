- print expressions succinctly (unparse)?
- change Token to carry a Span (line, col, line2, col2) 
- allow multiline in repl?
- allow reading in files
- allow arbitrary expression as assignment target/lhs
    - require them to be a Name, for now
    - later allow them to be a Tuple, once those exist
- functions
    - call frames
    - make sure to not use the native stack to stack run calls; cf Stackless Python

---

