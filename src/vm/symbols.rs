use std::collections::HashMap;

// could have the forward array (symbols) own Strings and the inverse stores references into them
// or the other way round, but then we need lifetimes on this which infect the VM -> Compiler -> every compile fn

pub struct SymbolStore {
    next: u32,
    symbols: Vec<String>,
    inverse: HashMap<String, u32>
}

impl SymbolStore {
    pub fn new() -> Self {
        SymbolStore { next: 0, symbols: Vec::new(), inverse: HashMap::new() }
    }

    pub fn add(&mut self, name: String) -> u32 {
        let idx = self.next;
        self.symbols.push(name.clone());
        self.inverse.insert(name, idx);
        self.next += 1;
        idx
    }

    pub fn get(&self, idx: u32) -> String {
        self.symbols[idx as usize].clone()
    }

    pub fn get_inv(&self, name: &String) -> Option<&u32> {
        self.inverse.get(name)
    }

    pub fn get_or_add(&mut self, name: String) -> u32 {
        match self.inverse.get(&name) {
            Some(n) => *n,
            None => self.add(name)
        }
    }
}