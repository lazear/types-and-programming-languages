use crate::unsafe_arena::Arena;
use std::collections::HashMap;

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Symbol(u32);

pub struct Interner {
    arena: Arena<String>,
    symbols: HashMap<&'static str, Symbol>,
    strings: Vec<&'static str>,
}

impl Interner {
    pub fn with_capacity(n: usize) -> Interner {
        Interner {
            arena: Arena::with_capacity(n),
            symbols: HashMap::with_capacity(n),
            strings: Vec::with_capacity(n),
        }
    }

    pub fn intern(&mut self, s: &str) -> Symbol {
        if let Some(sym) = self.symbols.get(s) {
            return *sym;
        }
        let ptr: &'static String =
            unsafe { std::mem::transmute(self.arena.alloc(String::from(s))) };

        let sym = Symbol(self.strings.len() as u32);
        self.strings.push(ptr);
        self.symbols.insert(ptr, sym);
        sym
    }

    pub fn get(&self, symbol: Symbol) -> Option<&str> {
        self.strings.get(symbol.0 as usize).copied()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn smoke() {
        let mut interner = Interner::with_capacity(32);
        let h = interner.intern("hello");
        let j = interner.intern("fn");
        let i = interner.intern("lambda");
        assert_eq!(interner.get(h), Some("hello"));
        assert_eq!(interner.get(j), Some("fn"));
        assert_eq!(interner.get(i), Some("lambda"));
        assert_eq!(i, Symbol(2));
    }
}
