use std::collections::HashMap;

#[derive(Clone, Default, Debug)]
pub struct Infix {
    precedence: HashMap<String, usize>,
}

impl Infix {
    pub fn insert(&mut self, s: String, prec: usize) {
        self.precedence.insert(s, prec);
    }

    pub fn get(&self, s: &str) -> Option<usize> {
        self.precedence.get(s).copied()
    }
}
