use std::collections::HashMap;

pub enum Binding {
    Name,
}

#[derive(Clone, Debug, Default)]
pub struct Context {
    inner: Vec<String>,
}

impl Context {
    pub fn bind(&mut self, hint: String) -> (Context, usize) {
        if self.inner.contains(&hint) {
            self.bind(format!("{}'", hint))
        } else {
            let mut ctx = self.clone();
            let idx = ctx.size();
            ctx.inner.push(hint);
            (ctx, idx)
        }
    }

    pub fn lookup(&self, key: String) -> Option<usize> {
        for (idx, s) in self.inner.iter().enumerate() {
            if key == *s {
                return Some(idx);
            }
        }
        None
    }

    pub fn size(&self) -> usize {
        self.inner.len()
    }
}
