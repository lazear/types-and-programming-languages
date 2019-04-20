use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Term {
    TmVar(usize),
    TmAbs(Box<Term>),
    TmApp(Box<Term>, Box<Term>)
}

#[derive(Default)]
struct Context {
    map: Vec<String>,
    uniq: u32,
}

enum Binding {
    NameBind,
}

impl Context {
    pub fn gensym(&mut self) -> char {
        let c = 'a' as u32;
        let ch = std::char::from_u32(c + self.uniq % 26).unwrap_or('x');
        self.uniq += 1;
        ch
    }
    
    pub fn bind<S: Into<String>>(&mut self, name: S) -> Term {
        let i = self.map.len();
        self.map.push(name.into());
        Term::TmVar(i)
    }

    pub fn fmt_tm(&mut self, term: &Term) -> String {
        match term {
            Term::TmAbs(t) => format!("(lambda {}. {})", self.gensym(), self.fmt_tm(t)),
            Term::TmApp(rator, rand) => format!("({} {})", self.fmt_tm(rator), self.fmt_tm(rand)),
            Term::TmVar(x) => self.map.get(*x).cloned().unwrap_or(String::from("[bad index]"))
        }
    }
}


fn main() {
    println!("Hello, world!");

    let mut ctx = Context::default();
    let tm1 = Term::TmApp(Box::new(Term::TmAbs(Box::new(ctx.bind("x")))), Box::new(ctx.bind("y")) );

    print!("{}", ctx.fmt_tm(&tm1));
}
