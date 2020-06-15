pub mod parser;
pub const T_ARROW: Tycon = Tycon { id: 0, arity: 2 };
pub const T_INT: Tycon = Tycon { id: 1, arity: 0 };
pub const T_UNIT: Tycon = Tycon { id: 2, arity: 0 };
pub const T_BOOL: Tycon = Tycon { id: 3, arity: 0 };

#[derive(Copy, Clone)]
pub struct Tycon {
    id: usize,
    arity: usize,
}

#[derive(Clone)]
pub enum Type {
    Var(usize),
    Con(Tycon, Vec<Type>),
}

impl std::fmt::Debug for Tycon {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.id {
            0 => write!(f, "->"),
            1 => write!(f, "int"),
            2 => write!(f, "unit"),
            3 => write!(f, "bool"),
            _ => write!(f, "??"),
        }
    }
}

fn fresh_name(x: usize) -> String {
    let last = ((x % 26) as u8 + 'a' as u8) as char;
    (0..x / 26)
        .map(|_| 'z')
        .chain(std::iter::once(last))
        .collect::<String>()
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Type::Var(x) => write!(f, "{}", fresh_name(*x)),
            Type::Con(tc, tys) => write!(
                f,
                "{:?} {}",
                tc,
                tys.iter()
                    .map(|ty| format!("{:?}", ty))
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
        }
    }
}

impl Type {
    fn arrow(a: Type, b: Type) -> Type {
        Type::Con(T_ARROW, vec![a, b])
    }

    fn bool() -> Type {
        Type::Con(T_BOOL, vec![])
    }
}

#[derive(Debug)]

pub enum Term {
    Unit,
    Int(usize),
    Var(usize),
    Abs(Box<Term>),
    App(Box<Term>, Box<Term>),
    If(Box<Term>, Box<Term>, Box<Term>),
    Let(Box<Term>, Box<Term>),
}

#[derive(Debug)]
pub enum SystemF {
    Unit,
    Int(usize),
    Var(usize),
    Abs(Box<Type>, Box<SystemF>),
    App(Box<SystemF>, Box<SystemF>),
}

#[derive(Debug)]
pub struct Scheme(Vec<Type>, Box<Constraint>, Type);

#[derive(Debug)]
pub enum Constraint {
    True,
    False,
    Subtype(Type, Type),
    And(Box<Constraint>, Box<Constraint>),
    Exist(usize, Box<Constraint>),
    // x has type T iff T is an instance of the type scheme associated with x
    Inst(usize, Type),
    //
    Let(Scheme, Box<Constraint>),
    Def(Type, Box<Constraint>),
}

pub enum Unification {
    True,
    False,
    And(Box<Unification>, Box<Unification>),
    Exist(usize, Box<Unification>),
    Multi(Vec<Type>),
}

#[derive(Default, Debug)]
struct Generator {
    exist: usize,
}

impl Generator {
    fn fresh(&mut self) -> usize {
        let ex = self.exist;
        self.exist += 1;
        ex
    }

    fn generate(&mut self, term: &Term, ty: Type) -> Constraint {
        use Constraint::*;
        match term {
            Term::Unit => Subtype(Type::Con(T_UNIT, vec![]), ty),
            Term::Int(_) => Subtype(Type::Con(T_INT, vec![]), ty),
            // x has type T iff T is an instance of the type scheme associated with x
            Term::Var(x) => Inst(*x, ty),
            // \z. t has type T iff for some X1 and X2:
            //  (i)  under the assumption that z has type X1, t has type X2
            //  (ii) T is a supertype of X1 -> X2
            Term::Abs(body) => {
                let arg = self.fresh();
                let ret = self.fresh();
                let inner = And(
                    // Box::new(Let(Scheme(vec![], Box::new(Constraint::True), Type::Var(arg)), Box::new(self.generate(body, Type::Var(ret))))),
                    Box::new(Constraint::Def(
                        Type::Var(arg),
                        Box::new(self.generate(body, Type::Var(ret))),
                    )),
                    Box::new(Subtype(Type::arrow(Type::Var(arg), Type::Var(ret)), ty)),
                );
                Constraint::Exist(arg, Box::new(Constraint::Exist(ret, Box::new(inner))))
            }
            // t1 t2 has type T iff for some X2, t1 has type X2 -> T and t2 has type X2
            Term::App(t1, t2) => {
                let x2 = self.fresh();
                let arr = Type::arrow(Type::Var(x2), ty);

                let a = self.generate(t1, arr);
                let b = self.generate(t2, Type::Var(x2));
                And(Box::new(a), Box::new(b))
            }
            Term::If(t1, t2, t3) => {
                let c1 = self.generate(t1, Type::bool());
                let c2 = self.generate(t2, ty.clone());
                let c3 = self.generate(t3, ty);
                And(Box::new(c1), Box::new(And(Box::new(c2), Box::new(c3))))
            }
            Term::Let(t1, t2) => {
                let x = self.fresh();
                let ct1 = self.generate(t1, Type::Var(x));
                let ct2 = self.generate(t2, ty);
                Let(Scheme(vec![Type::Var(x)], Box::new(ct1), Type::Var(x)), Box::new(ct2))
            }
        }
    }
}

// impl std::fmt::Debug for Constraint {
//     fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
//         use Constraint::*;
//         match self {
//             True => write!(f, "True"),
//             False => write!(f, "False"),
//             Subtype(a, b) => write!(f, "{:?} < {:?}", a, b),

//         }
//     }
// }

fn main() {
    use std::io::prelude::*;

    loop {
        let mut buffer = String::new();
        print!("repl: ");
        std::io::stdout().flush().unwrap();
        std::io::stdin().read_to_string(&mut buffer).unwrap();
        let mut gen = Generator::default();
        match parser::Parser::new(&buffer).parse_term() {
            Some(tm) => {
                dbg!(gen.generate(&tm, Type::Var(25)));
            }
            None => println!("parse error!"),
        }
    }
}
