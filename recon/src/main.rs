use std::collections::{HashMap, HashSet};

pub mod parser;
pub const T_ARROW: Tycon = Tycon { id: 0, arity: 2 };
pub const T_INT: Tycon = Tycon { id: 1, arity: 0 };
pub const T_UNIT: Tycon = Tycon { id: 2, arity: 0 };
pub const T_BOOL: Tycon = Tycon { id: 3, arity: 0 };

pub type Subst = HashMap<usize, Type>;

#[derive(Copy, Clone, PartialEq, PartialOrd, Eq)]
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

    fn occurs(&self, exist: usize) -> bool {
        match self {
            Type::Var(x) => *x == exist,
            Type::Con(_, tys) => tys.iter().any(|ty| ty.occurs(exist)),
        }
    }

    fn de_arrow(&self) -> (&Type, &Type) {
        match self {
            Type::Con(T_ARROW, v) => (&v[0], &v[1]),
            _ => panic!("Not arrow type! {:?}", self),
        }
    }

    fn ftv(&self, v: &mut Vec<usize>) {
        match &self {
            Type::Var(x) => {
                v.push(*x);
            }
            Type::Con(_, tys) => {
                for ty in tys {
                    ty.ftv(v);
                }
            }
        }
    }

    fn subst(self, map: &Subst) -> Type {
        match self {
            Type::Var(x) => map.get(&x).cloned().unwrap_or(Type::Var(x)),
            Type::Con(tc, vars) => Type::Con(tc, vars.into_iter().map(|ty| ty.subst(map)).collect()),
        }
    }
}

#[derive(Debug)]
pub enum Term {
    Unit,
    Int(usize),
    Var(usize),
    Abs(Box<Term>),
    App(Box<Term>, Box<Term>),
    Let(Box<Term>, Box<Term>),
}

#[derive(Debug)]
pub enum SystemF {
    Unit,
    Int(usize),
    Var(usize),
    Abs(Box<Type>, Box<SystemF>),
    App(Box<SystemF>, Box<SystemF>),
    Let(Box<SystemF>, Box<SystemF>),
}

#[derive(Debug, Clone)]
pub enum Scheme {
    Mono(Type),
    Poly(Vec<usize>, Type),
}

// impl Scheme {
//     pub fn instantiate(&self) -> Type {
//         match self {
//             Scheme::Mono(ty) => ty.clone(),
//             Scheme::Poly(_, ty) => ty.clone(),
//         }
//     }
// }

fn compose(s1: Subst, s2: Subst) -> Subst {
    let mut s2 = s2.into_iter().map(|(k, v)| (k, v.subst(&s1))).collect::<Subst>();
    for (k, v) in s1 {
        if !s2.contains_key(&k) {
            s2.insert(k, v);
        }
    }
    s2
}

fn var_bind(var: usize, ty: Type) -> Subst {
    if ty.occurs(var) {
        panic!("cyclic type!");
    }
    let mut sub = HashMap::new();
    match ty {
        Type::Var(x) if x == var => {}
        _ => {
            sub.insert(var, ty);
        }
    }
    sub
}

pub fn unify(a: Type, b: Type) -> Subst {
    println!("unify {:?} {:?}", a, b);
    match (a, b) {
        (Type::Con(a, a_args), Type::Con(b, b_args)) => {
            let mut map = HashMap::new();
            if a_args.len() == b_args.len() && a == b {
                for (a, b) in a_args.into_iter().zip(b_args.into_iter()) {
                    let tmp = unify(a.subst(&map), b.subst(&map));
                    map = compose(map, tmp);
                }
            }
            map
        }
        (Type::Var(tv), b) => var_bind(tv, b),
        (a, Type::Var(tv)) => var_bind(tv, a),
    }
}

#[derive(Default, Debug)]
struct Elaborator {
    exist: usize,
    context: Vec<Scheme>,
    constraints: Vec<(Type, Type)>,
}

impl Elaborator {
    fn fresh(&mut self) -> usize {
        let ex = self.exist;
        self.exist += 1;
        ex
    }

    fn get_scheme(&self, index: usize) -> Option<&Scheme> {
        for (idx, scheme) in self.context.iter().rev().enumerate() {
            if idx == index {
                return Some(scheme);
            }
        }
        None
    }

    fn generalize(&mut self, ty: Type) -> Scheme {
        let mut set = HashSet::new();
        let mut vec = Vec::new();
        ty.ftv(&mut vec);
        let vec: Vec<usize> = vec.into_iter().filter(|item| set.insert(*item)).collect();

        if set.is_empty() {
            Scheme::Mono(ty)
        } else {
            let freshv: Vec<usize> = (0..vec.len()).map(|_| self.fresh()).collect();
            let map = vec
                .into_iter()
                .zip(freshv.iter())
                .map(|(v, f)| (v, Type::Var(*f)))
                .collect::<HashMap<usize, Type>>();
            Scheme::Poly(freshv, ty.subst(&map))
        }
    }

    fn instantiate(&mut self, scheme: Scheme) -> Type {
        match scheme {
            Scheme::Mono(ty) => ty,
            Scheme::Poly(vars, ty) => {
                let freshv: Vec<usize> = (0..vars.len()).map(|_| self.fresh()).collect();
                let map = vars
                    .into_iter()
                    .zip(freshv.iter())
                    .map(|(v, f)| (v, Type::Var(*f)))
                    .collect::<HashMap<usize, Type>>();
                ty.subst(&map)
            }
        }
    }

    fn elaborate(&mut self, term: &Term) -> (SystemF, Type) {
        match term {
            Term::Unit => (SystemF::Unit, Type::Con(T_UNIT, vec![])),
            Term::Int(i) => (SystemF::Int(*i), Type::Con(T_INT, vec![])),
            // x has type T iff T is an instance of the type scheme associated with x
            Term::Var(x) => {
                // let fresh = self.fresh();
                let scheme = self.get_scheme(*x).cloned().expect("Unbound variable!");
                let ty = self.instantiate(scheme);

                // self.constraints.push((Type::Var(fresh), scheme));
                (SystemF::Var(*x), ty)
            }
            // \z. t has type T iff for some X1 and X2:
            //  (i)  under the assumption that z has type X1, t has type X2
            //  (ii) T is a supertype of X1 -> X2
            Term::Abs(body) => {
                let arg = self.fresh();

                self.context.push(Scheme::Mono(Type::Var(arg)));
                let (body, ty) = self.elaborate(body);
                self.context.pop();
                let arrow = Type::arrow(Type::Var(arg), ty);
                (SystemF::Abs(Box::new(Type::Var(arg)), Box::new(body)), arrow)
            }
            // t1 t2 has type T iff for some X2, t1 has type X2 -> T and t2 has type X2
            Term::App(t1, t2) => {
                let (t1, ty1) = self.elaborate(t1);
                let (t2, ty2) = self.elaborate(t2);

                // let (ty11, ty12) = ty1.de_arrow();

                let a = self.fresh();
                let b = self.fresh();
                let arr = Type::arrow(Type::Var(a), Type::Var(b));
                self.constraints.push((ty1, arr));
                // self.constraints.push((ty11.clone(), ty2));
                // (SystemF::App(Box::new(t1), Box::new(t2)), ty12.clone())
                self.constraints.push((Type::Var(a), ty2));
                (SystemF::App(Box::new(t1), Box::new(t2)), Type::Var(b))
            }
            Term::Let(t1, t2) => {
                let (t1, ty1) = self.elaborate(t1);
                let scheme = self.generalize(ty1);
                dbg!(&scheme);
                self.context.push(scheme);
                let (t2, ty2) = self.elaborate(t2);
                self.context.pop();
                (SystemF::Let(Box::new(t1), Box::new(t2)), ty2)

                // Let(Scheme(vec![x], Box::new(ct1), Type::Var(x)), Box::new(ct2))
            }
        }
    }
}

fn main() {
    use std::io::prelude::*;

    loop {
        let mut buffer = String::new();
        print!("repl: ");
        std::io::stdout().flush().unwrap();
        std::io::stdin().read_to_string(&mut buffer).unwrap();
        let mut gen = Elaborator::default();
        match parser::Parser::new(&buffer).parse_term() {
            Some(tm) => {
                let c = gen.elaborate(&tm);
                dbg!(&c);

                let mut sub = HashMap::new();
                println!("{:?}", gen.constraints);
                for (a, b) in &gen.constraints {
                    let tmp = unify(a.clone().subst(&sub), b.clone().subst(&sub));
                    sub = compose(sub, tmp);
                }

                dbg!(sub);
            }
            None => println!("parse error!"),
        }
    }
}
