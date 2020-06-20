use std::collections::{HashMap, HashSet};
pub mod disjoint;
pub mod naive;
pub mod parser;
pub mod types;

use types::*;

#[derive(Debug)]
pub enum Term {
    Unit,
    Bool(bool),
    Int(usize),
    Var(usize, String),
    Abs(Box<Term>),
    App(Box<Term>, Box<Term>),
    Let(Box<Term>, Box<Term>),
    If(Box<Term>, Box<Term>, Box<Term>),
}

#[derive(Debug)]
pub enum TypedTerm {
    Unit,
    Bool(bool),
    Int(usize),
    Var(usize, String),
    Abs(Box<SystemF>),
    App(Box<SystemF>, Box<SystemF>),
    Let(Box<SystemF>, Box<SystemF>),
    If(Box<SystemF>, Box<SystemF>, Box<SystemF>),
}

#[derive(Debug)]
pub struct SystemF {
    expr: TypedTerm,
    ty: Type,
}

pub enum Constraint {
    Eq(Type, Type),
    Inst(Type, Scheme),
    Gen(Type, Vec<TypeVar>, Type),
}

#[derive(Default, Debug)]
struct Elaborator {
    exist: TypeVar,
    context: Vec<Scheme>,
    constraints: Vec<(Type, Type)>,
}

impl SystemF {
    fn new(expr: TypedTerm, ty: Type) -> SystemF {
        SystemF { expr, ty }
    }

    fn de(self) -> (TypedTerm, Type) {
        (self.expr, self.ty)
    }
}

impl Substitution for Elaborator {
    fn ftv(&self) -> HashSet<TypeVar> {
        let mut set = HashSet::new();
        for s in &self.context {
            set.extend(s.ftv());
        }
        set
    }

    fn apply(self, map: &HashMap<TypeVar, Type>) -> Self {
        Elaborator {
            exist: self.exist,
            context: self.context.into_iter().map(|sch| sch.apply(map)).collect(),
            constraints: self.constraints,
        }
    }
}

impl Elaborator {
    fn fresh(&mut self) -> TypeVar {
        let ex = self.exist;
        self.exist.0 += 1;
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
        let set: HashSet<TypeVar> = ty.ftv().difference(&self.ftv()).copied().collect();

        if set.is_empty() {
            Scheme::Mono(ty)
        } else {
            Scheme::Poly(set.into_iter().collect(), ty)
        }
    }

    fn instantiate(&mut self, scheme: Scheme) -> Type {
        match scheme {
            Scheme::Mono(ty) => ty,
            Scheme::Poly(vars, ty) => {
                let freshv: Vec<TypeVar> = (0..vars.len()).map(|_| self.fresh()).collect();
                let map = vars
                    .into_iter()
                    .zip(freshv.iter())
                    .map(|(v, f)| (v, Type::Var(*f)))
                    .collect::<HashMap<TypeVar, Type>>();
                ty.apply(&map)
            }
        }
    }

    fn elaborate(&mut self, term: &Term) -> SystemF {
        // dbg!(term);
        match term {
            Term::Unit => SystemF::new(TypedTerm::Unit, Type::Con(T_UNIT, vec![])),
            Term::Bool(b) => SystemF::new(TypedTerm::Bool(*b), Type::Con(T_BOOL, vec![])),
            Term::Int(i) => SystemF::new(TypedTerm::Int(*i), Type::Con(T_INT, vec![])),
            // x has type T iff T is an instance of the type scheme associated with x
            Term::Var(x, s) => {
                // let fresh = self.fresh();
                let scheme = self.get_scheme(*x).cloned().expect("Unbound variable!");
                let ty = self.instantiate(scheme.clone());
                // println!("{:?} scheme inst {:?} -> {:?}", s, scheme, ty);

                // self.constraints.push((Type::Var(fresh), scheme));
                SystemF::new(TypedTerm::Var(*x, s.clone()), ty)
            }
            // \z. t has type T iff for some X1 and X2:
            //  (i)  under the assumption that z has type X1, t has type X2
            //  (ii) T is a supertype of X1 -> X2
            Term::Abs(body) => {
                let arg = self.fresh();

                self.context.push(Scheme::Mono(Type::Var(arg)));
                let (body, ty) = self.elaborate(body).de();
                self.context.pop();
                let arrow = Type::arrow(Type::Var(arg), ty.clone());
                SystemF::new(TypedTerm::Abs(Box::new(SystemF::new(body, ty))), arrow)
            }
            // t1 t2 has type T iff for some X2, t1 has type X2 -> T and t2 has type X2
            Term::App(t1, t2) => {
                let (t1, ty1) = self.elaborate(t1).de();
                let (t2, ty2) = self.elaborate(t2).de();

                let v = self.fresh();
                self.constraints
                    .push((ty1.clone(), Type::arrow(ty2.clone(), Type::Var(v))));

                SystemF::new(
                    TypedTerm::App(Box::new(SystemF::new(t1, ty1)), Box::new(SystemF::new(t2, ty2))),
                    Type::Var(v),
                )
            }
            Term::Let(t1, t2) => {
                let (t1, ty1) = self.elaborate(t1).de();

                let sub = naive::solve(self.constraints.drain(..)).unwrap();

                // let sub = disjoint::solve(self.constraints.drain(..).collect()).unwrap();

                self.context = self.context.drain(..).map(|sch| sch.apply(&sub)).collect();
                let scheme = self.generalize(ty1.clone().apply(&sub));

                // Add back any leftover constraints
                self.constraints.extend(sub.into_iter().map(|(k, v)| (Type::Var(k), v)));

                self.context.push(scheme);
                let (t2, ty2) = self.elaborate(t2).de();
                self.context.pop();
                SystemF::new(
                    TypedTerm::Let(Box::new(SystemF::new(t1, ty1)), Box::new(SystemF::new(t2, ty2.clone()))),
                    ty2,
                )
            }
            Term::If(t1, t2, t3) => {
                let (t1, ty1) = self.elaborate(t1).de();
                let (t2, ty2) = self.elaborate(t2).de();
                let (t3, ty3) = self.elaborate(t3).de();

                let fresh = self.fresh();
                self.constraints.push((ty1.clone(), Type::bool()));
                self.constraints.push((ty2.clone(), Type::Var(fresh)));
                self.constraints.push((ty3.clone(), Type::Var(fresh)));

                SystemF::new(
                    TypedTerm::If(
                        Box::new(SystemF::new(t1, ty1)),
                        Box::new(SystemF::new(t2, ty2)),
                        Box::new(SystemF::new(t3, ty3)),
                    ),
                    Type::Var(fresh),
                )
            }
        }
    }
}

impl TypedTerm {
    fn subst(self, s: &HashMap<TypeVar, Type>) -> TypedTerm {
        use TypedTerm::*;
        match self {
            Abs(a) => Abs(Box::new(a.subst(s))),
            App(a, b) => App(Box::new(a.subst(s)), Box::new(b.subst(s))),
            Let(a, b) => Let(Box::new(a.subst(s)), Box::new(b.subst(s))),
            If(a, b, c) => If(Box::new(a.subst(s)), Box::new(b.subst(s)), Box::new(c.subst(s))),
            x => x,
        }
    }
}

impl SystemF {
    fn subst(self, s: &HashMap<TypeVar, Type>) -> SystemF {
        SystemF {
            expr: self.expr.subst(s),
            ty: self.ty.apply(s),
        }
    }
}

fn main() {
    use std::io::prelude::*;
    use std::time::{Duration, Instant};

    let input = "fn m. let y = m in let x = y true in x";
    let tm = parser::Parser::new(input).parse_term().unwrap();

    let start = Instant::now();
    let mut gen = Elaborator::default();
    let u = disjoint::Unifier::new();
    let (tm, ty) = gen.elaborate(&u, &tm).de();
    // let sub =  disjoint.solve(gen.constraints);
    let sub = u.solve(gen.constraints.into_iter());
    let end1 = start.elapsed().as_micros();
    println!("{:?} {:?}", end1, sub);
    // loop {
    //     let mut buffer = String::new();
    //     print!("repl: ");
    //     std::io::stdout().flush().unwrap();
    //     std::io::stdin().read_to_string(&mut buffer).unwrap();
    //     let mut gen = Elaborator::default();
    //     match parser::Parser::new(&buffer).parse_term() {
    //         Some(tm) => {
    //             let (tm, ty) = gen.elaborate(&tm).de();

    //             // let mut sub = HashMap::new();
    //             // println!("{:?}", gen.constraints);
    //             // for (a, b) in &gen.constraints {
    //             //     let tmp = unify(a.clone().apply(&sub), b.clone().apply(&sub)).unwrap();
    //             //     sub = compose(tmp, sub);
    //             // }
    //             let sub =  disjoint::solve(gen.constraints.clone());
    //             println!("{:?}", sub);
    //             // println!("tm {:#?} :{:?}", tm, ty);

    //             // println!("tm {:#?} :{:?}", tm.subst(&sub), ty.apply(&sub));

    //             // dbg!(sub);
    //         }
    //         None => println!("parse error!"),
    //     }
    // }
}
