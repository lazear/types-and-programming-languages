use std::collections::{HashMap, HashSet};
pub mod disjoint;
pub mod mutation;
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
pub struct SystemF<T = Type> {
    expr: TypedTerm,
    ty: T,
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

    uni: disjoint::Unifier,
}

impl SystemF {
    fn new(expr: TypedTerm, ty: Type) -> SystemF {
        SystemF { expr, ty }
    }

    fn de(self) -> (TypedTerm, Type) {
        (self.expr, self.ty)
    }
}

impl Elaborator {
    fn fresh(&mut self) -> TypeVar {
        let ex = self.exist;
        self.exist.0 += 1;
        ex
    }

    fn ftv(&self) -> HashSet<TypeVar> {
        let mut set = HashSet::new();
        for s in &self.context {
            set.extend(s.ftv());
        }
        set
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

    fn push(&mut self, ty: (Type, Type)) {
        let a = self.uni.intern(ty.0);
        let b = self.uni.intern(ty.1);
        self.uni.unify(a, b).unwrap();
    }

    fn elaborate(&mut self, term: &Term) -> SystemF {
        // dbg!(term);
        match term {
            Term::Unit => SystemF::new(TypedTerm::Unit, Type::Con(T_UNIT, vec![])),
            Term::Bool(b) => SystemF::new(TypedTerm::Bool(*b), Type::Con(T_BOOL, vec![])),
            Term::Int(i) => SystemF::new(TypedTerm::Int(*i), Type::Con(T_INT, vec![])),
            // x has type T iff T is an instance of the type scheme associated with x
            Term::Var(x, s) => {
                let scheme = self.get_scheme(*x).cloned().expect("Unbound variable!");
                let ty = self.instantiate(scheme.clone());
                SystemF::new(TypedTerm::Var(*x, s.clone()), ty)
            }

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
                self.push((ty1.clone(), Type::arrow(ty2.clone(), Type::Var(v))));

                SystemF::new(
                    TypedTerm::App(Box::new(SystemF::new(t1, ty1)), Box::new(SystemF::new(t2, ty2))),
                    Type::Var(v),
                )
            }
            Term::Let(t1, t2) => {
                let (t1, ty1) = self.elaborate(t1).de();

                // let sub = disjoint::solve(self.constraints.drain(..)).unwrap();
                // for (a, b) in self.constraints.drain(..) {
                //     let a = self.uni.intern(a);
                //     let b = self.uni.intern(b);
                //     self.uni.unify(a, b).unwrap();
                // }

                let sub = self.uni.subst();
                self.context = self.context.drain(..).map(|sch| sch.apply(&sub)).collect();
                let scheme = self.generalize(ty1.clone().apply(&sub));

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
                self.push((ty1.clone(), Type::bool()));
                self.push((ty2.clone(), Type::Var(fresh)));
                self.push((ty3.clone(), Type::Var(fresh)));

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
    let input = "
    let id = fn x. x in 
        let g = id id in 
        let f = id true in 
        let h = (id id) 1 in 
        let j = id 10 in 
        g f";
    let tm = parser::Parser::new(input).parse_term().unwrap();

    let start = Instant::now();
    let mut gen = mutation::Elaborator::default();
    let tm = gen.elaborate(&tm);
    // let sub = gen.uni.subst();
    // let sub =  disjoint.solve(gen.constraints);
    // let sub = disjoint::solve(gen.constraints.into_iter());
    let end1 = start.elapsed().as_micros();
    println!("{:?} {:?}", end1, tm);

    loop {
        let mut buffer = String::new();
        print!("repl: ");
        std::io::stdout().flush().unwrap();
        std::io::stdin().read_to_string(&mut buffer).unwrap();
        // let mut gen = Elaborator::default();
        match parser::Parser::new(&buffer).parse_term() {
            Some(tm) => {
                // let (tm, ty) = gen.elaborate(&tm).de();

                let mut e = mutation::Elaborator::default();
                dbg!(e.elaborate(&tm));

                // let mut sub = HashMap::new();
                // println!("{:?}", gen.constraints);
                // for (a, b) in &gen.constraints {
                //     let tmp = unify(a.clone().apply(&sub), b.clone().apply(&sub)).unwrap();
                //     sub = compose(tmp, sub);
                // }
                // let sub =  disjoint::solve(gen.constraints.clone());
                // println!("{:?}", sub);
                // println!("tm {:#?} :{:?}", tm, ty);

                // println!("tm {:#?} :{:?}", tm.subst(&sub), ty.apply(&sub));

                // dbg!(sub);
            }
            None => println!("parse error!"),
        }
    }
}
