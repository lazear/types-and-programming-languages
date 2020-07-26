use super::{Term, T_ARROW, T_BOOL, T_INT, T_UNIT};
use std::collections::{HashMap, HashSet, VecDeque};
use std::rc::Rc;

mod write_once;
use write_once::WriteOnce;

#[derive(Debug, Clone, PartialEq)]
pub struct TypeVar {
    exist: usize,
    data: Rc<WriteOnce<Type>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Var(TypeVar),
    Con(super::Tycon, Vec<Type>),
}

#[derive(Debug, Clone)]
pub enum Scheme {
    Mono(Type),
    Poly(Vec<usize>, Type),
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

impl SystemF {
    fn new(expr: TypedTerm, ty: Type) -> SystemF {
        SystemF { expr, ty }
    }
}

impl Type {
    fn ftv(&self, rank: usize) -> HashSet<usize> {
        let mut set = HashSet::new();
        let mut queue = VecDeque::new();
        queue.push_back(self);

        while let Some(ty) = queue.pop_front() {
            match ty {
                Type::Var(x) => match x.data.get() {
                    None => {
                        if x.data.get_rank() > rank {
                            set.insert(x.exist);
                        }
                    }
                    Some(link) => {
                        queue.push_back(link);
                    }
                },
                Type::Con(_, tys) => {
                    for ty in tys {
                        queue.push_back(ty);
                    }
                }
            }
        }
        set
    }

    fn apply(self, map: &HashMap<usize, Type>) -> Type {
        match self {
            Type::Var(x) => match x.data.get() {
                Some(ty) => ty.clone().apply(map),
                None => map.get(&x.exist).cloned().unwrap_or(Type::Var(x)),
            },
            Type::Con(tc, vars) => Type::Con(tc, vars.into_iter().map(|ty| ty.apply(map)).collect()),
        }
    }
}

impl Type {
    pub fn arrow(a: Type, b: Type) -> Type {
        Type::Con(T_ARROW, vec![a, b])
    }

    pub fn bool() -> Type {
        Type::Con(T_BOOL, vec![])
    }

    pub fn de_arrow(&self) -> (&Type, &Type) {
        match self {
            Type::Con(T_ARROW, v) => (&v[0], &v[1]),
            _ => panic!("Not arrow type! {:?}", self),
        }
    }
}

pub fn occurs_check(v: &TypeVar, ty: &Type) -> bool {
    match ty {
        Type::Var(x) => {
            if let Some(info) = x.data.get() {
                occurs_check(v, &info)
            } else {
                let min_rank = x.data.get_rank().min(v.data.get_rank());
                if min_rank != x.data.get_rank() {
                    println!("promoting type var {:?} {}->{}", x, x.data.get_rank(), min_rank);
                    x.data.set_rank(min_rank);
                }

                x.exist == v.exist
            }
        }
        Type::Con(_, vars) => vars.iter().any(|x| occurs_check(v, x)),
    }
}

fn var_bind(v: &TypeVar, ty: &Type) -> Result<(), String> {
    if occurs_check(&v, ty) {
        return Err(format!("Failed occurs check {:?} {:?}", v, ty));
    }

    v.data.set(ty.clone()).unwrap();
    Ok(())
}

fn unify_type(a: &Type, b: &Type) -> Result<(), String> {
    match (a, b) {
        (Type::Var(a), b) => match a.data.get() {
            Some(ty) => unify_type(ty, b),
            None => var_bind(a, b),
        },
        (a, Type::Var(b)) => match b.data.get() {
            Some(ty) => unify_type(a, ty),
            None => var_bind(b, a),
        },
        (Type::Con(a, a_args), Type::Con(b, b_args)) => {
            if a != b {
                return Err(format!("Can't unify constructors {:?} and {:?}", a, b));
            }
            if a_args.len() != b_args.len() {
                return Err(format!("Can't unify argument lists {:?} and {:?}", a_args, b_args));
            }
            for (c, d) in a_args.into_iter().zip(b_args) {
                unify_type(c, d)?;
            }
            Ok(())
        }
    }
}

#[derive(Default, Debug)]
pub struct Elaborator {
    exist: usize,
    rank: usize,
    context: Vec<Scheme>,
}

impl Elaborator {
    fn fresh(&mut self) -> TypeVar {
        let ex = self.exist;
        self.exist += 1;
        TypeVar {
            exist: ex,
            data: Rc::new(WriteOnce::with_rank(self.rank)),
        }
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
        let set: HashSet<usize> = ty.ftv(self.rank);

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
                let map = vars
                    .into_iter()
                    .map(|v| (v, Type::Var(self.fresh())))
                    .collect::<HashMap<usize, Type>>();
                ty.apply(&map)
            }
        }
    }

    pub fn elaborate(&mut self, term: &Term) -> SystemF {
        match term {
            Term::Unit => SystemF::new(TypedTerm::Unit, Type::Con(T_UNIT, vec![])),
            Term::Bool(b) => SystemF::new(TypedTerm::Bool(*b), Type::Con(T_BOOL, vec![])),
            Term::Int(i) => SystemF::new(TypedTerm::Int(*i), Type::Con(T_INT, vec![])),

            Term::Var(x, s) => {
                let scheme = self.get_scheme(*x).cloned().expect("Unbound variable!");
                let ty = self.instantiate(scheme.clone());
                SystemF::new(TypedTerm::Var(*x, s.clone()), ty)
            }
            Term::Abs(body) => {
                let arg = self.fresh();

                self.context.push(Scheme::Mono(Type::Var(arg.clone())));
                let body = self.elaborate(body);
                self.context.pop();
                let arrow = Type::arrow(Type::Var(arg), body.ty.clone());
                SystemF::new(TypedTerm::Abs(Box::new(body)), arrow)
            }
            Term::App(t1, t2) => {
                let t1 = self.elaborate(t1);
                let t2 = self.elaborate(t2);

                let v = self.fresh();

                unify_type(&t1.ty, &Type::arrow(t2.ty.clone(), Type::Var(v.clone()))).unwrap();

                SystemF::new(TypedTerm::App(Box::new(t1), Box::new(t2)), Type::Var(v))
            }
            Term::Let(t1, t2) => {
                self.rank += 1;
                let t1 = self.elaborate(t1);
                self.rank -= 1;

                let scheme = self.generalize(t1.ty.clone());

                self.context.push(scheme);
                let t2 = self.elaborate(t2);
                self.context.pop();
                let ty = t2.ty.clone();
                SystemF::new(TypedTerm::Let(Box::new(t1), Box::new(t2)), ty)
            }
            Term::If(t1, t2, t3) => {
                let t1 = self.elaborate(t1);
                let t2 = self.elaborate(t2);
                let t3 = self.elaborate(t3);

                unify_type(&t1.ty, &Type::bool()).unwrap();
                unify_type(&t2.ty, &t3.ty).unwrap();

                let ty = t2.ty.clone();
                SystemF::new(TypedTerm::If(Box::new(t1), Box::new(t2), Box::new(t3)), ty)
            }
        }
    }
}
