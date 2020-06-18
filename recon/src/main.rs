use std::collections::{HashMap, HashSet, VecDeque};

pub mod parser;
pub const T_ARROW: Tycon = Tycon { id: 0, arity: 2 };
pub const T_INT: Tycon = Tycon { id: 1, arity: 0 };
pub const T_UNIT: Tycon = Tycon { id: 2, arity: 0 };
pub const T_BOOL: Tycon = Tycon { id: 3, arity: 0 };

pub type Subst = HashMap<TypeVar, Type>;

#[derive(Copy, Clone, Default, PartialEq, PartialOrd, Eq, Hash)]
pub struct TypeVar(pub usize);

#[derive(Copy, Clone, PartialEq, PartialOrd, Eq)]
pub struct Tycon {
    id: usize,
    arity: usize,
}

#[derive(Clone)]
pub enum Type {
    Var(TypeVar),
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

impl std::fmt::Debug for TypeVar {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str(&fresh_name(self.0))
    }
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Type::Var(x) => write!(f, "{:?}", x),
            Type::Con(T_ARROW, tys) => write!(f, "({:?} -> {:?})", tys[0], tys[1]),
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

    fn occurs(&self, exist: TypeVar) -> bool {
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

    fn ftv(&self) -> HashSet<TypeVar> {
        let mut set = HashSet::new();
        let mut queue = VecDeque::new();
        queue.push_back(self);

        while let Some(ty) = queue.pop_front() {
            match ty {
                Type::Var(x) => {
                    set.insert(*x);
                }
                Type::Con(_, tys) => {
                    for ty in tys {
                        queue.push_back(ty);
                    }
                }
            }
        }
        set
    }

    fn subst(self, map: &Subst) -> Type {
        match self {
            Type::Var(x) => map.get(&x).cloned().unwrap_or(Type::Var(x)),
            Type::Con(tc, vars) => Type::Con(tc, vars.into_iter().map(|ty| ty.subst(map)).collect()),
        }
    }
}

impl Scheme {
    fn ftv(&self) -> HashSet<TypeVar> {
        match self {
            Scheme::Mono(ty) => ty.ftv(),
            Scheme::Poly(vars, ty) => ty.ftv(),
        }
    }

    fn subst(self, map: &Subst) -> Scheme {
        match self {
            Scheme::Mono(ty) => Scheme::Mono(ty.subst(map)),
            Scheme::Poly(vars, ty) => {
                let mut map: HashMap<TypeVar, Type> = map.clone();
                for v in &vars {
                    map.remove(v);
                }
                Scheme::Poly(vars, ty.subst(&map))
            }
        }
    }
}

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

#[derive(Debug, Clone)]
pub enum Scheme {
    Mono(Type),
    Poly(Vec<TypeVar>, Type),
}

fn compose(s1: Subst, s2: Subst) -> Subst {
    let mut s2 = s2.into_iter().map(|(k, v)| (k, v.subst(&s1))).collect::<Subst>();
    for (k, v) in s1 {
        if !s2.contains_key(&k) {
            s2.insert(k, v);
        }
    }
    s2
}

fn var_bind(var: TypeVar, ty: Type) -> Result<Subst, (Type, Type)> {
    if ty.occurs(var) {
        return Err((Type::Var(var), ty));
    }
    let mut sub = HashMap::new();
    match ty {
        Type::Var(x) if x == var => {}
        _ => {
            sub.insert(var, ty);
        }
    }
    Ok(sub)
}

pub fn unify(a: Type, b: Type) -> Result<Subst, (Type, Type)> {
    println!("unify {:?} {:?}", a, b);
    match (a, b) {
        (Type::Con(a, a_args), Type::Con(b, b_args)) => {
            let mut map = HashMap::new();
            if a_args.len() == b_args.len() && a == b {
                for (a, b) in a_args.into_iter().zip(b_args.into_iter()) {
                    let tmp = unify(a.subst(&map), b.subst(&map))?;
                    map = compose(tmp, map);
                }
                Ok(map)
            } else {
                Err((Type::Con(a, a_args), Type::Con(b, b_args)))
            }
        }
        (Type::Var(tv), b) => var_bind(tv, b),
        (a, Type::Var(tv)) => var_bind(tv, a),
    }
}

pub enum Constraint {
    Eq(Type, Type),
    Inst(Type, Scheme),
    Gen(Type, Scheme),
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

    fn ftv(&self) -> HashSet<TypeVar> {
        let mut set = HashSet::new();
        for s in &self.context {
            set.extend(s.ftv());
        }
        set
    }

    fn generalize(&mut self, ty: Type) -> Scheme {
        let set: HashSet<TypeVar> = ty.ftv().difference(&self.ftv()).copied().collect();

        // println!("{:?} {:?} {:?}", ty, set, self.ftv());
        if set.is_empty() {
            Scheme::Mono(ty)
        } else {
            // let freshv: Vec<TypeVar> = (0..set.len()).map(|_| self.fresh()).collect();
            // let map = set
            //     .into_iter()
            //     .zip(freshv.iter())
            //     .map(|(v, f)| (v, Type::Var(*f)))
            //     .collect::<Subst>();
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
                    .collect::<Subst>();
                ty.subst(&map)
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
                println!("{:?} scheme inst {:?} -> {:?}", s, scheme, ty);

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

                let mut sub = HashMap::new();
                for (a, b) in self.constraints.iter().cloned() {
                    let tmp = unify(a.clone().subst(&sub), b.clone().subst(&sub)).unwrap();
                    sub = compose(tmp, sub);
                }

                self.context = self.context.drain(..).map(|s| s.subst(&sub)).collect();

                println!("{:?}, {:?} ", self.constraints, ty1);

                let scheme = self.generalize(ty1.clone());
                println!(
                    "{:?} scheme generalized: {:?}, {:?}, {:?}",
                    t1, ty1, scheme, self.context
                );
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
    fn subst(self, s: &Subst) -> TypedTerm {
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
    fn subst(self, s: &Subst) -> SystemF {
        SystemF {
            expr: self.expr.subst(s),
            ty: self.ty.subst(s),
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
                let (tm, ty) = gen.elaborate(&tm).de();

                let mut sub = HashMap::new();
                println!("{:?}", gen.constraints);
                for (a, b) in &gen.constraints {
                    let tmp = unify(a.clone().subst(&sub), b.clone().subst(&sub)).unwrap();
                    sub = compose(tmp, sub);
                }

                println!("tm {:#?} :{:?}", tm, ty);

                println!("tm {:#?} :{:?}", tm.subst(&sub), ty.subst(&sub));

                dbg!(sub);
            }
            None => println!("parse error!"),
        }
    }
}
