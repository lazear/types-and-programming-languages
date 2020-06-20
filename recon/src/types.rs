use std::collections::{HashMap, HashSet, VecDeque};

#[derive(Copy, Clone, Default, PartialEq, PartialOrd, Eq, Hash)]
pub struct TypeVar(pub u32, pub u32);

#[derive(Copy, Clone, PartialEq, PartialOrd, Eq, Hash)]
pub struct Tycon {
    id: usize,
    arity: usize,
}

#[derive(Clone, PartialEq, PartialOrd, Eq, Hash)]
pub enum Type {
    Var(TypeVar),
    Con(Tycon, Vec<Type>),
}

#[derive(Debug, Clone)]
pub enum Scheme {
    Mono(Type),
    Poly(Vec<TypeVar>, Type),
}

pub trait Substitution {
    fn ftv(&self) -> HashSet<TypeVar>;
    fn apply(self, s: &HashMap<TypeVar, Type>) -> Self;
}

impl Substitution for Type {
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

    fn apply(self, map: &HashMap<TypeVar, Type>) -> Type {
        match self {
            Type::Var(x) => map.get(&x).cloned().unwrap_or(Type::Var(x)),
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

    pub fn occurs(&self, exist: TypeVar) -> bool {
        match self {
            Type::Var(x) => *x == exist,
            Type::Con(_, tys) => tys.iter().any(|ty| ty.occurs(exist)),
        }
    }

    pub fn de_arrow(&self) -> (&Type, &Type) {
        match self {
            Type::Con(T_ARROW, v) => (&v[0], &v[1]),
            _ => panic!("Not arrow type! {:?}", self),
        }
    }
}

pub fn compose(s1: HashMap<TypeVar, Type>, s2: HashMap<TypeVar, Type>) -> HashMap<TypeVar, Type> {
    let mut s2 = s2
        .into_iter()
        .map(|(k, v)| (k, v.apply(&s1)))
        .collect::<HashMap<TypeVar, Type>>();
    for (k, v) in s1 {
        if !s2.contains_key(&k) {
            s2.insert(k, v);
        }
    }
    s2
}

impl Substitution for Scheme {
    fn ftv(&self) -> HashSet<TypeVar> {
        match self {
            Scheme::Mono(ty) => ty.ftv(),
            Scheme::Poly(vars, ty) => ty.ftv(),
        }
    }

    fn apply(self, map: &HashMap<TypeVar, Type>) -> Scheme {
        match self {
            Scheme::Mono(ty) => Scheme::Mono(ty.apply(map)),
            Scheme::Poly(vars, ty) => {
                let mut map: HashMap<TypeVar, Type> = map.clone();
                for v in &vars {
                    map.remove(v);
                }
                Scheme::Poly(vars, ty.apply(&map))
            }
        }
    }
}

pub const T_ARROW: Tycon = Tycon { id: 0, arity: 2 };
pub const T_INT: Tycon = Tycon { id: 1, arity: 0 };
pub const T_UNIT: Tycon = Tycon { id: 2, arity: 0 };
pub const T_BOOL: Tycon = Tycon { id: 3, arity: 0 };

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

fn fresh_name(x: u32) -> String {
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
            Type::Con(tc, _) => write!(f, "{:?}", tc,),
        }
    }
}
