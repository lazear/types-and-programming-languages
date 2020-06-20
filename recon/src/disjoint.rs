//! A disjoint set using the union-find algorithm with path-compression

use std::cell::Cell;
use std::cmp::Ordering;
use std::collections::HashMap;

struct SetElement<T> {
    data: Option<T>,
    rank: Cell<u32>,
    parent: Cell<usize>,
}

pub struct DisjointSet<T> {
    elements: Vec<SetElement<T>>,
    components: Cell<usize>,
}

impl<T> Default for DisjointSet<T> {
    fn default() -> Self {
        DisjointSet {
            elements: Vec::new(),
            components: Cell::new(0),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Eq, Hash)]
pub struct Element(usize);

pub enum Choice {
    Left,
    Right,
}

impl<T> DisjointSet<T> {
    pub fn new() -> DisjointSet<T> {
        DisjointSet {
            elements: Vec::new(),
            components: Cell::new(0),
        }
    }

    pub fn singleton(&mut self, data: T) -> Element {
        let n = self.elements.len();
        let elem = SetElement {
            data: Some(data),
            rank: Cell::new(0),
            parent: Cell::new(n),
        };
        self.elements.push(elem);
        self.components.replace(self.components.get() + 1);
        Element(n)
    }

    fn find_set(&self, id: usize) -> usize {
        // locate parent set
        let mut ptr = id;
        while ptr != self.elements[ptr].parent.get() {
            ptr = self.elements[ptr].parent.get();
        }

        // id is the representative element, return
        if ptr == id {
            return id;
        }

        // perform path compression
        let parent = ptr;
        ptr = id;
        while ptr != self.elements[ptr].parent.get() {
            ptr = self.elements[ptr].parent.replace(parent);
        }
        parent
    }

    pub fn find_repr(&self, element: Element) -> Element {
        Element(self.find_set(element.0))
    }

    pub fn data(&self, element: Element) -> Option<&T> {
        self.elements[element.0].data.as_ref()
    }

    pub fn find(&self, element: Element) -> &T {
        // Invariant that the representative element is always "Some"
        self.elements[self.find_set(element.0)]
            .data
            .as_ref()
            .expect("Invariant violated")
    }

    pub fn union<F: Fn(T, T) -> T>(&mut self, f: F, a: Element, b: Element) {
        let pa = self.find_set(a.0);
        let pb = self.find_set(b.0);

        if pa == pb {
            return;
        }

        // Move data out first to appease borrowck
        let a_data = self.elements[pa].data.take().expect("Invariant violated");
        let b_data = self.elements[pb].data.take().expect("Invariant violated");

        self.components.replace(self.components.get() - 1);
        match self.elements[pa].rank.cmp(&self.elements[pb].rank) {
            Ordering::Equal => {
                self.elements[pa].data = Some(f(a_data, b_data));
                self.elements[pb].parent.replace(pa);
                self.elements[pa].rank.replace(self.elements[pa].rank.get() + 1);
            }
            Ordering::Less => {
                self.elements[pb].data = Some(f(a_data, b_data));
                self.elements[pa].parent.replace(pb);
                self.elements[pb].rank.replace(self.elements[pb].rank.get() + 1);
            }
            Ordering::Greater => {
                self.elements[pa].data = Some(f(a_data, b_data));
                self.elements[pb].parent.replace(pa);
                self.elements[pa].rank.replace(self.elements[pa].rank.get() + 1);
            }
        }
    }

    pub fn partition(&self) -> Vec<&T> {
        let mut v = HashSet::new();

        for idx in 0..self.elements.len() {
            v.insert(self.find_set(idx));
        }
        v.into_iter()
            .map(|idx| self.elements[idx].data.as_ref().unwrap())
            .collect()
    }
}

use super::*;
type Variable = Element;

#[derive(Debug, Clone)]
pub enum Unification {
    Unknown(TypeVar),
    Constr(Tycon, Vec<Variable>),
}

impl Unification {
    fn is_var(&self) -> bool {
        match self {
            Self::Unknown(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Default)]
pub struct Unifier {
    set: disjoint::DisjointSet<Unification>,
    map: HashMap<Type, Variable>,
}

impl Unifier {
    pub fn new() -> Unifier {
        Unifier {
            set: DisjointSet::new(),
            map: HashMap::default(),
        }
    }

    pub fn occurs_check(&self, v: TypeVar, u: &Unification) -> bool {
        match u {
            Unification::Unknown(x) => *x == v,
            Unification::Constr(_, vars) => vars.iter().any(|x| self.occurs_check(v, self.set.find(*x))),
        }
    }

    pub fn decode(&self, uni: &Unification) -> Type {
        match uni {
            Unification::Unknown(x) => Type::Var(*x),
            Unification::Constr(tc, vars) => {
                Type::Con(*tc, vars.into_iter().map(|v| self.decode(self.set.find(*v))).collect())
            }
        }
    }

    pub fn intern(&mut self, ty: Type) -> Variable {
        if let Some(v) = self.map.get(&ty) {
            return *v;
        }

        let v = match &ty {
            Type::Var(x) => self.set.singleton(Unification::Unknown(*x)),
            Type::Con(tc, vars) => {
                let vars = vars.into_iter().cloned().map(|v| self.intern(v)).collect();
                self.set.singleton(Unification::Constr(*tc, vars))
            }
        };
        self.map.insert(ty, v);

        v
    }

    fn var_bind(&mut self, v: TypeVar, v_: Variable, u: &Unification, u_: Variable) -> Result<(), String> {
        if self.occurs_check(v, u) {
            return Err(format!("Failed occurs check {:?} {:?}", v, u));
        }
        self.set.union(
            |a, b| match (a, b) {
                (a @ Unification::Constr(_, _), _) => a,
                (_, b) => b,
            },
            u_,
            v_,
        );
        Ok(())
    }

    pub fn subst(&self) -> HashMap<TypeVar, Type> {
        let mut map = HashMap::new();
        for (ty, var) in &self.map {
            match ty {
                Type::Var(x) => {
                    map.insert(*x, self.decode(self.set.find(*var)));
                }
                _ => {}
            }
        }

        map
    }

    pub fn unify(&mut self, a_: Variable, b_: Variable) -> Result<(), String> {
        if a_ == b_ {
            return Ok(());
        }
        if a_ == self.set.find_repr(b_) || b_ == self.set.find_repr(a_) {
            return Ok(());
        }
        let a = self.set.find(a_).clone();
        let b = self.set.find(b_).clone();
        use Unification::*;
        match (a, b) {
            (Unknown(a), b) => self.var_bind(a, a_, &b, b_),
            (a, Unknown(b)) => self.var_bind(b, b_, &a, a_),
            (Constr(a, a_vars), Constr(b, b_vars)) => {
                if a != b {
                    return Err(format!("Can't unify constructors {:?} and {:?}", a, b));
                }
                if a_vars.len() != b_vars.len() {
                    return Err(format!("Can't unify argument lists {:?} and {:?}", a_vars, b_vars));
                }
                for (c, d) in a_vars.into_iter().zip(b_vars) {
                    self.set.union(
                        |a, b| match (a, b) {
                            (a @ Unification::Constr(_, _), _) => a,
                            (_, b) => b,
                        },
                        c,
                        d,
                    );
                }
                Ok(())
            }
        }
    }
}

pub fn solve<I: Iterator<Item = (Type, Type)>>(iter: I) -> Result<HashMap<TypeVar, Type>, String> {
    let mut un = Unifier::new();

    for (a, b) in iter {
        let a = un.intern(a);
        let b = un.intern(b);
        un.unify(a, b)?;
    }
    let mut map = HashMap::new();
    for (ty, var) in &un.map {
        match ty {
            Type::Var(x) => {
                map.insert(*x, un.decode(un.set.find(*var)));
            }
            _ => {}
        }
    }

    Ok(map)
}

impl<T: std::fmt::Debug> std::fmt::Debug for DisjointSet<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let part = self.partition();
        writeln!(f, "{{")?;
        for values in part {
            write!(f, "\t{:?}\n", values)?;
        }
        writeln!(f, "}}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let mut set = DisjointSet::new();

        let a = set.singleton(10);
        let b = set.singleton(12);
        let c = set.singleton(14);
        let d = set.singleton(29);
        let e = set.singleton(1);

        set.union(|a, b| a, a, d);
        set.union(|a, b| a, d, c);
        set.union(|a, b| a, a, e);

        assert_eq!(set.find(a), &29);
        assert_eq!(set.find(e), &29);
        assert_eq!(set.find(c), &29);
        assert_eq!(set.find(d), &29);
        assert!(set.find(a) != set.find(b));

        assert_eq!(set.components.get(), 2);
    }
}
