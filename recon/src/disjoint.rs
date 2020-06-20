//! A disjoint set using the union-find algorithm with path-compression

use std::cell::Cell;
use std::cmp::Ordering;
use std::collections::HashMap;

struct SetElement<T> {
    data: T,
    rank: Cell<u32>,
    parent: Cell<usize>,
}

pub struct DisjointSet<T> {
    elements: Vec<SetElement<T>>,
    components: Cell<usize>,
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
            data,
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

    pub fn data(&self, element: Element) -> &T {
        &self.elements[element.0].data
    }

    pub fn data_mut(&mut self, element: Element) -> &mut T {
        &mut self.elements[element.0].data
    }

    pub fn find(&self, element: Element) -> &T {
        &self.elements[self.find_set(element.0)].data
    }

    pub fn union(&self, a: Element, b: Element) {
        self.union_key(|_, _| Choice::Left, a, b)
    }

    pub fn union_key<F: Fn(&T, &T) -> Choice>(&self, f: F, a: Element, b: Element) {
        let pa = self.find_set(a.0);
        let pb = self.find_set(b.0);

        if pa == pb {
            return;
        }

        let a = &self.elements[pa];
        let b = &self.elements[pb];

        self.components.replace(self.components.get() - 1);
        match a.rank.cmp(&b.rank) {
            Ordering::Equal => match f(&a.data, &b.data) {
                Choice::Left => {
                    b.parent.replace(pa);
                    a.rank.replace(a.rank.get() + 1);
                }
                Choice::Right => {
                    a.parent.replace(pb);
                    b.rank.replace(b.rank.get() + 1);
                }
            },
            Ordering::Less => {
                a.parent.replace(pb);
                b.rank.replace(b.rank.get() + 1);
            }
            Ordering::Greater => {
                b.parent.replace(pa);
                a.rank.replace(a.rank.get() + 1);
            }
        }
    }

    pub fn union2<F: Fn(&T, &T) -> Choice>(&self, f: F, a: Element, b: Element) {
        let pa = self.find_set(a.0);
        let pb = self.find_set(b.0);

        if pa == pb {
            return;
        }

        let a = &self.elements[pa];
        let b = &self.elements[pb];

        self.components.replace(self.components.get() - 1);
        match f(&a.data, &b.data) {
            Choice::Left => {
                b.parent.replace(pa);
                a.rank.replace(a.rank.get() + 1);
            }
            Choice::Right => {
                a.parent.replace(pb);
                b.rank.replace(b.rank.get() + 1);
            }
        }
    }

    pub fn partition(&self) -> HashMap<usize, Vec<&T>> {
        let mut v = HashMap::new();

        for idx in 0..self.elements.len() {
            let parent = self.find_set(idx);
            v.entry(parent).or_insert_with(Vec::new).push(&self.elements[idx].data);
        }
        v
    }
}

use super::*;
type Variable = Element;

#[derive(Debug)]
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

#[derive(Debug)]
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
            Unification::Constr(_, vars) => vars.iter().any(|x| self.occurs_check(v, self.set.data(*x))),
        }
    }

    pub fn decode(&self, uni: &Unification) -> Type {
        match uni {
            Unification::Unknown(x) => Type::Var(*x),
            Unification::Constr(tc, vars) => Type::Con(
                *tc,
                vars.into_iter()
                    .map(|v| self.decode(self.set.find(*v).clone()))
                    .collect(),
            ),
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

    fn var_bind(&self, v: TypeVar, v_: Variable, u: &Unification, u_: Variable) -> Result<(), String> {
        if self.occurs_check(v, u) {
            return Err(format!("Failed occurs check {:?} {:?}", v, u));
        }
        self.set.union2(
            |a, b| match (a, b) {
                (Unification::Constr(_, _), _) => Choice::Left,
                _ => Choice::Right,
            },
            u_,
            v_,
        );
        Ok(())
    }

    pub fn unify(&self, a_: Variable, b_: Variable) -> Result<(), String> {
        if a_ == self.set.find_repr(b_) || b_ == self.set.find_repr(a_){
            return Ok(())
        }
        let a = self.set.find(a_);
        let b = self.set.find(b_);
        use Unification::*;
        // println!("{:?} {:?}", a, b);
        match (a, b) {
            (Unknown(a), b) => self.var_bind(*a, a_, b, b_),
            (a, Unknown(b)) => self.var_bind(*b, b_, a, a_),
            (Constr(a, a_vars), Constr(b, b_vars)) => {
                if a != b {
                    return Err(format!("Can't unify constructors {:?} and {:?}", a, b));
                }
                if a_vars.len() != b_vars.len() {
                    return Err(format!("Can't unify argument lists {:?} and {:?}", a_vars, b_vars));
                }
                for (c, d) in a_vars.into_iter().zip(b_vars) {
                    self.set.union2(
                        |a, b| match (a, b) {
                            (Unification::Constr(_, _), _) => Choice::Left,
                            _ => Choice::Right,
                        },
                        *c,
                        *d,
                    );
                }
                Ok(())
            }
        }
    }
}

pub fn solve(constraints: Vec<(Type, Type)>) -> Result<HashMap<TypeVar, Type>, String> {
    let mut un = Unifier::new();
    let vars = constraints
        .into_iter()
        .map(|(a, b)| (un.intern(a), un.intern(b)))
        .collect::<Vec<_>>();
    for (a, b) in vars {
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
        for (key, values) in part {
            write!(f, "\t{:?}: {:?}\n", self.elements[key].data, values)?;
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

        set.union_key(|a, b| if a >= b { Choice::Left } else { Choice::Right }, a, d);
        set.union_key(|a, b| if a >= b { Choice::Left } else { Choice::Right }, d, c);
        set.union_key(|a, b| if a >= b { Choice::Left } else { Choice::Right }, a, e);

        assert_eq!(set.find(a), &29);
        assert_eq!(set.find(e), &29);
        assert_eq!(set.find(c), &29);
        assert_eq!(set.find(d), &29);
        assert!(set.find(a) != set.find(b));

        assert_eq!(set.components.get(), 2);
    }
}
