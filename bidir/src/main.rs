use std::collections::HashSet;

#[derive(Clone, Debug, PartialEq)]
enum Type {
    Unit,
    Var(usize),
    Arrow(Box<Type>, Box<Type>),
    Exist(usize),
    Univ(Box<Type>),
}

impl Type {
    fn monotype(&self) -> bool {
        match &self {
            Type::Univ(_) => false,
            Type::Arrow(t1, t2) => t1.monotype() && t2.monotype(),
            _ => true,
        }
    }

    fn freevars(&self) -> Vec<usize> {
        match &self {
            Type::Unit | Type::Var(_) => vec![],
            Type::Exist(v) => vec![*v],
            Type::Arrow(a, b) => {
                let mut v = a.freevars();
                v.extend(b.freevars());
                v
            }
            Type::Univ(a) => a.freevars(),
        }
    }

    fn subst(&mut self, s: &Type) {
        fn walk<F: Fn(&mut Type)>(t: &mut Type, c: usize, f: &F) {
            match t {
                Type::Var(n) if *n == c => f(t),
                Type::Arrow(a, b) => {
                    walk(a, c, f);
                    walk(b, c, f);
                }
                Type::Univ(a) => walk(a, c + 1, f),
                _ => {}
            }
        }
        walk(self, 0, &|f| *f = s.clone());
    }
}

#[derive(Clone, Debug, PartialEq)]
enum Expr {
    Unit,
    Var(usize),
    Abs(Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Ann(Box<Expr>, Box<Type>),
}

#[derive(Clone, Debug, PartialEq)]
enum Element {
    Var(usize),
    Ann(usize, Type),
    Exist(usize),
    Solved(usize, Type),
    Marker(usize),
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Context {
    ctx: Vec<Element>,
    ev: usize,
}

impl Context {
    fn fresh_ev(&mut self) -> usize {
        let e = self.ev;
        self.ev += 1;
        e
    }

    fn apply(&self, ty: Type) -> Type {
        match ty {
            Type::Unit | Type::Var(_) => ty,
            Type::Arrow(a, b) => Type::Arrow(Box::new(self.apply(*a)), Box::new(self.apply(*b))),
            Type::Univ(ty) => Type::Univ(Box::new(self.apply(*ty))),
            Type::Exist(n) => {
                println!("apply ex{}, {:?}", n, self.ctx);
                match self.solution(n) {
                    Some(solved) => solved.clone(),
                    None => ty,
                }
            }
        }
    }

    fn get_ann(&self, idx: usize) -> Option<&Type> {
        dbg!(&self.ctx);
        for (ix, ty) in self.ctx.iter().rev().enumerate().filter_map(|(i, x)| {
            if let Element::Ann(_, ty) = x {
                Some((i, ty))
            } else {
                None
            }
        }) {
            if ix == idx {
                return Some(ty);
            }
        }
        None
    }

    fn solution(&self, idx: usize) -> Option<&Type> {
        for elem in &self.ctx {
            match &elem {
                Element::Solved(n, ty) if *n == idx => return Some(ty),
                _ => {}
            }
        }
        None
    }

    fn subtype(&mut self, a: &Type, b: &Type) -> Result<(), String> {
        use Type::*;
        match (a, b) {
            // Rule <: Unit
            (Unit, Unit) => Ok(()),
            // Rule <: Var
            (Var(a), Var(b)) if a == b => Ok(()),
            // Rule <: Exvar
            (Exist(a), Exist(b)) if a == b => Ok(()),
            // Rule <: ->
            (Arrow(a1, a2), Arrow(b1, b2)) => self.subtype(b1, a1).and(self.subtype(b2, a2)),
            // Rule <: forall. L
            (Univ(a), b) => {
                let alpha = self.fresh_ev();
                let mut a_ = *a.clone();
                a_.subst(b);
                self.with_scope(|f| {
                    f.ctx.push(Element::Marker(alpha));
                    f.ctx.push(Element::Exist(alpha));
                    f.subtype(&a_, b)
                })
            }
            // Rule <: forall. R
            (a, Univ(b)) => {
                let alpha = self.fresh_ev();
                self.with_scope(|f| {
                    f.ctx.push(Element::Exist(alpha));
                    f.subtype(a, b)
                })
            }
            // Rule <: InstantiateL
            (Exist(alpha), A) => {
                println!("FV({:?}) contains {} {}", A, alpha, A.freevars().contains(alpha));
                unimplemented!()
            }
            // Rule <: InstantiateR
            (A, Exist(alpha)) => {
                println!("FV({:?}) contains {} {}", A, alpha, A.freevars().contains(alpha));
                unimplemented!()
            }
            (a, b) => Err(format!("{:?} is not a subtype of {:?}", a, b)),
        }
    }

    // Pop off any stack growth incurred from calling `f`
    fn with_scope<T, F: Fn(&mut Context) -> T>(&mut self, f: F) -> T {
        let n = self.ctx.len();
        let t = f(self);
        while self.ctx.len() > n {
            self.ctx.pop();
        }
        t
    }

    fn infer(&mut self, e: &Expr) -> Result<Type, String> {
        match e {
            // Rule 1l=>
            Expr::Unit => Ok(Type::Unit),
            // Rule Anno
            Expr::Ann(x, ty) => {
                self.check(x, ty)?;
                Ok(*ty.clone())
            }
            // Rule Var
            Expr::Var(x) => self.get_ann(*x).cloned().ok_or(format!("unbound db {:?}", x)),
            // Rule ->I =>
            Expr::Abs(e) => {
                let alpha = self.fresh_ev();
                let beta = self.fresh_ev();
                // Fresh existential var for function domain
                self.ctx.push(Element::Exist(alpha));
                // And for codomain
                self.ctx.push(Element::Exist(beta));

                // Check the function body against Beta
                self.with_scope(|f| {
                    f.ctx.push(Element::Ann(0, Type::Exist(alpha)));
                    f.check(e, &Type::Exist(beta))
                })?;

                // alpha and beta stay on the stack, since they appear in the output type
                Ok(Type::Arrow(Box::new(Type::Exist(alpha)), Box::new(Type::Exist(beta))))
            }
            Expr::App(e1, e2) => {
                let ty = self.infer(&e1)?;
                dbg!(ty);
                dbg!(&self);
                unimplemented!()
            }
        }
    }

    fn check(&mut self, e: &Expr, a: &Type) -> Result<(), String> {
        match (e, a) {
            // Rule 1l
            (Expr::Unit, Type::Unit) => Ok(()),
            // Rule ->I
            (Expr::Abs(body), Type::Arrow(a1, a2)) => self.with_scope(|f| {
                f.ctx.push(Element::Ann(0, *a1.clone()));
                f.check(body, a2)
            }),
            // Rule forall. I
            (e, Type::Univ(ty)) => self.with_scope(|f| {
                f.ctx.push(Element::Var(0));
                f.check(e, &ty)
            }),
            // Rule Sub
            (e, b) => {
                let a = self.infer(e)?;
                let a = self.apply(a);
                let b = self.apply(b.clone());
                self.subtype(&a, &b)?;
                Ok(())
            }
        }
    }
}

macro_rules! var {
    ($x:expr) => {
        Expr::Var($x)
    };
}

macro_rules! app {
    ($x:expr, $y:expr) => {
        Expr::App(Box::new($x), Box::new($y))
    };
}

macro_rules! abs {
    ($x:expr) => {
        Expr::Abs(Box::new($x))
    };
}

macro_rules! ann {
    ($x:expr, $t:expr) => {
        Expr::Ann(Box::new($x), Box::new($t))
    };
}

fn main() {
    println!("Hello, world!");

    let id = abs!(var!(0));
    let mut ctx = Context::default();

    dbg!(ctx.infer(&id));
}
