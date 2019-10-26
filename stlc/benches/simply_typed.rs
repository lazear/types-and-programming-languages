use criterion::*;

#[derive(Clone, Debug)]
pub enum Term {
    True,
    False,
    Var(usize),
    Abs(Type, Box<Term>),
    App(Box<Term>, Box<Term>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Bool,
    Arrow(Box<Type>, Box<Type>),
}

#[derive(Clone, Default, Debug)]
pub struct Context<'a> {
    parent: Option<&'a Context<'a>>,
    ty: Option<Type>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeError {
    Parameter,
    Arrow,
    Unbound,
}

impl<'a> Context<'a> {
    fn bind(&self, ty: Type) -> Context {
        if self.ty.is_none() {
            Context {
                parent: self.parent,
                ty: Some(ty),
            }
        } else {
            Context {
                parent: Some(self),
                ty: Some(ty),
            }
        }
    }

    fn find(&self, idx: usize) -> Option<Type> {
        if idx == 0 {
            self.ty.as_ref().cloned()
        } else if let Some(ctx) = self.parent {
            ctx.find(idx - 1)
        } else {
            None
        }
    }

    fn type_of(&self, term: &Term) -> Result<Type, TypeError> {
        match term {
            Term::True | Term::False => Ok(Type::Bool),
            Term::Var(idx) => self.find(*idx).ok_or(TypeError::Unbound),
            Term::Abs(ty, tm) => {
                let ctx = self.bind(ty.clone());
                let ty_ = ctx.type_of(&tm)?;
                Ok(Type::Arrow(Box::new(ty.clone()), Box::new(ty_)))
            }
            Term::App(t1, t2) => {
                let ty1 = self.type_of(&t1)?;
                let ty2 = self.type_of(&t2)?;
                match ty1 {
                    Type::Arrow(ty11, ty12) => {
                        if *ty11 == ty2 {
                            Ok(*ty12)
                        } else {
                            Err(TypeError::Parameter)
                        }
                    }
                    _ => Err(TypeError::Arrow),
                }
            }
        }
    }
}

trait Visitor<T> {
    fn visit_bool(&mut self, b: bool) -> T;
    fn visit_var(&mut self, var: usize) -> T;
    fn visit_abs(&mut self, ty: &Type, term: &Term) -> T;
    fn visit_app(&mut self, t1: &Term, t2: &Term) -> T;
}

trait Visitable<V, T>
where
    V: Visitor<T>,
{
    fn accept(&self, visitor: &mut V) -> T;
}

impl<V, T> Visitable<V, T> for Term
where
    V: Visitor<T>,
{
    fn accept(&self, visitor: &mut V) -> T {
        match self {
            Term::True => visitor.visit_bool(true),
            Term::False => visitor.visit_bool(false),
            Term::Var(idx) => visitor.visit_var(*idx),
            Term::Abs(ty, term) => visitor.visit_abs(ty, term),
            Term::App(t1, t2) => visitor.visit_app(t1, t2),
        }
    }
}

impl<'ctx> Visitor<Result<Type, TypeError>> for Context<'ctx> {
    fn visit_bool(&mut self, b: bool) -> Result<Type, TypeError> {
        Ok(Type::Bool)
    }

    fn visit_var(&mut self, var: usize) -> Result<Type, TypeError> {
        self.find(var).ok_or(TypeError::Unbound)
    }
    fn visit_abs(&mut self, ty: &Type, term: &Term) -> Result<Type, TypeError> {
        let mut ctx = self.bind(ty.clone());
        let ty_ = term.accept(&mut ctx);
        Ok(Type::Arrow(Box::new(ty.clone()), Box::new(ty_?)))
    }
    fn visit_app(&mut self, t1: &Term, t2: &Term) -> Result<Type, TypeError> {
        let ty1 = t1.accept(self)?;
        let ty2 = t2.accept(self)?;
        match ty1 {
            Type::Arrow(ty11, ty12) => {
                if *ty11 == ty2 {
                    Ok(*ty12)
                } else {
                    Err(TypeError::Parameter)
                }
            }
            _ => Err(TypeError::Arrow),
        }
    }
}

struct Smoke;

impl Visitor<Result<Type, TypeError>> for Smoke {
    fn visit_bool(&mut self, b: bool) -> Result<Type, TypeError> {
        Ok(Type::Bool)
    }
    fn visit_var(&mut self, var: usize) -> Result<Type, TypeError> {
        Ok(Type::Bool)
    }
    fn visit_abs(&mut self, ty: &Type, term: &Term) -> Result<Type, TypeError> {
        Ok(Type::Bool)
    }
    fn visit_app(&mut self, t1: &Term, t2: &Term) -> Result<Type, TypeError> {
        Ok(Type::Bool)
    }
}

macro_rules! app {
    ($ex:expr, $xy:expr) => {
        Term::App(Box::new($ex), Box::new($xy))
    };
}

macro_rules! abs {
    ($ty:expr, $body:expr) => {
        Term::Abs($ty, Box::new($body))
    };
}

macro_rules! var {
    ($var:expr) => {
        Term::Var($var)
    };
}

macro_rules! arrow {
    ($a:expr, $b:expr) => {
        Type::Arrow(Box::new($a), Box::new($b))
    };
}

fn dynamic(c: &mut Criterion) {
    c.bench_function("dynamic dispatch", |b| {
        let mut ctx = Context::default();
        let id = abs!(Type::Bool, var!(0));
        let app = app!(
            abs!(arrow!(Type::Bool, Type::Bool), app!(var!(0), Term::False)),
            id
        );
        b.iter(|| assert_eq!(black_box(&app).accept(&mut ctx), Ok(Type::Bool)));
    });
}

fn pattern(c: &mut Criterion) {
    c.bench_function("pattern matching", |b| {
        let mut ctx = Context::default();
        let id = abs!(Type::Bool, var!(0));
        let app = app!(
            abs!(arrow!(Type::Bool, Type::Bool), app!(var!(0), Term::False)),
            id
        );
        b.iter(|| assert_eq!(ctx.type_of(black_box(&app)), Ok(Type::Bool)));
    });
}

criterion_group!(benches, dynamic, pattern);
criterion_main!(benches);
