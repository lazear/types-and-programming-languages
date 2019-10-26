use crate::term::{RecordField, Term};
use crate::visitor::{Visitable, Visitor};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

#[derive(Clone, PartialEq, PartialOrd)]
pub enum Type {
    Unit,
    Bool,
    Nat,
    Var(String),
    Arrow(Box<Type>, Box<Type>),
    Record(Vec<(Rc<String>, Type)>),
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Unit => write!(f, "Unit"),
            Type::Bool => write!(f, "Bool"),
            Type::Nat => write!(f, "Nat"),
            Type::Arrow(a, b) => write!(f, "{:?}->{:?}", a, b),
            Type::Record(r) => write!(
                f,
                "{{{}}}",
                r.iter()
                    .map(|x| format!("{:?}", x.1))
                    .collect::<Vec<String>>()
                    .join(",")
            ),
            Type::Var(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum TypeError {
    Guard,
    ArmMismatch,
    ParameterMismatch,
    UnknownVariable,
    ExpectedArrow,
    InvalidProjection,
    NotRecordType,
}

#[derive(Clone, Debug, Default)]
/// A typing context, Γ
///
/// Much simpler than the binding list suggested in the book, and used
/// in the other directories, but this should be more efficient, and
/// a vec is really overkill here
pub struct Context<'a> {
    types: Rc<RefCell<HashMap<String, Type>>>,
    parent: Option<&'a Context<'a>>,
    ty: Option<Type>,
}

impl<'a> Visitor<Result<Type, TypeError>> for Context<'a> {
    fn visit_var(&mut self, var: usize) -> Result<Type, TypeError> {
        self.get(var).cloned().ok_or(TypeError::UnknownVariable)
    }

    fn visit_abs(&mut self, ty: Type, body: Rc<Term>) -> Result<Type, TypeError> {
        let ty = match ty {
            Type::Var(name) => self
                .types
                .borrow()
                .get(&name)
                .cloned()
                .ok_or(TypeError::UnknownVariable)?,
            x => x,
        };
        let mut ctx = self.add(ty.clone());
        let ty_body: Result<Type, TypeError> = body.accept(&mut ctx);
        Ok(Type::Arrow(Box::new(ty), Box::new(ty_body?)))
    }

    fn visit_app(&mut self, t1: Rc<Term>, t2: Rc<Term>) -> Result<Type, TypeError> {
        Ok(Type::Nat)
    }

    fn visit_if(
        &mut self,
        guard: Rc<Term>,
        csq: Rc<Term>,
        alt: Rc<Term>,
    ) -> Result<Type, TypeError> {
        Ok(Type::Nat)
    }

    fn visit_let(&mut self, bind: Rc<Term>, alt: Rc<Term>) -> Result<Type, TypeError> {
        Ok(Type::Nat)
    }

    fn visit_succ(&mut self, t: Rc<Term>) -> Result<Type, TypeError> {
        Ok(Type::Nat)
    }

    fn visit_pred(&mut self, t: Rc<Term>) -> Result<Type, TypeError> {
        Ok(Type::Nat)
    }

    fn visit_iszero(&mut self, t: Rc<Term>) -> Result<Type, TypeError> {
        Ok(Type::Nat)
    }

    fn visit_const(&mut self, c: Rc<Term>) -> Result<Type, TypeError> {
        match c.as_ref() {
            Term::Unit => Ok(Type::Unit),
            Term::Zero => Ok(Type::Nat),
            Term::True | Term::False => Ok(Type::Bool),
            _ => unreachable!(),
        }
    }

    fn visit_record(&mut self, rec: &[RecordField]) -> Result<Type, TypeError> {
        let tys = rec
            .iter()
            .map(|f| f.data.accept(self).map(|ty| (f.label.clone(), ty)))
            .collect::<Result<Vec<(Rc<String>, Type)>, TypeError>>()?;
        Ok(Type::Record(tys))
    }

    fn visit_proj(&mut self, c: Rc<Term>, proj: Rc<String>) -> Result<Type, TypeError> {
        match c.accept(self)? {
            Type::Record(fields) => {
                for f in &fields {
                    if f.0 == proj {
                        return Ok(f.1.clone());
                    }
                }
                Err(TypeError::InvalidProjection)
            }
            _ => Err(TypeError::NotRecordType),
        }
    }

    fn visit_typedecl(&mut self, name: Rc<String>, ty: &Type) -> Result<Type, TypeError> {
        self.bind(name.to_string(), ty.clone());
        Ok(Type::Unit)
    }
}

impl<'a> Context<'a> {
    pub fn bind(&self, name: String, ty: Type) {
        self.types.borrow_mut().insert(name, ty);
    }

    pub fn lookup(&self, name: &str) -> Option<Type> {
        self.types.borrow().get(name).cloned()
    }

    pub fn add(&self, ty: Type) -> Context {
        if self.ty.is_none() {
            Context {
                types: self.types.clone(),
                parent: self.parent,
                ty: Some(ty),
            }
        } else {
            Context {
                types: self.types.clone(),
                parent: Some(self),
                ty: Some(ty),
            }
        }
    }

    pub fn get(&self, idx: usize) -> Option<&Type> {
        if idx == 0 {
            self.ty.as_ref()
        } else if let Some(ctx) = self.parent {
            ctx.get(idx - 1)
        } else {
            None
        }
    }

    pub fn type_of(&self, term: &Term) -> Result<Type, TypeError> {
        use Term::*;
        match term {
            TypeDecl(_, _) => Ok(Type::Unit),
            Unit => Ok(Type::Unit),
            True => Ok(Type::Bool),
            False => Ok(Type::Bool),
            Zero => Ok(Type::Nat),
            Record(rec) => {
                let tys = rec
                    .iter()
                    .map(|f| self.type_of(&f.data).map(|ty| (f.label.clone(), ty)))
                    .collect::<Result<Vec<(Rc<String>, Type)>, TypeError>>()?;
                Ok(Type::Record(tys))
            }
            Projection(r, proj) => match r.as_ref() {
                Term::Record(fields) => self.type_of(
                    crate::term::record_access(fields, &proj)
                        .ok_or(TypeError::InvalidProjection)?
                        .as_ref(),
                ),
                _ => Err(TypeError::NotRecordType),
            },
            IsZero(t) => {
                if let Ok(Type::Nat) = self.type_of(t) {
                    Ok(Type::Bool)
                } else {
                    Err(TypeError::ParameterMismatch)
                }
            }
            Succ(t) | Pred(t) => {
                if let Ok(Type::Nat) = self.type_of(t) {
                    Ok(Type::Nat)
                } else {
                    Err(TypeError::ParameterMismatch)
                }
            }
            If(guard, csq, alt) => {
                if let Ok(Type::Bool) = self.type_of(guard) {
                    let ty1 = self.type_of(csq)?;
                    let ty2 = self.type_of(alt)?;
                    if ty1 == ty2 {
                        Ok(ty2)
                    } else {
                        Err(TypeError::ArmMismatch)
                    }
                } else {
                    Err(TypeError::Guard)
                }
            }
            Let(bind, body) => {
                let ty = self.type_of(bind)?;
                let ctx = self.add(ty.clone());
                ctx.type_of(body)
            }
            Var(s) => match self.get(*s) {
                Some(ty) => Ok(ty.clone()),
                _ => Err(TypeError::UnknownVariable),
            },
            Abs(ty, body) => {
                let ctx = self.add(ty.clone());
                let ty_body = ctx.type_of(body)?;
                Ok(Type::Arrow(Box::new(ty.clone()), Box::new(ty_body)))
            }
            App(t1, t2) => {
                let ty1 = self.type_of(t1)?;
                let ty2 = self.type_of(t2)?;
                match ty1 {
                    Type::Arrow(ty11, ty12) => {
                        if *ty11 == ty2 {
                            Ok(*ty12)
                        } else {
                            Err(TypeError::ParameterMismatch)
                        }
                    }
                    _ => Err(TypeError::ExpectedArrow),
                }
            }
        }
    }
}
