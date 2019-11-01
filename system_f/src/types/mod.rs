use crate::terms::{Kind, Literal, Term};
use std::collections::{HashMap, VecDeque};
use std::fmt;
use util::span::Span;
mod visit;
use visit::{MutVisitor, Shift, Subst};

#[derive(Clone, PartialEq, PartialOrd)]
pub enum Type {
    Unit,
    Nat,
    Bool,
    Alias(String),
    Var(usize),
    Arrow(Box<Type>, Box<Type>),
    Universal(Box<Type>),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct TypeError {
    pub span: Span,
    pub kind: TypeErrorKind,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum TypeErrorKind {
    ParameterMismatch(Box<Type>, Box<Type>, Span),
    NotArrow,
    NotUniversal,
    UnboundVariable(usize),
}

pub enum Binding {}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Context {
    stack: VecDeque<Type>,
    map: HashMap<String, Type>,
}

impl Context {
    fn push(&mut self, ty: Type) {
        self.stack.push_front(ty);
    }

    fn pop(&mut self) {
        self.stack
            .pop_front()
            .expect("Context::pop() with empty type stack");
    }

    fn len(&self) -> usize {
        self.stack.len()
    }

    fn find(&self, idx: usize) -> Option<&Type> {
        self.stack.get(idx)
    }

    pub fn alias(&mut self, alias: String, ty: Type) {
        self.map.insert(alias, ty);
    }

    fn aliaser(&self) -> Aliaser<'_> {
        Aliaser { map: &self.map }
    }

    pub fn de_alias(&mut self, term: &mut Term) {
        crate::terms::visit::MutVisitor::visit(self, term)
    }
}

impl Context {
    pub const fn error(term: &Term, kind: TypeErrorKind) -> Result<Type, TypeError> {
        Err(TypeError {
            span: term.span,
            kind,
        })
    }

    pub fn type_of(&mut self, term: &Term) -> Result<Type, TypeError> {
        match term.kind() {
            Kind::Lit(Literal::Unit) => Ok(Type::Unit),
            Kind::Lit(Literal::Bool(_)) => Ok(Type::Bool),
            Kind::Lit(Literal::Nat(_)) => Ok(Type::Nat),
            Kind::Var(idx) => self.find(*idx).cloned().ok_or_else(|| TypeError {
                span: term.span,
                kind: TypeErrorKind::UnboundVariable(*idx),
            }),
            Kind::Abs(ty, t2) => {
                self.push(*ty.clone());
                let ty2 = self.type_of(t2)?;
                // println!("{:?} -: {:?}", ty2, t2);
                // Shift::new(-1).visit(&mut ty2);
                // println!("{:?} -: {:?}", ty2, t2);
                self.pop();
                Ok(Type::Arrow(ty.clone(), Box::new(ty2)))
            }
            Kind::App(t1, t2) => {
                let ty1 = self.type_of(t1)?;
                let ty2 = self.type_of(t2)?;
                match ty1 {
                    Type::Arrow(ty11, ty12) => {
                        if *ty11 == ty2 {
                            Ok(*ty12)
                        } else {
                            Context::error(
                                t1,
                                TypeErrorKind::ParameterMismatch(ty11, Box::new(ty2), t2.span),
                            )
                        }
                    }
                    _ => Context::error(term, TypeErrorKind::NotArrow),
                }
            }
            Kind::Fix(inner) => {
                let ty = self.type_of(inner)?;
                match ty {
                    Type::Arrow(ty1, ty2) => {
                        if ty1 == ty2 {
                            Ok(*ty1)
                        } else {
                            Context::error(
                                term,
                                TypeErrorKind::ParameterMismatch(ty1, ty2, inner.span),
                            )
                        }
                    }
                    _ => Context::error(term, TypeErrorKind::NotArrow),
                }
            }
            Kind::Let(t1, t2) => {
                let ty = self.type_of(t1)?;
                self.push(ty);
                self.type_of(t2)
            }
            Kind::TyAbs(ty, term) => {
                let ty2 = self.type_of(term)?;
                Ok(Type::Universal(Box::new(ty2)))
            }
            Kind::TyApp(term, ty) => {
                let mut ty = ty.clone();
                let ty1 = self.type_of(term)?;
                match ty1 {
                    Type::Universal(mut ty12) => {
                        Shift::new(1).visit(&mut ty);
                        Subst::new(*ty).visit(&mut ty12);
                        Shift::new(-1).visit(&mut ty12);
                        Ok(*ty12)
                    }
                    _ => Context::error(term, TypeErrorKind::NotUniversal),
                }
            }
        }
    }
}

struct Aliaser<'ctx> {
    map: &'ctx HashMap<String, Type>,
}

impl<'ctx> MutVisitor for Aliaser<'ctx> {
    fn visit(&mut self, ty: &mut Type) {
        match ty {
            Type::Unit | Type::Bool | Type::Nat => {}
            Type::Var(v) => {}
            Type::Alias(v) => {
                if let Some(aliased) = self.map.get(v) {
                    *ty = aliased.clone();
                }
            }
            Type::Arrow(ty1, ty2) => self.visit_arrow(ty1, ty2),
            Type::Universal(ty) => self.visit_universal(ty),
        }
    }
}

impl crate::terms::visit::MutVisitor for Context {
    fn visit_abs(&mut self, sp: &mut Span, ty: &mut Type, term: &mut Term) {
        self.aliaser().visit(ty);
        self.visit(term);
    }

    fn visit_tyabs(&mut self, sp: &mut Span, ty: &mut Type, term: &mut Term) {
        self.aliaser().visit(ty);
        self.visit(term);
    }

    fn visit_tyapp(&mut self, sp: &mut Span, term: &mut Term, ty: &mut Type) {
        self.aliaser().visit(ty);
        self.visit(term);
    }
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Unit => write!(f, "Unit"),
            Type::Bool => write!(f, "Bool"),
            Type::Nat => write!(f, "Nat"),
            Type::Var(v) => write!(f, "TyVar({})", v),
            Type::Alias(s) => write!(f, "{}", s),
            Type::Arrow(t1, t2) => write!(f, "({:?}->{:?})", t1, t2),
            Type::Universal(ty) => write!(f, "forall X.{:?}", ty),
        }
    }
}
