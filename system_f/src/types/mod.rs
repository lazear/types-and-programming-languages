//! Typechecking of the simply typed lambda calculus with parametric
//! polymorphism
pub mod patterns;
pub mod visit;
use crate::diagnostics::*;
use crate::terms::{Kind, Literal, Primitive, Term};
use crate::visit::{MutTermVisitor, MutTypeVisitor};
use std::collections::{HashMap, VecDeque};
use std::fmt;
use util::span::Span;
use visit::{Shift, Subst};

#[derive(Clone, PartialEq, PartialOrd, Eq, Hash)]
pub enum Type {
    Unit,
    Nat,
    Bool,
    Alias(String),
    Var(usize),
    Variant(Vec<Variant>),
    Product(Vec<Type>),
    Arrow(Box<Type>, Box<Type>),
    Universal(Box<Type>),
    Rec(Box<Type>),
}

#[derive(Clone, PartialEq, PartialOrd, Eq, Hash)]
pub struct Variant {
    pub label: String,
    pub ty: Type,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct TypeError {
    pub span: Span,
    pub kind: TypeErrorKind,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum TypeErrorKind {
    ParameterMismatch(Box<Type>, Box<Type>, Span),

    InvalidProjection,
    NotArrow,
    NotUniversal,
    NotVariant,
    NotProduct,
    NotRec,
    IncompatibleArms,
    InvalidPattern,
    NotExhaustive,
    UnreachablePattern,
    UnboundVariable(usize),
}

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
        crate::visit::MutTermVisitor::visit(self, term)
    }
}

/// Helper function for extracting type from a variant
pub fn variant_field<'vs>(
    var: &'vs [Variant],
    label: &str,
    span: Span,
) -> Result<&'vs Type, Diagnostic> {
    for f in var {
        if label == f.label {
            return Ok(&f.ty);
        }
    }
    Err(Diagnostic::error(
        span,
        format!("constructor {} doesn't appear in variant fields", label),
    ))

    // Err(TypeError {
    //     span,
    //     kind: TypeErrorKind::NotVariant,
    // })
}

impl Context {
    pub fn type_check(&mut self, term: &Term) -> Result<Type, Diagnostic> {
        // dbg!(&self.stack);
        // println!("{}", term);
        match term.kind() {
            Kind::Lit(Literal::Unit) => Ok(Type::Unit),
            Kind::Lit(Literal::Bool(_)) => Ok(Type::Bool),
            Kind::Lit(Literal::Nat(_)) => Ok(Type::Nat),
            Kind::Var(idx) => self
                .find(*idx)
                .cloned()
                .ok_or_else(|| Diagnostic::error(term.span, format!("unbound variable {}", idx))),
            Kind::Abs(ty, t2) => {
                self.push(*ty.clone());
                let ty2 = self.type_check(t2)?;
                // Shift::new(-1).visit(&mut ty2);
                self.pop();
                Ok(Type::Arrow(ty.clone(), Box::new(ty2)))
            }
            Kind::App(t1, t2) => {
                let ty1 = self.type_check(t1)?;
                let ty2 = self.type_check(t2)?;
                match ty1 {
                    Type::Arrow(ty11, ty12) => {
                        if *ty11 == ty2 {
                            Ok(*ty12)
                        } else {
                            let d = Diagnostic::error(term.span, "Type mismatch in application")
                                .message(t1.span, format!("Abstraction requires type {:?}", ty11))
                                .message(t2.span, format!("Value has a type of {:?}", ty2));
                            Err(d)
                        }
                    }
                    _ => Err(Diagnostic::error(term.span, "Expected arrow type!")
                        .message(t1.span, format!("operator has type {:?}", ty1))),
                }
            }
            Kind::Fix(inner) => {
                let ty = self.type_check(inner)?;
                match ty {
                    Type::Arrow(ty1, ty2) => {
                        if ty1 == ty2 {
                            Ok(*ty1)
                        } else {
                            let d = Diagnostic::error(term.span, "Type mismatch in fix term")
                                .message(
                                    inner.span,
                                    format!("Abstraction requires type {:?}->{:?}", ty1, ty1),
                                );
                            Err(d)
                        }
                    }
                    _ => Err(Diagnostic::error(term.span, "Expected arrow type!")
                        .message(inner.span, format!("operator has type {:?}", ty))),
                }
            }
            Kind::Primitive(prim) => match prim {
                Primitive::IsZero => Ok(Type::Arrow(Box::new(Type::Nat), Box::new(Type::Bool))),
                _ => Ok(Type::Arrow(Box::new(Type::Nat), Box::new(Type::Nat))),
            },
            Kind::Injection(label, tm, ty) => match ty.as_ref() {
                Type::Variant(fields) => {
                    for f in fields {
                        if label == &f.label {
                            let ty_ = self.type_check(tm)?;
                            if ty_ == f.ty {
                                return Ok(*ty.clone());
                            } else {
                                let d = Diagnostic::error(
                                    term.span,
                                    "Invalid associated type in variant",
                                )
                                .message(
                                    tm.span,
                                    format!(
                                        "variant {} requires type {:?}, but this is {:?}",
                                        label, f.ty, ty_
                                    ),
                                );
                                return Err(d);
                            }
                        }
                    }
                    Err(Diagnostic::error(
                        term.span,
                        format!(
                            "constructor {} does not belong to the variant {:?}",
                            label,
                            fields
                                .iter()
                                .map(|f| f.label.clone())
                                .collect::<Vec<String>>()
                                .join(" | ")
                        ),
                    ))
                }
                _ => Err(Diagnostic::error(
                    term.span,
                    format!("Cannot injection {} into non-variant type {:?}", label, ty),
                )),
            },
            Kind::Projection(term, idx) => match self.type_check(term)? {
                Type::Product(types) => match types.get(*idx) {
                    Some(ty) => Ok(ty.clone()),
                    None => Err(Diagnostic::error(
                        term.span,
                        format!(
                            "{} is out of range for product of length {}",
                            idx,
                            types.len()
                        ),
                    )),
                },
                ty => Err(Diagnostic::error(
                    term.span,
                    format!("Cannot project on non-product type {:?}", ty),
                )),
            },
            Kind::Product(terms) => Ok(Type::Product(
                terms
                    .iter()
                    .map(|t| self.type_check(t))
                    .collect::<Result<_, _>>()?,
            )),
            Kind::Let(pat, t1, t2) => {
                let ty = self.type_check(t1)?;
                if !self.pattern_type_eq(&pat, &ty) {
                    return Err(Diagnostic::error(
                        t1.span,
                        format!("pattern does not match type of binder"),
                    ));
                }

                let height = self.stack.len();

                let binds = crate::patterns::PatTyStack::collect(&ty, &pat);
                for b in binds.into_iter().rev() {
                    self.push(b.clone());
                }

                let y = self.type_check(t2);

                while self.stack.len() > height {
                    self.pop();
                }

                y
            }
            Kind::TyAbs(term) => {
                let ty2 = self.type_check(term)?;
                Ok(Type::Universal(Box::new(ty2)))
            }
            Kind::TyApp(term, ty) => {
                let mut ty = ty.clone();
                let ty1 = self.type_check(term)?;
                match ty1 {
                    Type::Universal(mut ty12) => {
                        Shift::new(1).visit(&mut ty);
                        Subst::new(*ty).visit(&mut ty12);
                        Shift::new(-1).visit(&mut ty12);
                        Ok(*ty12)
                    }
                    _ => Err(Diagnostic::error(
                        term.span,
                        format!("Expected a universal type, not {:?}", ty1),
                    )),
                }
            }
            // See src/types/patterns.rs for exhaustiveness and typechecking
            // of case expressions
            Kind::Case(expr, arms) => self.type_check_case(expr, arms),

            Kind::Unfold(rec, tm) => match rec.as_ref() {
                Type::Rec(inner) => {
                    let ty_ = self.type_check(&tm)?;
                    if ty_ == *rec.clone() {
                        let s = subst(*rec.clone(), *inner.clone());
                        Ok(s)
                    } else {
                        let d = Diagnostic::error(term.span, "Type mismatch in unfold")
                            .message(term.span, format!("unfold requires type {:?}", rec))
                            .message(tm.span, format!("term has a type of {:?}", ty_));
                        Err(d)
                    }
                }
                _ => Err(Diagnostic::error(
                    term.span,
                    format!("Expected a recursive type, not {:?}", rec),
                )),
            },

            Kind::Fold(rec, tm) => match rec.as_ref() {
                Type::Rec(inner) => {
                    let ty_ = self.type_check(&tm)?;
                    let s = subst(*rec.clone(), *inner.clone());
                    if ty_ == s {
                        Ok(*rec.clone())
                    } else {
                        let d = Diagnostic::error(term.span, "Type mismatch in fold")
                            .message(term.span, format!("unfold requires type {:?}", s))
                            .message(tm.span, format!("term has a type of {:?}", ty_));
                        Err(d)
                    }
                }
                _ => Err(Diagnostic::error(
                    term.span,
                    format!("Expected a recursive type, not {:?}", rec),
                )),
            },
        }
    }
}

pub fn subst(mut s: Type, mut t: Type) -> Type {
    Shift::new(1).visit(&mut s);
    Subst::new(s).visit(&mut t);
    Shift::new(-1).visit(&mut t);
    t
}

struct Aliaser<'ctx> {
    map: &'ctx HashMap<String, Type>,
}

impl<'ctx> MutTypeVisitor for Aliaser<'ctx> {
    fn visit(&mut self, ty: &mut Type) {
        match ty {
            Type::Unit | Type::Bool | Type::Nat => {}
            Type::Var(v) => {}
            Type::Alias(v) => {
                if let Some(aliased) = self.map.get(v) {
                    *ty = aliased.clone();
                }
            }
            Type::Variant(v) => self.visit_variant(v),
            Type::Product(v) => self.visit_product(v),

            Type::Arrow(ty1, ty2) => self.visit_arrow(ty1, ty2),
            Type::Universal(ty) => self.visit_universal(ty),
            Type::Rec(ty) => self.visit_rec(ty),
        }
    }
}

impl MutTermVisitor for Context {
    fn visit_abs(&mut self, sp: &mut Span, ty: &mut Type, term: &mut Term) {
        self.aliaser().visit(ty);
        self.visit(term);
    }

    fn visit_tyapp(&mut self, sp: &mut Span, term: &mut Term, ty: &mut Type) {
        self.aliaser().visit(ty);
        self.visit(term);
    }

    fn visit_injection(
        &mut self,
        sp: &mut Span,
        label: &mut String,
        term: &mut Term,
        ty: &mut Type,
    ) {
        self.aliaser().visit(ty);
        self.visit(term);
    }

    fn visit_fold(&mut self, sp: &mut Span, ty: &mut Type, tm: &mut Term) {
        self.aliaser().visit(ty);
        self.visit(tm);
    }

    fn visit_unfold(&mut self, sp: &mut Span, ty: &mut Type, tm: &mut Term) {
        self.aliaser().visit(ty);
        self.visit(tm);
    }
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Unit => write!(f, "Unit"),
            Type::Bool => write!(f, "Bool"),
            Type::Nat => write!(f, "Nat"),
            Type::Var(v) => write!(f, "TyVar({})", v),
            Type::Variant(v) => write!(
                f,
                "{:?}",
                v.iter()
                    .map(|x| format!("{}: {:?}", x.label, x.ty))
                    .collect::<Vec<String>>()
                    .join(" | ")
            ),
            Type::Product(v) => write!(
                f,
                "({})",
                v.iter()
                    .map(|x| format!("{:?}", x))
                    .collect::<Vec<String>>()
                    .join(",")
            ),
            Type::Alias(s) => write!(f, "{}", s),
            Type::Arrow(t1, t2) => write!(f, "({:?}->{:?})", t1, t2),
            Type::Universal(ty) => write!(f, "forall X.{:?}", ty),
            Type::Rec(ty) => write!(f, "rec {:?}", ty),
        }
    }
}
