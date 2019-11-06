use crate::terms::{Kind, Literal, Pattern, Primitive, Term};
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt;
use util::span::Span;
pub mod visit;
use visit::{MutVisitor, Shift, Subst};

#[derive(Clone, PartialEq, PartialOrd, Eq, Hash)]
pub enum Type {
    Unit,
    Nat,
    Bool,
    Alias(String),
    Var(usize),
    Variant(Vec<Variant>),
    Arrow(Box<Type>, Box<Type>),
    Universal(Box<Type>),
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
    IncompatibleArms,
    NotArrow,
    NotUniversal,
    NotVariant,
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
        crate::terms::visit::MutVisitor::visit(self, term)
    }
}

/// Helper function for extracting type from a variant
fn variant_field(var: &[Variant], label: &str, span: Span) -> Result<Type, TypeError> {
    for f in var {
        if label == &f.label {
            return Ok(f.ty.clone());
        }
    }
    return Err(TypeError {
        span,
        kind: TypeErrorKind::NotVariant,
    });
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
            Kind::Primitive(prim) => match prim {
                Primitive::IsZero => Ok(Type::Arrow(Box::new(Type::Nat), Box::new(Type::Bool))),
                _ => Ok(Type::Arrow(Box::new(Type::Nat), Box::new(Type::Nat))),
            },
            Kind::Constructor(label, tm, ty) => match ty.as_ref() {
                Type::Variant(fields) => {
                    for f in fields {
                        if label == &f.label {
                            let ty_ = self.type_of(tm)?;
                            if ty_ == f.ty {
                                return Ok(*ty.clone());
                            } else {
                                return Context::error(
                                    term,
                                    TypeErrorKind::ParameterMismatch(
                                        Box::new(ty_),
                                        Box::new(f.ty.clone()),
                                        tm.span,
                                    ),
                                );
                            }
                        }
                    }
                    Context::error(term, TypeErrorKind::NotVariant)
                }
                _ => Context::error(term, TypeErrorKind::NotVariant),
            },
            Kind::Case(tm, arms) => {
                let ty = self.type_of(tm)?;
                match &ty {
                    Type::Variant(fields) => {
                        // have we seen a variable or _ pattern?
                        let mut default = false;

                        // set of all possible variant fields
                        let field_set = fields.iter().map(|f| &f.label).collect::<HashSet<_>>();

                        let mut discriminants = HashSet::with_capacity(arms.len());
                        let mut ty_set = HashSet::with_capacity(arms.len());
                        for arm in arms {
                            // Check to see if we have already used a variable or _ pattern,
                            // or if we have covered all discriminants. If so, then this arm
                            // is unreachable
                            if default || field_set.is_subset(&discriminants) {
                                return Context::error(term, TypeErrorKind::UnreachablePattern);
                            }

                            let ty_arm = match &arm.pat {
                                Pattern::Any => {
                                    default = true;
                                    self.type_of(&arm.term)?
                                }
                                Pattern::Variable(_) => {
                                    // Pattern binds some variable, which should
                                    // have a type of the variant
                                    default = true;
                                    self.push(ty.clone());
                                    let ty_arm = self.type_of(&arm.term)?;
                                    self.pop();
                                    ty_arm
                                }
                                Pattern::Constructor(c) => {
                                    let ty_con = variant_field(&fields, c, arm.span)?;

                                    // Set::insert() returns false if the
                                    // element is already present
                                    if !discriminants.insert(c) {
                                        println!("Discriminant {} already used!", c);
                                        return Context::error(
                                            term,
                                            TypeErrorKind::UnreachablePattern,
                                        );
                                    }
                                    self.push(ty_con);
                                    let ty_arm = self.type_of(&arm.term)?;
                                    self.pop();
                                    ty_arm
                                }
                            };
                            ty_set.insert(ty_arm);
                        }

                        // dbg!(&ty_set);

                        if !field_set.is_subset(&discriminants) && !default {
                            println!(
                                "Patterns {:?} not covered",
                                field_set.difference(&discriminants)
                            );
                            return Context::error(term, TypeErrorKind::NotExhaustive);
                        }

                        if ty_set.len() != 1 {
                            println!("Match arms have incompatible types! {:?}", ty_set);
                            return Context::error(term, TypeErrorKind::IncompatibleArms);
                        }
                        for s in ty_set {
                            return Ok(s);
                        }
                        Context::error(term, TypeErrorKind::NotVariant)
                    }
                    _ => Context::error(term, TypeErrorKind::NotVariant),
                }
            }
            Kind::Let(t1, t2) => {
                let ty = self.type_of(t1)?;
                self.push(ty);
                let y = self.type_of(t2);
                self.pop();
                y
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
            Type::Variant(v) => self.visit_variant(v),
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

    fn visit_constructor(&mut self, label: &mut String, term: &mut Term, ty: &mut Type) {
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
            Type::Variant(v) => write!(
                f,
                "{:?}",
                v.iter()
                    .map(|x| format!("{}: {:?}", x.label, x.ty))
                    .collect::<Vec<String>>()
                    .join(" | ")
            ),
            Type::Alias(s) => write!(f, "{}", s),
            Type::Arrow(t1, t2) => write!(f, "({:?}->{:?})", t1, t2),
            Type::Universal(ty) => write!(f, "forall X.{:?}", ty),
        }
    }
}
