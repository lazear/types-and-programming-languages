use crate::term::{Field, Term};
use crate::visitor::{Direction, Shifting, Visitor};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use util::span::Span;

#[derive(Clone, PartialEq, PartialOrd)]
pub enum Type {
    Unit,
    Bool,
    Nat,
    Var(String),
    Arrow(Box<Type>, Box<Type>),
    Record(Record),
    Variant(Variant),
}

#[derive(Clone, PartialEq, PartialOrd)]
pub struct Variant {
    // pub span: Span,
    pub ident: String,
    pub data: RecordField,
}

#[derive(Clone, PartialEq, PartialOrd)]
pub struct Record {
    // pub span: Span,
    pub ident: String,
    pub fields: Vec<RecordField>,
}

#[derive(Clone, PartialEq, PartialOrd)]
pub struct RecordField {
    // pub span: Span,
    pub ident: String,
    pub ty: Box<Type>,
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Unit => write!(f, "Unit"),
            Type::Bool => write!(f, "Bool"),
            Type::Nat => write!(f, "Nat"),
            Type::Arrow(a, b) => write!(f, "({:?}->{:?})", a, b),
            Type::Record(r) => write!(
                f,
                "{} {{{}}}",
                r.ident,
                r.fields
                    .iter()
                    .map(|x| format!("{}:{:?}", x.ident, x.ty))
                    .collect::<Vec<String>>()
                    .join(",")
            ),
            Type::Var(s) => write!(f, "{}", s),
            Type::Variant(_) => unimplemented!(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum TypeError {
    Guard,
    ArmMismatch,
    ParameterMismatch,
    UnknownVariable(usize),
    ExpectedArrow,
    InvalidProjection,
    NotRecordType,
    Undefined(String),
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
                parent: None,
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
            Record(fields) => {
                let fields: Vec<RecordField> = fields
                    .iter()
                    .map(|f| {
                        self.type_of(&f.term).map(|ty| {
                            RecordField {
                                // span: f.span,
                                ident: f.ident.clone(),
                                ty: Box::new(ty),
                            }
                        })
                    })
                    .collect::<Result<Vec<RecordField>, TypeError>>()?;

                Ok(Type::Record(crate::typing::Record {
                    // span: Span::dummy(),
                    ident: String::new(),
                    fields,
                }))
            }
            Projection(r, proj) => {
                match self.type_of(r)? {
                    Type::Record(self::Record { fields, .. }) => {
                        for f in &fields {
                            if &f.ident == proj.as_ref() {
                                return Ok(*f.ty.clone());
                            }
                        }
                        Err(TypeError::InvalidProjection)
                    }
                    _ => Err(TypeError::NotRecordType),
                }
            }
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
                _ => Err(TypeError::UnknownVariable(*s)),
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

// impl<'a> Visitor for Context<'a> {
//     fn visit_var(&mut self, var: usize) {
//         self.get(var)
//             .cloned()
//             .ok_or(TypeError::UnknownVariable(var))
//     }

//     fn visit_abs(&mut self, ty: Type, body: &Term) {
//         let ty = match ty {
//             Type::Var(name) => self
//                 .types
//                 .borrow()
//                 .get(&name)
//                 .cloned()
//                 .ok_or(TypeError::Undefined(name))?,
//             x => x,
//         };
//         let mut ctx = self.add(ty.clone());
//         let ty_body: Result<Type, TypeError> = body.accept(&mut ctx);
//         Ok(Type::Arrow(Box::new(ty), Box::new(ty_body?)))
//     }

//     fn visit_app(&mut self, t1: &Term, t2: &Term) {
//         let ty1 = t1.accept(self)?;
//         let ty2 = t2.accept(self)?;
//         match ty1 {
//             Type::Arrow(ty11, ty12) => {
//                 if *ty11 == ty2 {
//                     Ok(*ty12)
//                 } else {
//                     Err(TypeError::ParameterMismatch)
//                 }
//             }
//             _ => Err(TypeError::ExpectedArrow),
//         }
//     }

//     fn visit_if(
//         &mut self,
//         guard: &Term,
//         csq: &Term,
//         alt: &Term,
//     ) {
//         if let Ok(Type::Bool) = guard.accept(self) {
//             let ty1 = csq.accept(self)?;
//             let ty2 = alt.accept(self)?;
//             if ty1 == ty2 {
//                 Ok(ty2)
//             } else {
//                 Err(TypeError::ArmMismatch)
//             }
//         } else {
//             Err(TypeError::Guard)
//         }
//     }

//     fn visit_let(&mut self, bind: &Term, body: &Term) {
//         // Dirty hack or correct behavior?
//         //
//         // We definitely need to correct var indices or how the context is
//         // working so that let binders can access names defined in an
//         // enclosing let-bound scope
//         let ty = bind
//             // .accept(&mut Shifting::new(Direction::Down))
//             .accept(self)?;
//         let mut ctx = self.add(ty);
//         body.accept(&mut ctx)
//     }

//     fn visit_succ(&mut self, t: &Term) {
//         Ok(Type::Nat)
//     }

//     fn visit_pred(&mut self, t: &Term) {
//         Ok(Type::Nat)
//     }

//     fn visit_iszero(&mut self, t: &Term) {
//         Ok(Type::Bool)
//     }

//     fn visit_const(&mut self, c: &Term) {
//         match c.as_ref() {
//             Term::Unit => Ok(Type::Unit),
//             Term::Zero => Ok(Type::Nat),
//             Term::True | Term::False => Ok(Type::Bool),
//             _ => unreachable!(),
//         }
//     }

//     fn visit_record(&mut self, rec: &[RecordField]) {
//         let tys = rec
//             .iter()
//             .map(|f| f.data.accept(self).map(|ty| (f.label.clone(), ty)))
//             .collect::<Result<Vec<(Rc<String>, Type)>, TypeError>>()?;
//         Ok(Type::Record(tys))
//     }

//     fn visit_proj(&mut self, c: &Term, proj: Rc<String>) {
//         match c.accept(self)? {
//             Type::Record(fields) => {
//                 for f in &fields {
//                     if f.0 == proj {
//                         return Ok(f.1.clone());
//                     }
//                 }
//                 Err(TypeError::InvalidProjection)
//             }
//             _ => Err(TypeError::NotRecordType),
//         }
//     }

//     fn visit_typedecl(&mut self, name: Rc<String>, ty: &Type) {
//         self.bind(name.to_string(), ty.clone());
//         Ok(Type::Unit)
//     }
// }
