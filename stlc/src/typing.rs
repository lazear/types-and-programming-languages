use crate::term::Term;
use crate::visitor::{Shifting, Substitution, Visitable, Visitor};
use std::fmt;
use std::rc::Rc;

#[derive(Clone, PartialEq, PartialOrd)]
pub enum Type {
    Bool,
    Nat,
    Arrow(Box<Type>, Box<Type>),
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Bool => write!(f, "Bool"),
            Type::Nat => write!(f, "Nat"),
            Type::Arrow(a, b) => write!(f, "{:?}->{:?}", a, b),
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
}

#[derive(Clone, Debug, Default, PartialEq, PartialOrd)]
/// A typing context, Γ
///
/// Much simpler than the binding list suggested in the book, and used
/// in the other directories, but this should be more efficient, and
/// a vec is really overkill here
pub struct Context<'a> {
    parent: Option<&'a Context<'a>>,
    ty: Option<Type>,
}

impl<'a> Visitor<Result<Type, TypeError>> for Context<'a> {
    fn visit_var(&mut self, var: usize) -> Result<Type, TypeError> {
        self.get(var).cloned().ok_or(TypeError::UnknownVariable)
    }

    fn visit_abs(&mut self, ty: Type, body: Rc<Term>) -> Result<Type, TypeError> {
        let mut ctx = self.add(ty.clone());
        let ty_body: Result<Type, TypeError> = body.accept(&mut ctx);
        Ok(Type::Arrow(Box::new(ty.clone()), Box::new(ty_body?)))
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
            Term::Zero => Ok(Type::Nat),
            Term::True | Term::False => Ok(Type::Bool),
            _ => unreachable!(),
        }
    }
}

impl<'a> Context<'a> {
    pub fn add<'ctx>(&'ctx self, ty: Type) -> Context<'ctx> {
        if self.ty.is_none() {
            Context {
                parent: self.parent.clone(),
                ty: Some(ty),
            }
        } else {
            Context {
                parent: Some(self),
                ty: Some(ty),
            }
        }
    }

    pub fn get(&self, idx: usize) -> Option<&Type> {
        if idx == 0 {
            self.ty.as_ref()
        } else {
            if let Some(ctx) = self.parent {
                ctx.get(idx - 1)
            } else {
                None
            }
        }
    }

    pub fn type_of(&self, term: &Term) -> Result<Type, TypeError> {
        use Term::*;
        match term {
            True => Ok(Type::Bool),
            False => Ok(Type::Bool),
            Zero => Ok(Type::Nat),
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

// impl<'a> Visitor<Result<Rc<Term>, TypeError>> for Context<'a> {
//     fn visit_const(&mut self, c: Rc<Term>) -> Result<Rc<Term>, TypeError> {
//         Ok(c)
//     }

//     fn visit_iszero(&mut self, t: Rc<Term>) -> Result<Rc<Term>, TypeError> {
//         match t.as_ref() {
//             Term::Zero => Ok(Term::True.into()),
//             Term::Succ(_) => Ok(Term::False.into()),
//             _ => Ok(Term::IsZero(t.accept(self)?).into()),
//         }
//     }

//     fn visit_pred(&mut self, t: Rc<Term>) -> Result<Rc<Term>, TypeError> {
//         match t.as_ref() {
//             Term::Zero => Ok(t.clone()),
//             Term::Succ(n) => Ok(n.clone()),
//             _ => Ok(Term::Pred(t.accept(self)?).into()),
//         }
//     }

//     fn visit_succ(&mut self, t: Rc<Term>) -> Result<Rc<Term>, TypeError> {
//         let t_prime = t.accept(self)?;
//         Ok(Term::Succ(t_prime).into())
//     }

//     fn visit_if(&mut self, guard: Rc<Term>, csq: Rc<Term>, alt: Rc<Term>) -> Result<Rc<Term>, TypeError> {
//         match &*guard {
//             Term::True => Ok(csq.clone()),
//             Term::False => Ok(alt.clone()),
//             _ => {
//                 let t_prime = guard.accept(self)?;
//                 Ok(Term::If(t_prime, csq.clone(), alt.clone()).into())
//             }
//         }
//     }

//     fn visit_app(&mut self, t1: Rc<Term>, t2: Rc<Term>) ->  Result<Rc<Term>, TypeError>  {
//         match &*t1 {
//             Term::Abs(_, body) => {
//                 let mut sub = Substitution::new(t2.accept(&mut Shifting::default()));
//                                 // Ok(subst_top(t2.clone(), body.clone()))
//                 Ok(body.accept(&mut sub).accept(&mut Shifting::new(-1)))
//             },
//             _ => {
//                 let t_prime = t2.accept(self)?;
//                 Ok(Term::App(t1.clone(), t_prime).into())
//             }
//         }
//     }

//     fn visit_var(&mut self, var: usize) -> Result<Rc<Term>, TypeError> {
//         Err(TypeError::UnknownVariable)
//     }

//     fn visit_abs(&mut self, ty: Type, body: Rc<Term>) ->  Result<Rc<Term>, TypeError> {
//         Err(TypeError::UnknownVariable)
//     }

// // fn eval1(ctx: &Context, term: Rc<Term>) -> Result<Rc<Term>, Error> {
// //     match term.as_ref() {
// //         Term::App(t1, ref t2) if value(ctx, &t2) => {
// //             if let Term::Abs(_, body) = t1.as_ref() {
// //                 let mut sub = Substitution::new(t2.accept(&mut Shifting::default()));
// //                 // Ok(subst_top(t2.clone(), body.clone()))
// //                 Ok(body.accept(&mut sub).accept(&mut Shifting::new(-1)))
// //             } else if value(ctx, &t1) {
// //                 let t_prime = eval1(ctx, t2.clone())?;
// //                 Ok(Term::App(t1.clone(), t_prime).into())
// //             } else {
// //                 let t_prime = eval1(ctx, t1.clone())?;
// //                 Ok(Term::App(t_prime, t2.clone()).into())
// //             }
// //         }
// //         Term::App(t1, t2) if value(ctx, &t1) => {
// //             let t_prime = eval1(ctx, t2.clone())?;
// //             Ok(Term::App(t1.clone(), t_prime).into())
// //         }
// //         Term::App(t1, t2) => {
// //             let t_prime = eval1(ctx, t1.clone())?;
// //             Ok(Term::App(t_prime, t2.clone()).into())
// //         }
// //     }
// // }
// }
