use super::term::*;
use super::typing::{Context};
use super::visitor::{Direction, Shifting, Substitution, Visitable};
use std::rc::Rc;

#[derive(Debug)]
pub enum Error {
    NoRuleApplies,
}

#[inline]
fn subst(val: Rc<Term>, body: Rc<Term>) -> Rc<Term> {
    let mut sub = Substitution::new(val.accept(&mut Shifting::new(Direction::Up)));
    body.accept(&mut sub)
        .accept(&mut Shifting::new(Direction::Down))
}

fn value(ctx: &Context, term: &Term) -> bool {
    match term {
        Term::Unit | Term::True | Term::False | Term::Abs(_, _) | Term::Zero => true,
        Term::Succ(t) | Term::Pred(t) | Term::IsZero(t) => value(ctx, t),
        Term::Record(rec) => {
            for field in rec {
                if !value(ctx, &field.data) {
                    return false;
                }
            }
            true
        }
        _ => false,
    }
}

fn eval1(ctx: &Context, term: Rc<Term>) -> Result<Rc<Term>, Error> {
    match term.as_ref() {
        Term::App(t1, ref t2) if value(ctx, &t2) => {
            if let Term::Abs(_, body) = t1.as_ref() {
                Ok(subst(t2.clone(), body.clone()))
            } else if value(ctx, &t1) {
                let t_prime = eval1(ctx, t2.clone())?;
                Ok(Term::App(t1.clone(), t_prime).into())
            } else {
                let t_prime = eval1(ctx, t1.clone())?;
                Ok(Term::App(t_prime, t2.clone()).into())
            }
        }
        Term::App(t1, t2) if value(ctx, &t1) => {
            let t_prime = eval1(ctx, t2.clone())?;
            Ok(Term::App(t1.clone(), t_prime).into())
        }
        Term::App(t1, t2) => {
            let t_prime = eval1(ctx, t1.clone())?;
            Ok(Term::App(t_prime, t2.clone()).into())
        }
        Term::If(guard, csq, alt) => match **guard {
            Term::True => Ok(csq.clone()),
            Term::False => Ok(alt.clone()),
            _ => {
                let t_prime = eval1(ctx, guard.clone())?;
                Ok(Term::If(t_prime, csq.clone(), alt.clone()).into())
            }
        },
        Term::Let(bind, body) => {
            if value(ctx, bind) {
                Ok(subst(bind.clone(), body.clone()))
            } else {
                let t = eval1(ctx, bind.clone())?;
                Ok(Term::Let(t, body.clone()).into())
            }
        }
        Term::Succ(t) => {
            let t_prime = eval1(ctx, t.clone())?;
            Ok(Term::Succ(t_prime).into())
        }

        Term::Pred(t) => match t.as_ref() {
            Term::Zero => Ok(t.clone()),
            Term::Succ(n) => Ok(n.clone()),
            _ => Ok(Term::Pred(eval1(ctx, t.clone())?).into()),
        },

        Term::IsZero(t) => match t.as_ref() {
            Term::Zero => Ok(Term::True.into()),
            Term::Succ(_) => Ok(Term::False.into()),
            _ => Ok(Term::IsZero(eval1(ctx, t.clone())?).into()),
        },

        Term::Projection(rec, proj) if value(ctx, rec) => match rec.as_ref() {
            Term::Record(rec) => crate::term::record_access(rec, proj).ok_or(Error::NoRuleApplies),
            _ => Ok(Term::Projection(eval1(ctx, rec.clone())?, proj.clone()).into()),
        },

        Term::Projection(rec, proj) => {
            Ok(Term::Projection(eval1(ctx, rec.clone())?, proj.clone()).into())
        }

        // Term::TypeDecl(name, ty) => {
        //     ctx.bind(name.to_string(), ty.clone());
        //     Ok(Term::Unit.into())
        // }
        _ => Err(Error::NoRuleApplies),
    }
}

pub fn eval(ctx: &Context, term: Rc<Term>) -> Result<Rc<Term>, Error> {
    let mut tp = term;
    loop {
        println!("  -> {}", &tp);
        match eval1(ctx, tp.clone()) {
            Ok(r) => tp = r,
            Err(e) => {
                return Ok(tp);
            }
        }
    }
}

// pub struct Evaluator<'ctx> {
//     pub context: &'ctx Context<'ctx>,
// }

// impl<'a> Visitor<Result<Rc<Term>, TypeError>> for Evaluator<'a> {
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

//     fn visit_if(
//         &mut self,
//         guard: Rc<Term>,
//         csq: Rc<Term>,
//         alt: Rc<Term>,
//     ) -> Result<Rc<Term>, TypeError> {
//         match &*guard {
//             Term::True => Ok(csq.clone()),
//             Term::False => Ok(alt.clone()),
//             _ => {
//                 let t_prime = guard.accept(self)?;
//                 Ok(Term::If(t_prime, csq.clone(), alt.clone()).into())
//             }
//         }
//     }

//     fn visit_app(&mut self, t1: Rc<Term>, t2: Rc<Term>) -> Result<Rc<Term>, TypeError> {
//         match &*t1 {
//             Term::Abs(_, body) => {
//                 let mut sub = Substitution::new(t2.accept(&mut Shifting::new(Direction::Up)));
//                 Ok(body
//                     .accept(&mut sub)
//                     .accept(&mut Shifting::new(Direction::Down)))
//             }
//             _ => {
//                 let t_prime = t2.accept(self)?;
//                 Ok(Term::App(t1.clone(), t_prime).into())
//             }
//         }
//     }

//     fn visit_let(&mut self, bind: Rc<Term>, body: Rc<Term>) -> Result<Rc<Term>, TypeError> {
//         Err(TypeError::UnknownVariable)
//     }

//     fn visit_var(&mut self, var: usize) -> Result<Rc<Term>, TypeError> {
//         Err(TypeError::UnknownVariable)
//     }

//     fn visit_abs(&mut self, ty: Type, body: Rc<Term>) -> Result<Rc<Term>, TypeError> {
//         Err(TypeError::UnknownVariable)
//     }

//     fn visit_record(&mut self, rec: Rc<Record>) -> Rc<Term> {
//         Err(TypeError::UnknownVariable)
//     }
// }
