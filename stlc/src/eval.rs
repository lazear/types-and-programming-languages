use super::term::Term;
use super::typing::{Context, Type, TypeError};
use super::visitor::{Shifting, Substitution, Visitable, Visitor};
use std::rc::Rc;

#[derive(Debug)]
pub enum Error {
    NoRuleApplies,
}

fn value(ctx: &Context, term: &Term) -> bool {
    match term {
        Term::True | Term::False | Term::Abs(_, _) | Term::Zero => true,
        Term::Succ(t) | Term::Pred(t) | Term::IsZero(t) => value(ctx, t),
        _ => false,
    }
}

fn eval1(ctx: &Context, term: Rc<Term>) -> Result<Rc<Term>, Error> {
    match term.as_ref() {
        Term::App(t1, ref t2) if value(ctx, &t2) => {
            if let Term::Abs(_, body) = t1.as_ref() {
                let mut sub = Substitution::new(t2.accept(&mut Shifting::default()));
                // Ok(subst_top(t2.clone(), body.clone()))
                Ok(body.accept(&mut sub).accept(&mut Shifting::new(-1)))
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
