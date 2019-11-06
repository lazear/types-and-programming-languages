use super::term::*;
use super::typing::Context;
use super::visitor::{Direction, MutVisitor, Shifting, Substitution};

#[derive(Debug)]
pub enum Error {
    NoRuleApplies,
}

#[inline]
fn subst(mut val: Term, body: &mut Term) {
    Shifting::new(Direction::Up).visit_term(&mut val);
    Substitution::new(val).visit_term(body);
    Shifting::new(Direction::Down).visit_term(body);
}

fn value(ctx: &Context, term: &Term) -> bool {
    match term {
        Term::Unit | Term::True | Term::False | Term::Abs(_, _) | Term::Zero => true,
        Term::Succ(t) | Term::Pred(t) | Term::IsZero(t) => value(ctx, t),
        Term::Record(fields) => {
            for field in fields {
                if !value(ctx, &field.term) {
                    return false;
                }
            }
            true
        }
        _ => false,
    }
}

fn eval1(ctx: &Context, term: Term) -> Result<Box<Term>, Error> {
    match term {
        Term::App(t1, t2) => {
            if value(ctx, &t2) {
                match *t1 {
                    Term::Abs(_, mut abs) => {
                        subst(*t2, abs.as_mut());
                        Ok(abs)
                    }
                    _ => {
                        let t_prime = eval1(ctx, *t1)?;
                        Ok(Term::App(t_prime, t2).into())
                    }
                }
            } else if value(ctx, &t1) {
                let t_prime = eval1(ctx, *t2)?;
                Ok(Term::App(t1.clone(), t_prime).into())
            } else {
                let t_prime = eval1(ctx, *t1)?;
                Ok(Term::App(t_prime, t2.clone()).into())
            }
        }
        Term::If(guard, csq, alt) => match &*guard {
            Term::True => Ok(csq.clone()),
            Term::False => Ok(alt.clone()),
            _ => {
                let t_prime = eval1(ctx, *guard)?;
                Ok(Term::If(t_prime, csq.clone(), alt.clone()).into())
            }
        },
        Term::Let(bind, mut body) => {
            if value(ctx, &bind) {
                subst(*bind, body.as_mut());
                Ok(body)
            } else {
                let t = eval1(ctx, *bind)?;
                Ok(Term::Let(t, body.clone()).into())
            }
        }
        Term::Succ(t) => {
            let t_prime = eval1(ctx, *t)?;
            Ok(Term::Succ(t_prime).into())
        }

        Term::Pred(t) => match t.as_ref() {
            Term::Zero => Ok(t.clone()),
            Term::Succ(n) => Ok(n.clone()),
            _ => Ok(Term::Pred(eval1(ctx, *t)?).into()),
        },

        Term::IsZero(t) => match t.as_ref() {
            Term::Zero => Ok(Term::True.into()),
            Term::Succ(_) => Ok(Term::False.into()),
            _ => Ok(Term::IsZero(eval1(ctx, *t)?).into()),
        },

        Term::Projection(rec, proj) => {
            if value(ctx, &rec) {
                match rec.as_ref() {
                    Term::Record(rec) => {
                        crate::term::record_access(rec, &proj).ok_or(Error::NoRuleApplies)
                    }
                    _ => Ok(Term::Projection(eval1(ctx, *rec)?, proj.clone()).into()),
                }
            } else {
                Ok(Term::Projection(eval1(ctx, *rec)?, proj.clone()).into())
            }
        }

        _ => Err(Error::NoRuleApplies),
    }
}

pub fn eval(ctx: &Context, term: Term) -> Result<Term, Error> {
    let mut tp = term;
    loop {
        println!("  -> {}", &tp);
        match eval1(ctx, tp.clone()) {
            Ok(r) => tp = *r,
            Err(e) => {
                return Ok(tp);
            }
        }
    }
}
