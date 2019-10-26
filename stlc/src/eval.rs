use super::term::Term;
use super::typing::{Context, Type, TypeError};
use super::visitor::Substitution;
use std::rc::Rc;

fn map_var<F: Fn(usize, usize) -> Rc<Term>>(f: &F, c: usize, t: Rc<Term>) -> Rc<Term> {
    match t.as_ref() {
        Term::True | Term::False | Term::Zero => t,
        Term::Pred(t) => Term::Pred(map_var(f, c, t.clone())).into(),
        Term::Succ(t) => Term::Succ(map_var(f, c, t.clone())).into(),
        Term::IsZero(t) => Term::IsZero(map_var(f, c, t.clone())).into(),
        Term::Abs(ty, t1) => Term::Abs(ty.clone(), map_var(f, c + 1, t1.clone())).into(),
        Term::App(t1, t2) => Term::App(map_var(f, c, t1.clone()), map_var(f, c, t2.clone())).into(),
        Term::If(t1, t2, t3) => Term::If(
            map_var(f, c, t1.clone()),
            map_var(f, c, t2.clone()),
            map_var(f, c, t3.clone()),
        )
        .into(),
        Term::Var(idx) => f(c, *idx).into(),
    }
}

fn shift_above(d: isize, c: usize, t: Rc<Term>) -> Rc<Term> {
    map_var(
        &|c, x| {
            if x >= c {
                Term::Var(x + d as usize).into()
            } else {
                Term::Var(x).into()
            }
        },
        c,
        t,
    )
}

fn shift(d: isize, t: Rc<Term>) -> Rc<Term> {
    shift_above(d, 0, t)
}

fn subst(j: usize, s: Rc<Term>, t: Rc<Term>) -> Rc<Term> {
    map_var(
        &|c, x| {
            if x == c {
                shift(j as isize, s.clone())
            } else {
                Term::Var(x).into()
            }
        },
        j,
        t.clone(),
    )
}

pub fn subst_top(s: Rc<Term>, t: Rc<Term>) -> Rc<Term> {
    shift(-1, subst(0, shift(1, s), t))
}

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
                let mut sub = Substitution::new(t2.clone());
                Ok(subst_top(t2.clone(), body.clone()))
            // Ok(body.accept(&mut sub))
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
