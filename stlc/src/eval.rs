use super::term::Term;
use super::typing::{Context, Type, TypeError};

fn map_var<F: Fn(usize, usize) -> Term>(f: &F, c: usize, t: Term) -> Term {
    match t {
        Term::True | Term::False => t,
        Term::Abs(ty, t1) => abs!(ty, map_var(f, c + 1, *t1)),
        Term::App(t1, t2) => app!(map_var(f, c, *t1), map_var(f, c, *t2)),
        Term::If(t1, t2, t3) => if_!(map_var(f, c, *t1), map_var(f, c, *t2), map_var(f, c, *t3)),
        Term::Var(idx) => f(c, idx),
    }
}

fn shift_above(d: isize, c: usize, t: Term) -> Term {
    map_var(
        &|c, x| {
            if x >= c {
                Term::Var(x + d as usize)
            } else {
                Term::Var(x)
            }
        },
        c,
        t,
    )
}

fn shift(d: isize, t: Term) -> Term {
    shift_above(d, 0, t)
}

fn subst(j: usize, s: Term, t: Term) -> Term {
    map_var(
        &|c, x| {
            if x == c {
                shift(j as isize, s.clone())
            } else {
                Term::Var(x)
            }
        },
        j,
        t.clone(),
    )
}

pub fn subst_top(s: Term, t: Term) -> Term {
    shift(-1, subst(0, shift(1, s), t))
}

#[derive(Debug)]
pub enum Error {
    NoRuleApplies,
}

fn value(ctx: &Context, term: &Term) -> bool {
    match term {
        Term::True | Term::False | Term::Abs(_, _) => true,
        _ => false,
    }
}

fn eval1(ctx: &Context, term: Term) -> Result<Term, Error> {
    match term {
        Term::App(t1, t2) if value(ctx, &t2) => {
            if let Term::Abs(ty, body) = *t1 {
                Ok(subst_top(*t2, *body))
            } else {
                Err(Error::NoRuleApplies)
            }
        }
        Term::App(t1, t2) if value(ctx, &t1) => {
            let t_prime = eval1(ctx, *t2)?;
            Ok(Term::App(t1, Box::new(t_prime)))
        }
        Term::App(t1, t2) => {
            let t_prime = eval1(ctx, *t1)?;
            Ok(Term::App(Box::new(t_prime), t2))
        }
        Term::If(guard, csq, alt) => match *guard {
            Term::True => Ok(*csq),
            Term::False => Ok(*alt),
            _ => {
                let t_prime = eval1(ctx, *guard)?;
                Ok(Term::If(Box::new(t_prime), csq, alt))
            }
        },
        _ => Err(Error::NoRuleApplies),
    }
}

pub fn eval(ctx: &Context, term: Term) -> Result<Term, Error> {
    let mut tp = term;
    loop {
        match eval1(ctx, tp.clone()) {
            Ok(r) => tp = r,
            Err(_) => return Ok(tp),
        }
    }
}
