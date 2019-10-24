mod context;
mod lexer;
mod parser;
use parser::Parser;

use context::Context;
use parser::{RcTerm, Term};
use util::span::Span;

fn shift1(d: isize, c: isize, tm: RcTerm) -> RcTerm {
    match &tm as &Term {
        Term::TmVar(sp, x) => {
            if *x as isize >= c {
                Term::TmVar(*sp, *x + d as usize).into()
            } else {
                Term::TmVar(*sp, *x).into()
            }
        }
        Term::TmAbs(sp, x) => Term::TmAbs(*sp, shift1(d, c + 1, x.clone())).into(),
        Term::TmApp(sp, a, b) => {
            Term::TmApp(*sp, shift1(d, c, a.clone()), shift1(d, c, b.clone())).into()
        }
    }
}

fn shift(d: isize, tm: RcTerm) -> RcTerm {
    shift1(d, 0, tm)
}

fn subst_walk(j: isize, s: RcTerm, c: isize, t: RcTerm) -> RcTerm {
    match &t as &Term {
        Term::TmVar(sp, x) => {
            if *x as isize == j + c {
                shift(c, s)
            } else {
                t
            }
        }
        Term::TmAbs(sp, tm) => Term::TmAbs(*sp, subst_walk(j, s, c + 1, tm.clone())).into(),
        Term::TmApp(sp, lhs, rhs) => Term::TmApp(
            *sp,
            subst_walk(j, s.clone(), c, lhs.clone()),
            subst_walk(j, s, c, rhs.clone()),
        )
        .into(),
    }
}

fn subst(j: isize, s: RcTerm, tm: RcTerm) -> RcTerm {
    subst_walk(j, s, 0, tm)
}

fn term_subst_top(s: RcTerm, tm: RcTerm) -> RcTerm {
    shift(-1, subst(0, shift(1, s), tm))
}

fn isval(ctx: &Context, tm: RcTerm) -> bool {
    match &tm as &Term {
        Term::TmAbs(_, _) => true,
        _ => false,
    }
}

fn eval1(ctx: &Context, tm: RcTerm) -> RcTerm {
    match &tm as &Term {
        Term::TmApp(sp, t, v) if isval(ctx, v.clone()) => {
            if let Term::TmAbs(_, t2) = &t as &Term {
                term_subst_top(v.clone(), t2.clone())
            } else {
                panic!("No rule applies!")
            }
        }
        Term::TmApp(sp, v, t) if isval(ctx, v.clone()) => {
            let t_prime = eval1(ctx, t.clone());
            Term::TmApp(*sp, v.clone(), t_prime).into()
        }
        Term::TmApp(sp, t1, t2) => {
            let t_prime = eval1(ctx, t1.clone());
            Term::TmApp(*sp, t_prime, t2.clone()).into()
        }
        _ => panic!("No rule applies!"),
    }
}

fn main() {
    // let input = "(λ x. x x) (λ x. x x) λ x. λ y. y λ x. λ x. x";
    //
    let input = "(λ x. x) (λ y. y)";
    let mut p = Parser::new(input);
    while let Some(tm) = p.parse_term() {
        println!("{:?}", tm);
        dbg!(eval1(p.ctx(), tm));
        // dbg!(term_subst_top(Term::TmVar(Span::default(), 0).into(), tm));
    }

    dbg!(p.ctx());

    let diag = p.diagnostic();
    if diag.error_count() > 0 {
        println!("\n{} error(s) detected while parsing!", diag.error_count());
        println!("{}", diag.emit());
    }
}
