mod lexer;
mod parser;
mod span;

use parser::{Parser, Term};

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
pub enum RuntimeError {
    NoRuleApplies,
}

impl Term {
    pub fn is_numeric(&self) -> bool {
        match self {
            Term::TmZero => true,
            Term::TmSucc(t) => t.is_numeric(),
            _ => false,
        }
    }

    pub fn is_normal(&self) -> bool {
        match self {
            Term::TmZero | Term::TmTrue | Term::TmFalse => true,
            _ => false,
        }
    }
}

pub fn eval1(t: Term) -> Result<Term, RuntimeError> {
    use Term::*;
    let res = match t {
        TmIf(cond, csq, alt) => match *cond {
            TmFalse => *alt,
            TmTrue => *csq,
            _ => TmIf(Box::new(eval1(*cond)?), csq, alt),
        },
        TmSucc(term) => TmSucc(Box::new(eval1(*term)?)),
        TmPred(term) => match *term {
            TmZero => TmZero,
            TmSucc(nv) => {
                if nv.is_numeric() {
                    *nv
                } else {
                    return Err(RuntimeError::NoRuleApplies);
                }
            }
            _ => TmPred(Box::new(eval1(*term)?)),
        },
        TmIsZero(term) => match *term {
            TmZero => TmTrue,
            TmSucc(nv) => {
                if nv.is_numeric() {
                    TmFalse
                } else {
                    return Err(RuntimeError::NoRuleApplies);
                }
            }
            _ => TmIsZero(Box::new(eval1(*term)?)),
        },
        _ => return Err(RuntimeError::NoRuleApplies),
    };
    Ok(res)
}

pub fn eval(t: Term) -> Term {
    let mut r = t;
    while let Ok(tprime) = eval1(r.clone()) {
        r = tprime;
        if r.is_normal() {
            break;
        }
    }
    r
}

fn main() {
    let input = "succ(succ(succ(succ(0)))); iszero(0); iszero(1); iszero(pred(succ(0))";
    let mut p = Parser::new(input);
    while let Some(tm) = p.parse_term() {
        print!("{:?} ==> ", tm);
        println!("{:?}", eval(tm));
    }

    let diag = p.diagnostic();
    if diag.error_count() > 0 {
        println!("\n{} error(s) detected while parsing!", diag.error_count());
        println!("{}", diag.emit());
    }
}
