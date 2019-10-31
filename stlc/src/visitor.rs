use super::*;
use crate::term::{Field, Term};
use std::default::Default;
use std::rc::Rc;

pub trait Visitor: Sized {
    fn visit_var(&mut self, var: usize);
    fn visit_abs(&mut self, ty: Type, body: &Term);
    fn visit_app(&mut self, t1: &Term, t2: &Term);
    fn visit_if(&mut self, guard: &Term, csq: &Term, alt: &Term);
    fn visit_let(&mut self, bind: &Term, body: &Term);
    fn visit_succ(&mut self, t: &Term);
    fn visit_pred(&mut self, t: &Term);
    fn visit_iszero(&mut self, t: &Term);
    fn visit_const(&mut self, c: &Term);
    fn visit_record(&mut self, c: &[Field]);
    fn visit_proj(&mut self, c: &Term, proj: &String);
    fn visit_typedecl(&mut self, name: &String, ty: &Type);
}

pub trait MutVisitor: Sized {
    fn visit_var(&mut self, var: &mut Term) {}

    fn visit_abs(&mut self, ty: &mut Type, body: &mut Term) {
        self.visit_term(body);
    }
    fn visit_app(&mut self, t1: &mut Term, t2: &mut Term) {
        self.visit_term(t1);
        self.visit_term(t2);
    }
    fn visit_if(&mut self, guard: &mut Term, csq: &mut Term, alt: &mut Term) {
        self.visit_term(guard);
        self.visit_term(csq);
        self.visit_term(alt);
    }
    fn visit_let(&mut self, bind: &mut Term, body: &mut Term) {
        self.visit_term(bind);
        self.visit_term(body);
    }
    fn visit_succ(&mut self, t: &mut Term) {
        self.visit_term(t);
    }
    fn visit_pred(&mut self, t: &mut Term) {
        self.visit_term(t);
    }
    fn visit_iszero(&mut self, t: &mut Term) {
        self.visit_term(t);
    }
    fn visit_const(&mut self, t: &mut Term) {}
    fn visit_record(&mut self, c: &mut [Field]) {
        for t in c {
            self.visit_term(t.term.as_mut());
        }
    }
    fn visit_proj(&mut self, t: &mut Term, proj: &mut String) {
        self.visit_term(t);
    }
    fn visit_typedecl(&mut self, name: &mut String, ty: &mut Type) {}

    fn visit_term(&mut self, term: &mut Term) {
        walk_mut_term(self, term);
    }
}

fn walk_mut_term<V: MutVisitor>(visitor: &mut V, var: &mut Term) {
    match var {
        Term::Unit | Term::True | Term::False | Term::Zero => visitor.visit_const(var),
        Term::Succ(t) => visitor.visit_succ(t),
        Term::Pred(t) => visitor.visit_pred(t),
        Term::IsZero(t) => visitor.visit_iszero(t),
        Term::Var(_) => visitor.visit_var(var),
        Term::Abs(ty, body) => visitor.visit_abs(ty, body),
        Term::App(t1, t2) => visitor.visit_app(t1, t2),
        Term::If(a, b, c) => visitor.visit_if(a, b, c),
        Term::Let(bind, body) => visitor.visit_let(bind, body),
        Term::Record(rec) => visitor.visit_record(rec),
        Term::Projection(rec, idx) => visitor.visit_proj(rec, idx),
        Term::TypeDecl(name, ty) => visitor.visit_typedecl(name, ty),
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Direction {
    Up,
    Down,
}

#[derive(Copy, Clone, Debug)]
pub struct Shifting {
    pub cutoff: usize,
    pub direction: Direction,
}

impl Default for Shifting {
    fn default() -> Self {
        Shifting {
            cutoff: 0,
            direction: Direction::Up,
        }
    }
}

impl Shifting {
    pub fn new(direction: Direction) -> Self {
        Shifting {
            cutoff: 0,
            direction,
        }
    }
}

impl MutVisitor for Shifting {
    fn visit_var(&mut self, var: &mut Term) {
        let mut n = match var {
            Term::Var(n) => n,
            _ => unreachable!(),
        };

        if *n >= self.cutoff {
            // NB: Substracting 1 from the usize here is safe, as long as
            // a shift Down is only called *after* a shift/substitute cycle
            match self.direction {
                Direction::Up => *n += 1,
                Direction::Down => *n -= 1,
            }
        }
    }

    fn visit_abs(&mut self, ty_: &mut Type, body: &mut Term) {
        self.cutoff += 1;
        self.visit_term(body);
        self.cutoff -= 1;
    }

    fn visit_let(&mut self, bind: &mut Term, body: &mut Term) {
        self.cutoff += 1;
        self.visit_term(bind);
        self.visit_term(body);
        self.cutoff -= 1;
    }
}

#[derive(Debug)]
pub struct Substitution {
    pub cutoff: usize,
    pub term: Term,
}

impl Substitution {
    pub fn new(term: Term) -> Substitution {
        Substitution { cutoff: 0, term }
    }
}

impl MutVisitor for Substitution {
    fn visit_var(&mut self, var: &mut Term) {
        match var {
            Term::Var(n) if *n >= self.cutoff => {
                *var = self.term.clone();
            }
            _ => unreachable!(),
        }
    }

    fn visit_abs(&mut self, ty_: &mut Type, body: &mut Term) {
        self.cutoff += 1;
        walk_mut_term(self, body);
        self.cutoff -= 1;
    }

    fn visit_let(&mut self, bind: &mut Term, body: &mut Term) {
        self.cutoff += 1;
        walk_mut_term(self, bind);
        walk_mut_term(self, body);
        self.cutoff -= 1;
    }
}
