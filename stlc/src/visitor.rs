use super::*;
use std::default::Default;
use std::rc::Rc;

pub trait Visitable<V, T>
where
    V: Visitor<T>,
{
    fn accept(&self, visitor: &mut V) -> T;
}

pub trait Visitor<T> {
    fn visit_var(&mut self, var: usize) -> T;
    fn visit_abs(&mut self, ty: Type, body: Rc<Term>) -> T;
    fn visit_app(&mut self, t1: Rc<Term>, t2: Rc<Term>) -> T;
    fn visit_if(&mut self, guard: Rc<Term>, csq: Rc<Term>, alt: Rc<Term>) -> T;
    fn visit_succ(&mut self, t: Rc<Term>) -> T;
    fn visit_pred(&mut self, t: Rc<Term>) -> T;
    fn visit_iszero(&mut self, t: Rc<Term>) -> T;
    fn visit_const(&mut self, c: Rc<Term>) -> T;
}

#[derive(Debug)]
pub struct Shifting {
    pub cutoff: usize,
    pub shift: isize,
}

impl Default for Shifting {
    fn default() -> Self {
        Shifting {
            cutoff: 0,
            shift: 1,
        }
    }
}

impl Shifting {
    pub fn new(shift: isize) -> Self {
        Shifting { cutoff: 0, shift }
    }
}

impl Visitor<Rc<Term>> for Shifting {
    fn visit_var(&mut self, var: usize) -> Rc<Term> {
        if var >= self.cutoff {
            Rc::new(Term::Var((var as isize + self.shift) as usize))
        } else {
            Rc::new(Term::Var(var))
        }
    }

    fn visit_abs(&mut self, ty: Type, body: Rc<Term>) -> Rc<Term> {
        // While I understand the rationale for incrementing the
        // cutoff here, I'm also not sure if it's strictly necessary
        // since we later go through and decrement everything by one
        // -- I need to grok the text more
        self.cutoff += 1;
        let inner = body.accept(self);
        self.cutoff -= 1;
        Term::Abs(ty, inner).into()
    }

    fn visit_app(&mut self, t1: Rc<Term>, t2: Rc<Term>) -> Rc<Term> {
        Term::App(t1.accept(self), t2.accept(self)).into()
    }

    fn visit_if(&mut self, guard: Rc<Term>, csq: Rc<Term>, alt: Rc<Term>) -> Rc<Term> {
        Term::If(guard.accept(self), csq.accept(self), alt.accept(self)).into()
    }

    fn visit_succ(&mut self, t: Rc<Term>) -> Rc<Term> {
        Term::Succ(t.accept(self)).into()
    }

    fn visit_pred(&mut self, t: Rc<Term>) -> Rc<Term> {
        Term::Pred(t.accept(self)).into()
    }

    fn visit_iszero(&mut self, t: Rc<Term>) -> Rc<Term> {
        Term::IsZero(t.accept(self)).into()
    }

    fn visit_const(&mut self, c: Rc<Term>) -> Rc<Term> {
        c
    }
}

#[derive(Debug)]
pub struct Substitution {
    pub cutoff: usize,
    pub term: Rc<Term>,
}

impl Substitution {
    pub fn new(term: Rc<Term>) -> Substitution {
        Substitution { cutoff: 0, term }
    }
}

impl Visitor<Rc<Term>> for Substitution {
    fn visit_var(&mut self, var: usize) -> Rc<Term> {
        if var == self.cutoff {
            self.term.accept(&mut Shifting::default())
        } else {
            Rc::new(Term::Var(var))
        }
    }

    fn visit_abs(&mut self, ty: Type, body: Rc<Term>) -> Rc<Term> {
        self.cutoff += 1;
        let r = Term::Abs(ty, body.accept(self));
        self.cutoff -= 1;
        r.into()
    }

    fn visit_app(&mut self, t1: Rc<Term>, t2: Rc<Term>) -> Rc<Term> {
        Term::App(t1.accept(self), t2.accept(self)).into()
    }

    fn visit_if(&mut self, guard: Rc<Term>, csq: Rc<Term>, alt: Rc<Term>) -> Rc<Term> {
        Term::If(guard.accept(self), csq.accept(self), alt.accept(self)).into()
    }

    fn visit_succ(&mut self, t: Rc<Term>) -> Rc<Term> {
        Term::Succ(t.accept(self)).into()
    }

    fn visit_pred(&mut self, t: Rc<Term>) -> Rc<Term> {
        Term::Pred(t.accept(self)).into()
    }

    fn visit_iszero(&mut self, t: Rc<Term>) -> Rc<Term> {
        Term::IsZero(t.accept(self)).into()
    }

    fn visit_const(&mut self, c: Rc<Term>) -> Rc<Term> {
        c
    }
}
