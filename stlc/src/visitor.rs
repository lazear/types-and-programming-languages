use super::*;
use std::rc::Rc;

pub trait Visitor<T> {
    fn visit_var(&mut self, var: usize) -> T;
    fn visit_abs(&mut self, ty: Type, body: Rc<Term>) -> T;
    fn visit_app(&mut self, t1: Rc<Term>, t2: Rc<Term>) -> T;
    fn visit_if(&mut self, guard: Rc<Term>, csq: Rc<Term>, alt: Rc<Term>) -> T;
    fn visit_bool(&mut self, val: bool) -> T;
    fn visit_nat(&mut self, nat: u32) -> T;
}

#[derive(Debug)]
pub struct Shifting {
    pub cutoff: usize,
    pub shift: isize,
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

    fn visit_bool(&mut self, val: bool) -> Rc<Term> {
        match val {
            true => Rc::new(Term::True),
            false => Rc::new(Term::False),
        }
    }

    fn visit_nat(&mut self, val: u32) -> Rc<Term> {
        Term::Zero.into()
    }
}

#[derive(Debug)]
pub struct Substitution {
    pub term: Rc<Term>,
    pub shift: Shifting,
}

impl Substitution {
    pub fn new(term: Rc<Term>) -> Substitution {
        Substitution {
            term,
            shift: Shifting {
                cutoff: 0,
                shift: 1,
            },
        }
    }
}

impl Visitor<Rc<Term>> for Substitution {
    fn visit_var(&mut self, var: usize) -> Rc<Term> {
        if var == 0 {
            self.term.clone()
        } else {
            Rc::new(Term::Var(var))
        }
    }

    fn visit_abs(&mut self, ty: Type, body: Rc<Term>) -> Rc<Term> {
        let _t = self.term.clone();
        self.term = _t.accept(&mut self.shift);
        // dbg!(&body);
        dbg!(&self.term);
        let r = Term::Abs(ty, body.accept(self));
        // dbg!(&r);
        self.term = _t;
        self.shift.shift = -1;
        r.accept(&mut self.shift)
    }

    fn visit_app(&mut self, t1: Rc<Term>, t2: Rc<Term>) -> Rc<Term> {
        Term::App(t1.accept(self), t2.accept(self)).into()
    }

    fn visit_if(&mut self, guard: Rc<Term>, csq: Rc<Term>, alt: Rc<Term>) -> Rc<Term> {
        Term::If(guard.accept(self), csq.accept(self), alt.accept(self)).into()
    }

    fn visit_bool(&mut self, val: bool) -> Rc<Term> {
        match val {
            true => Rc::new(Term::True),
            false => Rc::new(Term::False),
        }
    }

    fn visit_nat(&mut self, val: u32) -> Rc<Term> {
        Term::Zero.into()
    }
}
