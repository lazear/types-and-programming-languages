use super::*;
use std::rc::Rc;

pub trait Visitor<T> {
    fn visit_var(&mut self, var: usize) -> T;
    fn visit_abs(&mut self, ty: Type, body: Rc<Term>) -> T;
    fn visit_app(&mut self, t1: Rc<Term>, t2: Rc<Term>) -> T;
    fn visit_if(&mut self, guard: Rc<Term>, csq: Rc<Term>, alt: Rc<Term>) -> T;
    fn visit_bool(&mut self, val: bool) -> T;
}

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
}

pub struct Substitution {
    pub term: Rc<Term>,
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
        let mut shift = Shifting {
            cutoff: 0,
            shift: 1,
        };

        let _t = self.term.clone();
        self.term = _t.accept(&mut shift);
        let r = Term::Abs(ty, body.accept(self));
        self.term = _t;
        shift.shift = -1;
        r.accept(&mut shift)
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
}
