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
    fn visit_let(&mut self, bind: Rc<Term>, body: Rc<Term>) -> T;
    fn visit_succ(&mut self, t: Rc<Term>) -> T;
    fn visit_pred(&mut self, t: Rc<Term>) -> T;
    fn visit_iszero(&mut self, t: Rc<Term>) -> T;
    fn visit_const(&mut self, c: Rc<Term>) -> T;
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

impl Visitor<Rc<Term>> for Shifting {
    fn visit_var(&mut self, var: usize) -> Rc<Term> {
        if var >= self.cutoff {
            // NB: Substracting 1 from the usize here is safe, as long as
            // a shift Down is only called *after* a shift/substitute cycle
            match self.direction {
                Direction::Up => Term::Var(var + 1).into(),
                Direction::Down => Term::Var(var - 1).into(),
            }
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

    fn visit_let(&mut self, bind: Rc<Term>, body: Rc<Term>) -> Rc<Term> {
        self.cutoff += 1;
        let bind_ = bind.accept(self);

        let body_ = body.accept(self);
        self.cutoff -= 1;
        Term::Let(bind_, body_).into()
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

    fn visit_let(&mut self, bind: Rc<Term>, body: Rc<Term>) -> Rc<Term> {
        self.cutoff += 1;
        let bind_ = bind.accept(self);
        let body_ = body.accept(self);
        self.cutoff -= 1;
        Term::Let(bind_, body_).into()
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
