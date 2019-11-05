use crate::terms::{visit::MutVisitor, visit::Shift, visit::Subst, Kind, Literal, Primitive, Term};
use crate::types::{visit, Context, Type};

pub struct Eval<'ctx> {
    context: &'ctx Context,
}

impl<'ctx> Eval<'ctx> {
    pub fn with_context(context: &Context) -> Eval<'_> {
        Eval { context }
    }

    fn normal_form(&self, term: &Term) -> bool {
        match &term.kind {
            Kind::Lit(_) => true,
            Kind::Abs(_, _) => true,
            Kind::TyAbs(_, _) => true,
            Kind::Primitive(_) => true,
            _ => false,
        }
    }

    pub fn small_step(&self, term: Term) -> Option<Term> {
        match term.kind {
            Kind::App(t1, t2) => {
                if self.normal_form(&t2) {
                    if let Term {
                        kind: Kind::Abs(_, mut abs),
                        ..
                    } = *t1
                    {
                        // We have a B-redex now
                        term_subst(*t2, abs.as_mut());
                        Some(*abs)
                    } else {
                        // assuming `term` is well typed, then t1 must be
                        // beta-reducible to an abstraction. We perform an
                        // additional B-reduction, returning a new App
                        let t = self.small_step(*t1)?;
                        Some(Term::new(Kind::App(Box::new(t), t2), term.span))
                    }
                } else if self.normal_form(&t1) {
                    // t1 is in normal form, but t2 is not, so we will
                    // carry out the reducton t2 -> t2', and return
                    // App(t1, t2')
                    let t = self.small_step(*t2)?;
                    Some(Term::new(Kind::App(t1, Box::new(t)), term.span))
                } else {
                    // Neither t1 nor t2 are in normal form, we reduce t1 first
                    let t = self.small_step(*t1)?;
                    Some(Term::new(Kind::App(Box::new(t), t2), term.span))
                }
            }
            Kind::Let(bind, mut body) => {
                if self.normal_form(&bind) {
                    term_subst(*bind, &mut body);
                    Some(*body)
                } else {
                    let t = self.small_step(*bind)?;
                    Some(Term::new(Kind::Let(Box::new(t), body), term.span))
                }
            }
            Kind::TyApp(tm1, ty) => match tm1.kind {
                Kind::TyAbs(_, tm2) => Some(Term::new(Kind::Abs(ty, tm2), tm1.span)),
                _ => None,
            },
            _ => None,
        }
    }
}

fn term_subst(mut s: Term, t: &mut Term) {
    Shift::new(1).visit(&mut s);
    Subst::new(s).visit(t);
    Shift::new(-1).visit(t);
}
