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

    fn eval_primitive(&self, p: Primitive, mut term: Term) -> Option<Term> {
        fn map<F: Fn(u32) -> u32>(f: F, mut term: Term) -> Option<Term> {
            match &term.kind {
                Kind::Lit(Literal::Nat(n)) => {
                    term.kind = Kind::Lit(Literal::Nat(f(*n)));
                    Some(term)
                }
                _ => None,
            }
        }

        match p {
            Primitive::Succ => map(|l| l + 1, term),
            Primitive::Pred => map(|l| l.saturating_sub(1), term),
            Primitive::IsZero => match &term.kind {
                Kind::Lit(Literal::Nat(0)) => {
                    Some(Term::new(Kind::Lit(Literal::Bool(true)), term.span))
                }
                _ => Some(Term::new(Kind::Lit(Literal::Bool(false)), term.span)),
            },
        }
    }

    pub fn small_step(&self, term: Term) -> Option<Term> {
        match term.kind {
            Kind::App(t1, t2) => {
                if self.normal_form(&t2) {
                    match t1.kind {
                        Kind::Abs(_, mut abs) => {
                            term_subst(*t2, abs.as_mut());
                            Some(*abs)
                        }
                        Kind::Primitive(p) => self.eval_primitive(p, *t2),
                        _ => {
                            let t = self.small_step(*t1)?;
                            Some(Term::new(Kind::App(Box::new(t), t2), term.span))
                        }
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
