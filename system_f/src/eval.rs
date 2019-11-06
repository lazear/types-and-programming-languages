use crate::terms::{self, Kind, Literal, Pattern, Primitive, Term};
use crate::types::{self, Context, Type};

pub struct Eval<'ctx> {
    _context: &'ctx Context,
}

impl<'ctx> Eval<'ctx> {
    pub fn with_context(_context: &Context) -> Eval<'_> {
        Eval { _context }
    }

    fn normal_form(&self, term: &Term) -> bool {
        match &term.kind {
            Kind::Lit(_) => true,
            Kind::Abs(_, _) => true,
            Kind::TyAbs(_) => true,
            Kind::Primitive(_) => true,
            Kind::Constructor(_, tm, _) => self.normal_form(tm),
            _ => false,
        }
    }

    fn eval_primitive(&self, p: Primitive, term: Term) -> Option<Term> {
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
            Kind::TyApp(tm, ty) => match tm.kind {
                Kind::TyAbs(mut tm2) => {
                    type_subst(*ty, &mut tm2);
                    Some(*tm2)
                }
                _ => {
                    let t_prime = self.small_step(*tm)?;
                    Some(Term::new(Kind::TyApp(Box::new(t_prime), ty), term.span))
                }
            },
            Kind::Constructor(label, tm, ty) => {
                let t_prime = self.small_step(*tm)?;
                Some(Term::new(
                    Kind::Constructor(label, Box::new(t_prime), ty),
                    term.span,
                ))
            }
            Kind::Case(expr, arms) => {
                match expr.kind() {
                    Kind::Lit(lit) => {
                        for mut arm in arms {
                            match arm.pat {
                                Pattern::Any => return Some(*arm.term),
                                Pattern::Variable(_) => {
                                    // Variable should be bound to Kind::Var(0),
                                    // so we want to do a term substition of expr
                                    // into Var(0)
                                    // case ex of
                                    //    | x => (cons x 1),
                                    term_subst(*expr, arm.term.as_mut());
                                    dbg!(&arm.term);
                                    return Some(*arm.term);
                                }
                                Pattern::Literal(l) => {
                                    if lit == &l {
                                        return Some(*arm.term);
                                    }
                                }
                                Pattern::Constructor(_) => return None,
                            }
                        }
                        None
                    }
                    Kind::Constructor(label, tm, ty) => {
                        for mut arm in arms {
                            match arm.pat {
                                Pattern::Any => return Some(*arm.term),
                                Pattern::Variable(_) => {
                                    // Variable should be bound to Kind::Var(0),
                                    // so we want to do a term substition of expr
                                    // into Var(0)
                                    // case ex of
                                    //    | x => (cons x 1),
                                    term_subst(*expr, arm.term.as_mut());
                                    dbg!(&arm.term);
                                    return Some(*arm.term);
                                }
                                Pattern::Constructor(tag) => {
                                    if label == &tag {
                                        // If the constructor is bound to a type
                                        // that is not unit, we should term subst
                                        // in the constructor's bound term to
                                        // Var(0) so that the term in the arm
                                        // can access it
                                        match ty.as_ref() {
                                            Type::Unit => {}
                                            _ => term_subst(*tm.clone(), arm.term.as_mut()),
                                        }

                                        return Some(*arm.term);
                                    }
                                }
                                Pattern::Literal(_) => return None,
                            }
                        }
                        None
                    }
                    _ => {
                        let t_prime = self.small_step(*expr)?;
                        Some(Term::new(Kind::Case(Box::new(t_prime), arms), term.span))
                    }
                }
            }

            _ => None,
        }
    }
}

fn term_subst(mut s: Term, t: &mut Term) {
    use terms::visit::*;
    Shift::new(1).visit(&mut s);
    Subst::new(s).visit(t);
    Shift::new(-1).visit(t);
}

fn type_subst(mut s: Type, t: &mut Term) {
    use types::visit::*;
    Shift::new(1).visit(&mut s);
    let mut s = TyTermSubst {
        subst: Subst::new(s),
    };

    terms::visit::MutVisitor::visit(&mut s, t);
    terms::visit::MutVisitor::visit(&mut terms::visit::Shift::new(-1), t);
}

struct TyTermSubst {
    subst: types::visit::Subst,
}

use types::visit::MutVisitor;
use util::span::Span;

impl terms::visit::MutVisitor for TyTermSubst {
    fn visit_abs(&mut self, sp: &mut Span, ty: &mut Type, term: &mut Term) {
        self.subst.visit(ty);
        self.visit(term);
    }

    fn visit_tyapp(&mut self, sp: &mut Span, term: &mut Term, ty: &mut Type) {
        self.subst.visit(ty);
        self.visit(term);
    }

    fn visit_constructor(&mut self, label: &mut String, term: &mut Term, ty: &mut Type) {
        self.subst.visit(ty);
        self.visit(term);
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn eval_literal() {
        let ctx = crate::types::Context::default();
        let eval = Eval::with_context(&ctx);
        assert_eq!(eval.small_step(lit!(false)), None);
    }

    #[test]
    fn eval_application() {
        let ctx = crate::types::Context::default();
        let eval = Eval::with_context(&ctx);
        let tm = app!(
            abs!(Type::Nat, app!(prim!(Primitive::Succ), var!(0))),
            nat!(1)
        );

        let t1 = eval.small_step(tm);
        assert_eq!(t1, Some(app!(prim!(Primitive::Succ), nat!(1))));
        let t2 = eval.small_step(t1.unwrap());
        assert_eq!(t2, Some(nat!(2)));
        let t3 = eval.small_step(t2.unwrap());
        assert_eq!(t3, None);
    }

    #[test]
    fn eval_type_application() {
        let ctx = crate::types::Context::default();
        let eval = Eval::with_context(&ctx);
        let tm = tyapp!(
            tyabs!(abs!(Type::Var(0), app!(prim!(Primitive::Succ), var!(0)))),
            Type::Nat
        );

        let t1 = eval.small_step(tm);
        assert_eq!(
            t1,
            Some(abs!(Type::Nat, app!(prim!(Primitive::Succ), var!(0))))
        );
        let t2 = eval.small_step(t1.unwrap());
        assert_eq!(t2, None);
    }
}
