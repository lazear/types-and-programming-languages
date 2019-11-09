use crate::terms::visit::{MutVisitor, Shift, Subst, TyTermSubst};
use crate::terms::{self, Kind, Literal, Pattern, Primitive, Term};
use crate::types::{self, Context, Type};
use util::span::Span;

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
            Kind::Injection(_, tm, _) => self.normal_form(tm),
            Kind::Product(fields) => fields.iter().all(|f| self.normal_form(f)),
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
        if self.normal_form(&term) {
            return None;
        }
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
            Kind::Injection(label, tm, ty) => {
                let t_prime = self.small_step(*tm)?;
                Some(Term::new(
                    Kind::Injection(label, Box::new(t_prime), ty),
                    term.span,
                ))
            }
            Kind::Projection(tm, idx) => {
                if self.normal_form(&tm) {
                    match tm.kind {
                        // Typechecker ensures that idx is in bounds
                        Kind::Product(terms) => terms.get(idx).cloned(),
                        _ => None,
                    }
                } else {
                    let t_prime = self.small_step(*tm)?;
                    Some(Term::new(
                        Kind::Projection(Box::new(t_prime), idx),
                        term.span,
                    ))
                }
            }
            Kind::Product(terms) => {
                let mut v = Vec::with_capacity(terms.len());
                for term in terms {
                    if self.normal_form(&term) {
                        v.push(term);
                    } else {
                        v.push(self.small_step(term)?);
                    }
                }
                Some(Term::new(Kind::Product(v), term.span))
            }
            Kind::Case(expr, arms) => {
                if !self.normal_form(&expr) {
                    let t_prime = self.small_step(*expr)?;
                    return Some(Term::new(Kind::Case(Box::new(t_prime), arms), term.span));
                }

                for mut arm in arms {
                    if arm.pat.matches(&expr) {
                        self.case_subst(&arm.pat, &expr, arm.term.as_mut());
                        return Some(*arm.term);
                    }
                }

                None
            }
            // Kind::Case(expr, arms) => {
            //     match expr.kind() {
            //         Kind::Lit(lit) => {
            //             for mut arm in arms {
            //                 match arm.pat {
            //                     Pattern::Any => return Some(*arm.term),
            //                     Pattern::Variable(_) => {
            //                         // Variable should be bound to Kind::Var(0),
            //                         // so we want to do a term substition of expr
            //                         // into Var(0)
            //                         // case ex of
            //                         //    | x => (cons x 1),
            //                         term_subst(*expr, arm.term.as_mut());
            //                         dbg!(&arm.term);
            //                         return Some(*arm.term);
            //                     }
            //                     Pattern::Literal(l) => {
            //                         if lit == &l {
            //                             return Some(*arm.term);
            //                         }
            //                     }
            //                     Pattern::Constructor(_, _) => return None,
            //                     Pattern::Product(_) => unimplemented!(),
            //                 }
            //             }
            //             None
            //         }
            //         Kind::Injection(label, tm, ty) => {
            //             for mut arm in arms {
            //                 match arm.pat {
            //                     Pattern::Any => return Some(*arm.term),
            //                     Pattern::Variable(_) => {
            //                         // Variable should be bound to Kind::Var(0),
            //                         // so we want to do a term substition of expr
            //                         // into Var(0)
            //                         // case ex of
            //                         //    | x => (cons x 1),
            //                         term_subst(*expr, arm.term.as_mut());
            //                         dbg!(&arm.term);
            //                         return Some(*arm.term);
            //                     }
            //                     Pattern::Constructor(tag, _) => {
            //                         if label == &tag {
            //                             // If the constructor is bound to a type
            //                             // that is not unit, we should term subst
            //                             // in the constructor's bound term to
            //                             // Var(0) so that the term in the arm
            //                             // can access it
            //                             match ty.as_ref() {
            //                                 Type::Unit => {}
            //                                 _ => term_subst(*tm.clone(), arm.term.as_mut()),
            //                             }

            //                             return Some(*arm.term);
            //                         }
            //                     }
            //                     Pattern::Literal(_) => return None,
            //                     Pattern::Product(_) => unimplemented!(),
            //                 }
            //             }
            //             None
            //         }
            //         _ => {
            //             let t_prime = self.small_step(*expr)?;
            //             Some(Term::new(Kind::Case(Box::new(t_prime), arms), term.span))
            //         }
            //     }
            // }
            _ => None,
        }
    }

    fn case_subst(&self, pat: &Pattern, expr: &Term, term: &mut Term) {
        use Pattern::*;
        match pat {
            Any => {}
            Literal(_) => {}
            Variable(_) => {
                term_subst(expr.clone(), term);
            }
            Product(v) => {
                if let Kind::Product(terms) = &expr.kind {
                    for (idx, pat) in v.iter().enumerate() {
                        self.case_subst(&pat, &terms[idx], term);
                    }
                }
            }
            Constructor(label, v) => {
                if let Kind::Injection(label_, tm, _) = &expr.kind {
                    if label == label_ {
                        self.case_subst(&v, &tm, term);
                    }
                }
            }
        }
    }
}

fn term_subst(mut s: Term, t: &mut Term) {
    Shift::new(1).visit(&mut s);
    Subst::new(s).visit(t);
    Shift::new(-1).visit(t);
}

fn type_subst(s: Type, t: &mut Term) {
    TyTermSubst::new(s).visit(t);
    Shift::new(-1).visit(t);
}

#[cfg(test)]
mod test {
    use super::*;
    use util::span::Span;

    #[test]
    fn literal() {
        let ctx = crate::types::Context::default();
        let eval = Eval::with_context(&ctx);
        assert_eq!(eval.small_step(lit!(false)), None);
    }

    #[test]
    fn application() {
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
    fn type_application() {
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

    #[test]
    fn projection() {
        let ctx = crate::types::Context::default();
        let eval = Eval::with_context(&ctx);
        let product = Term::new(
            Kind::Product(vec![nat!(5), nat!(6), nat!(29)]),
            Span::zero(),
        );
        let projection = Term::new(Kind::Projection(Box::new(product), 2), Span::zero());
        let term = app!(prim!(Primitive::Succ), projection);

        let t1 = eval.small_step(term);
        assert_eq!(t1, Some(app!(prim!(Primitive::Succ), nat!(29))));
        let t2 = eval.small_step(t1.unwrap());
        assert_eq!(t2, Some(nat!(30)));
        let t3 = eval.small_step(t2.unwrap());
        assert_eq!(t3, None);
    }
}
