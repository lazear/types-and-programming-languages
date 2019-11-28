use crate::diagnostics::Diagnostic;
use crate::stack::Stack;
use crate::terms::{Constant, Field, Kind, Record, Term};
use crate::types::{MutTypeVisitor, Shift, TyField, TyKind, Type};
use util::span::Span;
/// A typing context, Γ
#[derive(Debug)]
pub struct Context {
    stack: Stack<Type>,
    kstack: Stack<TyKind>,
}

impl Default for Context {
    fn default() -> Context {
        Context {
            stack: Stack::with_capacity(16),
            kstack: Stack::with_capacity(16),
        }
    }
}

#[derive(Debug, PartialEq)]
enum KindError {
    Mismatch(TyKind, TyKind),
    NotArrow(TyKind),
    NotStar(TyKind),
    Unbound(usize),
}

impl KindError {
    fn to_diag(self, span: Span) -> Diagnostic {
        match self {
            KindError::Mismatch(k1, k2) => Diagnostic::error(
                span,
                format!(
                    "a type of kind {:?} is required, but a type of kind {:?} was supplied",
                    k1, k2
                ),
            ),
            KindError::NotArrow(k) => Diagnostic::error(
                span,
                format!(
                    "a type of kind *->* was required, but a type of kind {:?} was supplied",
                    k
                ),
            ),
            KindError::NotStar(k) => Diagnostic::error(
                span,
                format!(
                    "a type of kind * was required, but a type of kind {:?} was supplied",
                    k
                ),
            ),
            KindError::Unbound(idx) => Diagnostic::error(
                span,
                format!("unbound type variable with de Bruijn index {}", idx),
            ),
        }
    }
}

impl Context {
    fn kinding(&mut self, ty: &Type) -> Result<TyKind, KindError> {
        match ty {
            Type::Var(idx) => self
                .kstack
                .get(*idx)
                .cloned()
                .ok_or(KindError::Unbound(*idx)),
            Type::Abs(kind, t) => {
                self.kstack.push(*kind.clone());
                let k_ = self.kinding(&t)?;
                self.kstack.pop();
                Ok(TyKind::Arrow(kind.clone(), Box::new(k_)))
            }
            Type::App(s, t) => match self.kinding(&s)? {
                TyKind::Arrow(a, b) => {
                    let k = self.kinding(&t)?;
                    if k == *a {
                        Ok(*b)
                    } else {
                        Err(KindError::Mismatch(*a, k))
                    }
                }
                k => Err(KindError::NotArrow(k)),
            },
            Type::Arrow(s, t) => match self.kinding(&s)? {
                TyKind::Star => match self.kinding(&t)? {
                    TyKind::Star => Ok(TyKind::Star),
                    k => Err(KindError::Mismatch(TyKind::Star, k)),
                },
                k => Err(KindError::Mismatch(TyKind::Star, k)),
            },
            Type::Universal(kind, t) => {
                self.kstack.push(*kind.clone());
                let k_ = self.kinding(&t)?;
                self.kstack.pop();
                match k_ {
                    TyKind::Star => Ok(TyKind::Star),
                    k => Err(KindError::Mismatch(TyKind::Star, k)),
                }
            }
            Type::Existential(kind, t) => {
                self.kstack.push(*kind.clone());
                let k_ = self.kinding(&t)?;
                self.kstack.pop();
                match k_ {
                    TyKind::Star => Ok(TyKind::Star),
                    k => Err(KindError::Mismatch(TyKind::Star, k)),
                }
            }
            _ => Ok(TyKind::Star),
        }
    }

    fn is_star_kind(&mut self, ty: &Type, err_span: Span) -> Result<(), Diagnostic> {
        let kind = self
            .kinding(ty)
            .map_err(|k| KindError::to_diag(k, err_span))?;
        if kind == TyKind::Star {
            Ok(())
        } else {
            return Err(Diagnostic::error(
                err_span,
                format!(
                    "type bound in type abstraction must have a kind *, {} has a kind of {}",
                    ty, kind
                ),
            ));
        }
    }

    pub fn typecheck(&mut self, term: &Term) -> Result<Type, Diagnostic> {
        match &term.kind {
            Kind::Const(c) => match c {
                Constant::Unit => Ok(Type::Unit),
                Constant::Nat(_) => Ok(Type::Nat),
                Constant::Bool(_) => Ok(Type::Bool),
            },
            Kind::Var(idx) => self
                .stack
                .get(*idx)
                .cloned()
                .ok_or(Diagnostic::error(term.span, "unbound variable")),
            Kind::Abs(ty, tm) => {
                self.is_star_kind(ty, term.span)?;
                self.stack.push(*ty.clone());
                let mut ty2 = self.typecheck(&tm)?;
                self.stack.pop();

                // dbg!(&ty2);
                // Shift::new(-1).visit(&mut ty2);
                // dbg!(&ty2);

                Ok(Type::Arrow(ty.clone(), Box::new(ty2)))
            }
            Kind::App(m, n) => {
                let ty = self.typecheck(&m)?;
                if let Type::Arrow(ty11, ty12) = ty {
                    let ty2 = self.typecheck(&n)?;
                    if *ty11 == ty2 {
                        Ok(*ty12)
                    } else {
                        dbg!(&self.stack);
                        dbg!(&self.kstack);
                        let d = Diagnostic::error(term.span, "type mismatch in application")
                            .message(m.span, format!("abstraction {} requires type {}", m, ty11))
                            .message(n.span, format!("term {} has a type of {}", n, ty2));
                        return Err(d);
                    }
                } else {
                    let d = Diagnostic::error(term.span, "type mismatch in application")
                        .message(m.span, format!("this term has a type {}, not T->U", ty));
                    return Err(d);
                }
            }
            Kind::TyAbs(tk, polymorphic) => {
                self.kstack.push(*tk.clone());
                let ty = self.typecheck(&polymorphic)?;
                self.kstack.pop();
                Ok(Type::Universal(tk.clone(), Box::new(ty)))
            }
            Kind::TyApp(tyabs, applied) => {
                match self.typecheck(&tyabs)? {
                    Type::Universal(k1, u) => {
                        let k2 = self
                            .kinding(&applied)
                            .map_err(|k| KindError::to_diag(k, term.span))?;
                        if k2 == *k1 {
                            // actually do subst
                            let mut u = *u;
                            u.subst(*applied.clone());
                            Ok(u)
                        } else {
                            let d = Diagnostic::error(term.span, "type kind mismatch in term-level type application")
                            .message(tyabs.span, format!("universal type requires a type of kind {}, but a kind of {} is given", &k1, k2));
                            Err(d)
                        }
                    }
                    ty => {
                        let d = Diagnostic::error(
                            term.span,
                            "type mismatch in term-level type application",
                        )
                        .message(
                            tyabs.span,
                            format!("this term has a type {}, not forall. X::K", ty),
                        );
                        Err(d)
                    }
                }
            }

            Kind::Record(rec) => {
                let mut tys = Vec::with_capacity(rec.fields.len());
                for f in &rec.fields {
                    tys.push(TyField {
                        label: f.label.clone(),
                        ty: Box::new(self.typecheck(&f.expr)?),
                    });
                }
                Ok(Type::Record(tys))
            }

            Kind::Pack(witness, packed, sig) => {
                // where sig =  {∃X, T2}
                // Γ⊢ packed : [ X -> witness ] T2 and Γ⊢  {∃X, T2} :: *
                // then {*witness, packed} as  {∃X, T2}

                let k = self
                    .kinding(sig)
                    .map_err(|k| KindError::to_diag(k, term.span))?;
                if k != TyKind::Star {
                    return diag!(
                        term.span,
                        "existential type definition does not have a kind of *"
                    );
                }
                match sig.as_ref() {
                    Type::Existential(kind, t2) => {
                        let witness_kind = self
                            .kinding(&witness)
                            .map_err(|k| KindError::to_diag(k, term.span))?;

                        if &witness_kind != kind.as_ref() {
                            return diag!(term.span, "existential type requires a type of kind {:?}, but implementation type has a kind of {:?}", kind, witness_kind);
                        }

                        let ty_packed = self.typecheck(packed)?;
                        let mut ty_packed_prime = *t2.clone();
                        ty_packed_prime.subst(*witness.clone());

                        // Pierce's code has kind checking before type-substitution,
                        // does this matter? He also directly kind-checks the
                        // witness type against kind

                        if ty_packed == ty_packed_prime {
                            // let kind_prime = self
                            //     .kinding(&ty_packed_prime)
                            //     .map_err(|k| KindError::to_diag(k, term.span))?;

                            // if kind_prime != TyKind::Star {
                            //     return diag!(
                            //         term.span,
                            //         "existential type definition does not have a kind of *!"
                            //     );
                            // }

                            // if &kind_prime != kind.as_ref() {
                            //      return diag!(term.span, "existential type requires a type of kind {:?}, but implementation type has a kind of {:?}", kind, kind_prime);
                            // }

                            Ok(*sig.clone())
                        } else {
                            Err(Diagnostic::error(
                                term.span,
                                "type mismatch in existential package",
                            )
                            .message(packed.span, format!("term has a type of {:?}", ty_packed))
                            .message(
                                term.span,
                                format!(
                                    "but the existential package type is defined as {:?}",
                                    ty_packed_prime,
                                ),
                            ))
                        }
                    }
                    ty => diag!(term.span, "cannot pack an existential type into {:?}", ty),
                }
            }

            Kind::Unpack(_, _) => diag!(term.span, "unknwon term {}", 10), // _ => Err(Diagnostic::error(term.span, "unknown term")),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn ty_app() {
        // ΛX. λx: X. x  should typecheck to ∀X. X->X
        let id = tyabs!(kind!(*), abs!(Type::Var(0), var!(0)));
        // Instantiations of polymorphic identity function ∀X. X->X
        let inst1 = tyapp!(id.clone(), Type::Nat);
        let inst2 = tyapp!(id.clone(), arrow!(Type::Unit, Type::Nat));

        let mut ctx = Context::default();
        // ΛX. λx: X. x  should typecheck to ∀X. X->X
        assert_eq!(
            ctx.typecheck(&id),
            Ok(univ!(kind!(*), arrow!(Type::Var(0), Type::Var(0))))
        );
        assert_eq!(ctx.typecheck(&inst1), Ok(arrow!(Type::Nat, Type::Nat)));
        assert_eq!(
            ctx.typecheck(&inst2),
            Ok(arrow!(
                arrow!(Type::Unit, Type::Nat),
                arrow!(Type::Unit, Type::Nat)
            ))
        )
    }

    #[test]
    fn ty_exist() {
        let interface_ty = exist!(
            kind!(*),
            record!(
                ("new", Type::Var(0)),
                ("get", arrow!(Type::Var(0), Type::Nat))
            )
        );
        let adt = Term::new(
            Kind::Record(Record {
                fields: vec![
                    Field {
                        span: Span::zero(),
                        label: "new".to_string(),
                        expr: Box::new(nat!(0)),
                    },
                    Field {
                        span: Span::zero(),
                        label: "get".to_string(),
                        expr: Box::new(abs!(Type::Nat, var!(0))),
                    },
                ],
            }),
            Span::zero(),
        );
        let counter = pack!(Type::Nat, adt, interface_ty.clone());
        let mut ctx = Context::default();
        assert_eq!(ctx.typecheck(&counter), Ok(interface_ty));
    }

    #[test]
    fn type_type_abs() {
        let ty = tyop!(kind!(*), Type::Var(0));
        let mut ctx = Context::default();
        assert_eq!(ctx.kinding(&ty), Ok(kind!(* => *)));
        assert_eq!(
            ctx.kinding(&Type::App(Box::new(ty), Box::new(Type::Nat))),
            Ok(kind!(*))
        );

        let pair = tyop!(kind!(*), tyop!(kind!(*), univ!(kind!(*), Type::Var(0))));
        assert_eq!(ctx.kinding(&pair), Ok(kind!(kind!(*) => kind!(* => *))));
    }
}
