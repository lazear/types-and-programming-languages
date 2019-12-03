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
pub enum KindError {
    Mismatch(TyKind, TyKind),
    NotArrow(TyKind),
    Unbound(usize),
}

struct TypeSimplifier<'a> {
    ctx: &'a mut Context,
    res: Result<bool, KindError>,
}

impl<'a> TypeSimplifier<'a> {}

impl<'a> MutTypeVisitor for TypeSimplifier<'a> {
    fn visit_universal(&mut self, kind: &mut TyKind, ty: &mut Type) {
        self.ctx.kstack.push(kind.clone());
        self.visit(ty);
        self.ctx.kstack.pop();
    }

    fn visit_existential(&mut self, kind: &mut TyKind, ty: &mut Type) {
        self.ctx.kstack.push(kind.clone());
        self.visit(ty);
        self.ctx.kstack.pop();
    }

    fn visit_abs(&mut self, kind: &mut TyKind, ty: &mut Type) {
        self.ctx.kstack.push(kind.clone());
        self.visit(ty);
        self.ctx.kstack.pop();
    }
    fn visit(&mut self, ty: &mut Type) {
        match ty {
            Type::Var(_) | Type::Unit | Type::Bool | Type::Nat => {}
            Type::Record(fields) => self.visit_record(fields),
            Type::Arrow(ty1, ty2) => self.visit_arrow(ty1, ty2),
            Type::Universal(k, ty) => self.visit_universal(k, ty),
            Type::Existential(k, ty) => self.visit_existential(k, ty),
            Type::Abs(s, t) => self.visit_abs(s, t),
            Type::App(m, n) => {
                self.visit(m);
                self.visit(n);
                if let Type::Abs(k, t) = m.as_mut() {
                    match self.ctx.kinding(&n) {
                        Ok(n_kind) => {
                            if k.as_ref() == &n_kind {
                                t.subst(*n.clone());
                                *ty = *t.clone();
                                self.res = Ok(true);
                            } else {
                                self.res = Err(KindError::Mismatch(*k.clone(), n_kind))
                            }
                        }
                        Err(e) => self.res = Err(e),
                    }
                }
            }
        }
    }
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
            KindError::Unbound(idx) => Diagnostic::error(
                span,
                format!("unbound type variable with de Bruijn index {}", idx),
            ),
        }
    }
}

impl Context {
    pub fn kinding(&mut self, ty: &Type) -> Result<TyKind, KindError> {
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
            Type::Record(fields) => {
                for f in fields {
                    let k = self.kinding(&f.ty)?;
                    if k != TyKind::Star {
                        return Err(KindError::Mismatch(TyKind::Star, k));
                    }
                }
                Ok(TyKind::Star)
            }
            _ => Ok(TyKind::Star),
        }
    }

    pub fn simplify_ty(&mut self, ty: &mut Type) -> Result<bool, KindError> {
        let mut ts = TypeSimplifier {
            ctx: self,
            res: Ok(false),
        };
        let mut work = false;
        loop {
            ts.res = Ok(false);
            ts.visit(ty);
            match ts.res {
                Ok(false) => return Ok(work),
                Ok(true) => work = true,
                Err(e) => return Err(e),
            }
        }
    }

    pub fn equiv(&mut self, lhs: &Type, rhs: &Type) -> Result<bool, KindError> {
        if lhs == rhs {
            Ok(true)
        } else {
            let mut lhs_ = lhs.clone();
            let mut rhs_ = rhs.clone();
            self.simplify_ty(&mut lhs_)?;
            self.simplify_ty(&mut rhs_)?;
            Ok(lhs_ == rhs_)
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
                let ty2 = self.typecheck(&tm)?;
                self.stack.pop();
                Ok(Type::Arrow(ty.clone(), Box::new(ty2)))
            }
            Kind::App(m, n) => {
                let mut ty = self.typecheck(&m)?;
                self.simplify_ty(&mut ty)
                    .map_err(|ke| ke.to_diag(term.span))?;
                if let Type::Arrow(ty11, ty12) = ty {
                    let ty2 = self.typecheck(&n)?;
                    if self
                        .equiv(&ty11, &ty2)
                        .map_err(|ke| ke.to_diag(term.span))?
                    {
                        Ok(*ty12)
                    } else {
                        dbg!(&self.stack);
                        dbg!(&self.kstack);
                        let d = Diagnostic::error(term.span, "type mismatch in application")
                            .message(
                                m.span,
                                format!(
                                    "abstraction {} requires type {} to return {}",
                                    m, ty11, ty12
                                ),
                            )
                            .message(n.span, format!("term {} has a type of {}", n, ty2));
                        return Err(d);
                    }
                } else {
                    // dbg!(&self.stack);
                    dbg!(&m);
                    dbg!(&ty);
                    let d = Diagnostic::error(term.span, "type mismatch in application").message(
                        m.span,
                        format!("this term {} has a type {}, not T->U", m, ty),
                    );
                    return Err(d);
                }
            }
            Kind::TyAbs(tk, polymorphic) => {
                self.kstack.push(*tk.clone());
                self.stack.iter_mut().for_each(|ty| match ty {
                    Type::Var(v) => *v += 1,
                    _ => {}
                });
                let ty = self.typecheck(&polymorphic)?;
                self.stack.iter_mut().for_each(|ty| match ty {
                    Type::Var(v) => *v -= 1,
                    _ => {}
                });
                self.kstack.pop();
                Ok(Type::Universal(tk.clone(), Box::new(ty)))
            }
            Kind::TyApp(tyabs, applied) => {
                let mut ty = self.typecheck(&tyabs)?;
                self.simplify_ty(&mut ty)
                    .map_err(|ke| ke.to_diag(term.span))?;
                match ty {
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

                let mut sig = sig.clone();
                self.simplify_ty(&mut sig)
                    .map_err(|ke| ke.to_diag(term.span))?;
                match sig.as_mut() {
                    Type::Existential(kind, t2) => {
                        let witness_kind = self
                            .kinding(&witness)
                            .map_err(|k| KindError::to_diag(k, term.span))?;

                        if &witness_kind != kind.as_ref() {
                            return diag!(term.span, "existential type requires a type of kind {}, but implementation type has a kind of {}", kind, witness_kind);
                        }

                        let ty_packed = self.typecheck(packed)?;
                        let mut ty_packed_prime = *t2.clone();
                        ty_packed_prime.subst(*witness.clone());

                        // Pierce's code has kind checking before type-substitution,
                        // does this matter? He also directly kind-checks the
                        // witness type against kind

                        if self
                            .equiv(&ty_packed, &ty_packed_prime)
                            .map_err(|ke| ke.to_diag(term.span))?
                        {
                            Ok(*sig.clone())
                        } else {
                            Err(Diagnostic::error(
                                term.span,
                                "type mismatch in existential package",
                            )
                            .message(packed.span, format!("term has a type of {}", ty_packed))
                            .message(
                                term.span,
                                format!(
                                    "but the existential package type is defined as {}",
                                    ty_packed_prime,
                                ),
                            ))
                        }
                    }
                    ty => diag!(term.span, "cannot pack an existential type into {:?}", ty),
                }
            }

            Kind::Unpack(_, _) => diag!(term.span, "unknwon term {}", 10),
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
        let inst3 = tyapp!(
            id.clone(),
            univ!(kind!(*), arrow!(Type::Var(0), Type::Var(0)))
        );

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
        );
        assert_eq!(
            ctx.typecheck(&inst3),
            Ok(arrow!(
                univ!(kind!(*), arrow!(Type::Var(0), Type::Var(0))),
                univ!(kind!(*), arrow!(Type::Var(0), Type::Var(0)))
            ))
        );
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

    #[test]
    fn ty_equivalence() {
        let ty1 = op_app!(tyop!(kind!(*), Type::Var(0)), Type::Nat);
        let ty2 = Type::Nat;
        let mut ctx = Context::default();
        assert_eq!(ctx.equiv(&ty1, &ty2), Ok(true), "{:?}", ty1);
    }
}
