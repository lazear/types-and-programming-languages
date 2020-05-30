use super::*;

pub fn parameterized_set() -> Type {
    tyop!(kind!(*), exist!(kind!(* => *), op_app!(Type::Var(0), Type::Var(1))))
}

fn list_type() -> Type {
    let inner = tyop!(
        kind!(* => *),
        tyop!(
            kind!(*),
            sum!(
                ("Nil", Type::Unit),
                (
                    "Cons",
                    record!(("head", Type::Var(0)), ("tail", op_app!(Type::Var(1), Type::Var(0))))
                )
            )
        )
    );
    Type::Recursive(Box::new(inner))
}

pub fn parameterized_set_term() -> Term {
    let body = Term::new(
        terms::Kind::Fold(
            Box::new(op_app!(list_type(), Type::Var(0))),
            Box::new(Term::new(
                terms::Kind::Injection(
                    "Nil".into(),
                    Box::new(unit!()),
                    // Manually perform an unfold on list_type()
                    // - In the System F language, we had an
                    // InjRewriter macro that takes care of this,
                    // and we could probably tack it directly
                    // into the type-checker since we can do simplification
                    // now
                    Box::new(op_app!(unfold(list_type()), Type::Var(0))),
                ),
                Span::default(),
            )),
        ),
        Span::default(),
    );

    // \X :: * => pack type 'a list = Nil | Cons 'a * 'a list with Nil as /\T::*
    // {*X::*=>*, X T}
    tyabs!(
        kind!(*),
        pack!(list_type(), body, op_app!(parameterized_set(), Type::Var(0)))
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parameterized_functor() {
        let mut ctx = typecheck::Context::default();
        let func = tyapp!(functor::parameterized_set_term(), Type::Nat);
        let func_actual_ty = ctx.typecheck(&func).unwrap();
        let func_described_ty = op_app!(functor::parameterized_set(), Type::Nat);
        assert_eq!(ctx.equiv(&func_actual_ty, &func_described_ty), Ok(true));
    }
}
