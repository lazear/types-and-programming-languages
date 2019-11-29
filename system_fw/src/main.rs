#[macro_use]
pub mod macros;
pub mod diagnostics;
pub mod stack;
pub mod terms;
pub mod typecheck;
pub mod types;
use terms::{Field, Kind, Record, Term};
use types::{TyKind, Type};
use util::span::Span;

fn main() {
    let ex_k = kind!(kind!(*) => kind!(* => *));
    let witness = tyop!(
        kind!(*),
        tyop!(
            kind!(*),
            univ!(
                kind!(*),
                arrow!(
                    arrow!(Type::Var(2), arrow!(Type::Var(1), Type::Var(0))),
                    Type::Var(0)
                )
            )
        )
    );

    let fst = tyabs!(
        kind!(*),
        tyabs!(
            kind!(*),
            abs!(
                op_app!(op_app!(witness.clone(), Type::Var(1)), Type::Var(0)),
                app!(
                    tyapp!(var!(0), Type::Var(1)),
                    abs!(Type::Var(1), abs!(Type::Var(0), var!(1)))
                )
            )
        )
    );

    let snd = tyabs!(
        kind!(*),
        tyabs!(
            kind!(*),
            abs!(
                op_app!(op_app!(witness.clone(), Type::Var(1)), Type::Var(0)),
                app!(
                    tyapp!(var!(0), Type::Var(0)),
                    abs!(Type::Var(1), abs!(Type::Var(0), var!(0)))
                )
            )
        )
    );

    let pair_adt = Term::new(
        terms::Kind::Record(terms::Record {
            fields: vec![
                Field {
                    span: Span::zero(),
                    label: "pair".to_string(),
                    // required type signature
                    // ∀X. ∀Y. X->Y->(Pair X Y)
                    //
                    expr: Box::new(tyabs!(
                        kind!(*),
                        tyabs!(
                            kind!(*),
                            abs!(
                                Type::Var(1),
                                abs!(
                                    Type::Var(0),
                                    tyabs!(
                                        kind!(*),
                                        abs!(
                                            arrow!(
                                                Type::Var(2),
                                                arrow!(Type::Var(1), Type::Var(0))
                                            ),
                                            app!(app!(var!(0), var!(2)), var!(1))
                                        )
                                    )
                                )
                            )
                        )
                    )),
                },
                Field {
                    span: Span::zero(),
                    label: "fst".to_string(),
                    expr: Box::new(fst),
                },
                Field {
                    span: Span::zero(),
                    label: "snd".to_string(),
                    expr: Box::new(snd),
                },
            ],
        }),
        Span::zero(),
    );

    let mut pair_sig = exist!(
        ex_k,
        record!(
            (
                "pair",
                // ∀X. ∀Y. X->Y->(Pair X Y)
                // ∀X. ∀Y. 1->0->(2 1 0)
                univ!(univ!(arrow!(
                    Type::Var(1),
                    arrow!(
                        Type::Var(0),
                        op_app!(op_app!(Type::Var(2), Type::Var(1)), Type::Var(0))
                    )
                ))),
            ),
            // ∀X. ∀Y. (Pair X Y)->X
            (
                "fst",
                univ!(univ!(arrow!(
                    op_app!(op_app!(Type::Var(2), Type::Var(1)), Type::Var(0)),
                    Type::Var(1)
                )))
            ),
            // ∀X. ∀Y. (Pair X Y)->Y
            (
                "snd",
                univ!(univ!(arrow!(
                    op_app!(op_app!(Type::Var(2), Type::Var(1)), Type::Var(0)),
                    Type::Var(0)
                )))
            )
        )
    );

    let adt = pack!(witness.clone(), pair_adt, pair_sig.clone());
    let mut ctx = typecheck::Context::default();
    println!("{}", ctx.typecheck(&adt).unwrap());
}
