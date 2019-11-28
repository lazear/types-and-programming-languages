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
    println!("Hello, world!");

    // pack {Nat, 10} as {∃X, X}
    let tm = pack!(Type::Nat, nat!(10), exist!(kind!(*), Type::Var(0)));

    let ex_k = kind!(kind!(*) => kind!(* => *));
    let pair = tyop!(kind!(*), tyop!(kind!(*), univ!(kind!(*), Type::Var(0))));

    let pair_adt = Term::new(
        terms::Kind::Record(terms::Record {
            fields: vec![Field {
                span: Span::zero(),
                label: "pair".to_string(),
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
                                        arrow!(Type::Var(2), arrow!(Type::Var(1), Type::Var(0))),
                                        app!(app!(var!(0), var!(2)), var!(1))
                                    )
                                )
                            )
                        )
                    )
                )),
            }],
        }),
        Span::zero(),
    );

    let pair_sig = exist!(
        ex_k,
        record!(
            (
                "pair",
                univ!(univ!(arrow!(
                    Type::Var(1),
                    arrow!(
                        Type::Var(0),
                        op_app!(op_app!(Type::Var(2), Type::Var(1)), Type::Var(0))
                    )
                ))),
                // Type::Universal(
                //     Box::new(kind!(*)),
                //     Box::new(Type::Universal(
                //         Box::new(kind!(*)),
                //         Box::new(arrow!(Type::Var(1), arrow!(Type::Var(2), pair_app.clone())))
                //     ))
                // )
            ) // ("fst", univ!(kind::)
              // ("snd", arrow!(pair_app.clone(), Type::Var(2)))
        )
    );

    let adt = pack!(pair, pair_adt, pair_sig);

    // let pair_adt =
    // pack!(Type::Abs(Box::new(Type::Abs(Box::new(Type::Univ(Box::new(
    //     arrow!(Type::Var(2), arrow!(Type::Var(1), Type::Var(0)))
    //         )))))),

    // );

    let mut ctx = typecheck::Context::default();

    let t2 = tyabs!(
        // X
        kind!(*),
        tyabs!(
            // Y
            kind!(*),
            abs!(
                // \x: X.
                Type::Var(1),
                abs!(
                    // \y: Y.
                    Type::Var(0),
                    tyabs!(
                        // R
                        kind!(*),
                        abs!(
                            // \p: X->Y->R. p x y
                            arrow!(Type::Var(2), arrow!(Type::Var(1), Type::Var(0))),
                            app!(app!(var!(0), var!(2)), var!(1))
                        )
                    )
                )
            )
        )
    );
    // let t2 =
    // tyabs!( // X
    //     kind!(*),
    //     tyabs!( // Y
    //         kind!(*),
    //         abs!(   // \x: X.
    //             Type::Var(1),
    //             abs!(   // \y: Y.
    //                 Type::Var(0),

    //                     abs!(   // \p: X->Y->R. p x y
    //                         arrow!(Type::Var(1), arrow!(Type::Var(0), Type::Nat)),
    //                         app!(app!(var!(0), var!(2)), var!(1))
    //                     )

    //             )
    //         )
    //     )
    // );

    let t2 = abs!(
        Type::Nat,
        tyabs!(
            kind!(*),
            abs!(arrow!(Type::Nat, Type::Var(0)), app!(var!(0), var!(1)))
        )
    );
    let t2 = tyapp!(app!(t2, nat!(10)), Type::Bool);
    println!("{}", &t2);
    // let t3 = tyapp!(t2, Type::Unit);
    // let t4 = tyapp!(t3, Type::Bool);
    dbg!(ctx.typecheck(&t2).unwrap());
}
