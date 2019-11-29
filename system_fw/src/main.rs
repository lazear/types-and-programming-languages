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

    let ex_k = kind!(kind!(*) => kind!(* => *));
    // let pair = tyop!(kind!(*), tyop!(kind!(*), univ!(kind!(*), Type::Var(0))));
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

    // PairSig = {Some Pair::*=>*=>*, {pair: All X. All Y. X->Y->(Pair X Y)}};
    // pairadt = { *lambda X. lambda Y. All R. (X->Y->R)->R,
    //              {pair = lambda X. lambda Y. lambda x:X. lambda y:Y. lambda R. lambda p: X->Y->R. p x y}} as PairSig;
    //     (let {X,x} = pairadt as PairSig in x.pair)(lambda X.
    //         lambda Y.
    //           lambda x:X.
    //             lambda y:Y.
    //               lambda R.
    //                 lambda p:
    //                   X->
    //                   Y->R.
    //                   p x y)
    // : All X. All Y. X -> Y -> X X Y
    //
    // with debruijn
    // (let {X,x} = pairadt as 1 in x.pair)(lambda X.
    //         lambda Y.
    //         lambda x:1.
    //           lambda y:1.
    //             lambda R.
    //               lambda p:4->3->0.
    //                 p x y)
    // : All X. All Y. 1 -> 0 -> 1 1 0

    let pair_adt = Term::new(
        terms::Kind::Record(terms::Record {
            fields: vec![Field {
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
                                        // op_app!(op_app!(witness.clone(), Type::Var(2)), Type::Var(1)),
                                        // arrow!(Type::Var(2), Type::Var(1)),
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

    let mut pair_sig = exist!(
        ex_k,
        record!((
            "pair",
            // ∀X. ∀Y. X->Y->(Pair X Y)
            // ∀X. ∀Y. 1->0->(2 1 0)
            univ!(univ!(arrow!(
                Type::Var(1),
                arrow!(
                    Type::Var(0),
                    // witness.clone()
                    op_app!(op_app!(Type::Var(2), Type::Var(1)), Type::Var(0))
                )
            ))),
        ))
    );

    let adt = pack!(witness.clone(), pair_adt, pair_sig.clone());
    let mut ctx = typecheck::Context::default();

    let tt = op_app!(witness.clone(), Type::Nat);
    let mut ts = op_app!(tt, Type::Bool);

    ctx.simplify_ty(&mut ts);
    println!("{}", ts);
    dbg!(ctx.typecheck(&adt).unwrap());
}
