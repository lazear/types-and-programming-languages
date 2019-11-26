#[macro_use]
pub mod macros;
pub mod diagnostics;
pub mod stack;
pub mod terms;
pub mod typecheck;
pub mod types;
use types::{TyKind, Type};

fn main() {
    println!("Hello, world!");

    // pack {Nat, 10} as {∃X, X}
    let tm = pack!(Type::Nat, nat!(10), exist!(kind!(*), Type::Var(0)));

    let ex_k = kind!(kind!(*) => kind!(* => *));
    let pair_app = Type::App(
        Box::new(Type::App(Box::new(Type::Var(0)), Box::new(Type::Var(1)))),
        Box::new(Type::Var(2)),
    );

    let pair_sig = exist!(
        ex_k,
        record!(
            (
                "pair",
                Type::Universal(
                    Box::new(kind!(*)),
                    Box::new(Type::Universal(
                        Box::new(kind!(*)),
                        Box::new(arrow!(Type::Var(1), arrow!(Type::Var(2), pair_app.clone())))
                    ))
                )
            ),
            ("fst", arrow!(pair_app.clone(), Type::Var(1))),
            ("snd", arrow!(pair_app.clone(), Type::Var(2)))
        )
    );

    // let pair_adt =
    // pack!(Type::Abs(Box::new(Type::Abs(Box::new(Type::Univ(Box::new(
    //     arrow!(Type::Var(2), arrow!(Type::Var(1), Type::Var(0)))
    //         )))))),

    // );
    dbg!(pair_sig);
}
