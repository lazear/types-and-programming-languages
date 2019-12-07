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

fn object_type() -> Type {
    tyop!(
        kind!(* => *),
        exist!(
            kind!(*),
            record!(
                ("state", Type::Var(0)),
                ("methods", op_app!(Type::Var(1), Type::Var(0)))
            )
        )
    )
}

fn counter_interface() -> Type {
    tyop!(
        kind!(*),
        record!(
            ("get", arrow!(Type::Var(0), Type::Nat)),
            ("inc", arrow!(Type::Var(0), Type::Var(0)))
        )
    )
}

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

    println!(
        "object type operator :: {}",
        ctx.kinding(&object_type()).unwrap()
    );
    println!(
        "counter interface :: {}",
        ctx.kinding(&counter_interface()).unwrap()
    );

    let mut counter = op_app!(object_type(), counter_interface());
    ctx.simplify_ty(&mut counter).unwrap();
    // ctx.simplify_ty(&mut counter);
    println!("{}", counter);

    // let mut ty = op_app!(list_type(), Type::Nat);
    // let mut ty = fold(list_type());
    // ctx.simplify_ty(&mut ty);
    // println!("{}", ty);
    // ty = op_app!(ty, Type::Nat);
    // ctx.simplify_ty(&mut ty);
    // println!("{}", ty);

    let mut ty = list_type();
    println!("1:\n{} {:?}", ty, ctx.kinding(&ty));

    ctx.simplify_ty(&mut ty);
    println!("2:\n{}", ty);
    ty = fold(ty);
    ctx.simplify_ty(&mut ty);
    println!("{}\n", ty);
    ty = op_app!(ty, Type::Nat);
    ctx.simplify_ty(&mut ty);
    println!("{}\n", ty);

    // ty = op_app!(ty, Type::Nat);
    // ctx.simplify_ty(&mut ty);
    // println!("fold: {}", ty);
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
                    product!(Type::Var(0), op_app!(Type::Var(1), Type::Var(0)))
                )
            )
        )
    );
    Type::Recursive(Box::new(inner))
}

// This is the "lambda-dropped" version of list_type()
// Ralf Hinze,
// Polytypic values possess polykinded types,
// Science of Computer Programming,
fn list_ty2() -> Type {
    let ListF = tyop!(
        kind!(*),
        tyop!(
            kind!(*),
            sum!(
                (
                    "Cons",
                    record!(("head", Type::Var(1)), ("tail", Type::Var(0)))
                ),
                ("Nil", Type::Unit)
            )
        )
    );
    let List = tyop!(
        kind!(*),
        Type::Recursive(Box::new(op_app!(ListF, Type::Var(0))))
    );
    List
}

fn plist() -> Type {
    let PListF = tyop!(
        kind!(*),
        tyop!(
            kind!(* => *),
            sum!(
                (
                    "Cons",
                    product!(
                        Type::Var(1),
                        op_app!(Type::Var(0), product!(Type::Var(1), Type::Var(1)))
                    )
                ),
                ("Nil", Type::Unit)
            )
        )
    );
    let PList = tyop!(
        kind!(*),
        Type::Recursive(Box::new(op_app!(PListF, Type::Var(0))))
    );
    PList
}

fn fold(rec: Type) -> Type {
    match &rec {
        Type::Recursive(ty) => {
            let mut ty = *ty.clone();
            ty = op_app!(ty, rec.clone());
            ty
        }
        _ => rec,
    }
}
