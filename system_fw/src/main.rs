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

fn set() {
    let mut concrete = record!(
        ("empty", Type::Var(1)),
        (
            "add",
            arrow!(product!(Type::Var(0), Type::Var(1)), Type::Var(1))
        ),
        (
            "mem",
            arrow!(product!(Type::Var(0), Type::Var(1)), Type::Bool)
        )
    );

    let eq_sig = exist!(
        kind!(*),
        record!((
            "eq",
            arrow!(product!(Type::Var(1), Type::Var(1)), Type::Bool)
        ))
    );

    // We omit a universal type wrapper because we would have to manually
    // write Type::Var(1) to actually refer to the 0th debruijn index, due
    // to how the substition function currently works. This is probably
    // something worth investigating. The Contxt::typecheck function
    // performs substition on the value *inside* the universal wrapper,
    // not on the outside, which bypasses incrementing the cutoff value
    // for both Shift and Subst MutTypeVisitors
    let mut functor_ty = arrow!(
        // record!(
        //     (
        //         "eq",
        //         arrow!(product!(Type::Var(0), Type::Var(0)), Type::Bool)
        //     )
        // ),
        eq_sig,
        exist!(
            kind!(*),
            record!(
                ("empty", Type::Var(0)),
                (
                    "add",
                    arrow!(product!(Type::Var(1), Type::Var(0)), Type::Var(0))
                ),
                (
                    "mem",
                    arrow!(product!(Type::Var(1), Type::Var(0)), Type::Bool)
                )
            )
        )
    );
    functor_ty.subst(Type::Nat);
    let mut ctx = typecheck::Context::default();
    ctx.simplify_ty(&mut functor_ty).unwrap();
    println!("{}", functor_ty);

    let functor_body = Term::new(
        terms::Kind::Record(terms::Record {
            fields: vec![
                Field {
                    expr: Box::new(unit!()),
                    label: "empty".into(),
                    span: Span::default(),
                },
                Field {
                    expr: Box::new(abs!(product!(Type::Var(0), Type::Unit), unit!())),
                    label: "add".into(),
                    span: Span::default(),
                },
                Field {
                    expr: Box::new(abs!(product!(Type::Var(0), Type::Unit), bool!(true))),
                    label: "mem".into(),
                    span: Span::default(),
                },
            ],
        }),
        Span::default(),
    );

    let ff = tyabs!(
        kind!(*),
        abs!(
            record!((
                "eq",
                arrow!(product!(Type::Var(0), Type::Var(0)), Type::Bool)
            )),
            pack!(Type::Unit, functor_body, {
                let mut c = concrete.clone();
                c.subst(Type::Var(1));
                exist!(kind!(*), c)
            })
        )
    );
    let ff = tyapp!(ff, Type::Nat);
    println!("\n\n ff[Nat]\n\n{}", ff);
    println!("\nty: {}", ctx.typecheck(&ff).unwrap());
    assert_eq!(ctx.typecheck(&ff).unwrap(), functor_ty);
}

fn main() {
    // let mut ty = op_app!(list_type(), Type::Nat);
    // println!("{}", ty);

    // ty = unfold(ty);
    // println!("{}", ty);
    // ctx.simplify_ty(&mut ty);
    // println!("{}", ty);

    set();
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
    let PList = tyop!(
        kind!(* => *),
        tyop!(
            kind!(*),
            product!(
                Type::Var(0),
                op_app!(Type::Var(1), product!(Type::Var(0), Type::Var(0)))
            )
        )
    );
    Type::Recursive(Box::new(PList))
}

fn unfold(ty: Type) -> Type {
    match &ty {
        Type::Recursive(inner) => op_app!(*inner.clone(), ty),
        Type::App(a, b) => match a.as_ref() {
            Type::Recursive(rec) => op_app!(unfold(*a.clone()), *b.clone()),
            _ => ty,
        },
        _ => ty,
    }
}
