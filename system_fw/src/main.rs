#[macro_use]
pub mod macros;
pub mod diagnostics;
pub mod functor;
pub mod stack;
pub mod terms;
pub mod typecheck;
pub mod types;

use terms::{Field, Kind, Record, Term};
use types::{TyKind, Type};
use util::span::Span;

fn main() {
    let mut ctx = typecheck::Context::default();

    let ord = exist!(
        kind!(*),
        record!(("lt", arrow!(Type::Var(0), arrow!(Type::Var(0), Type::Bool))))
    );

    let always_true_body = Term::new(
        terms::Kind::Record(terms::Record {
            fields: vec![Field {
                expr: Box::new(abs!(Type::Nat, abs!(Type::Nat, bool!(true)))),
                label: "lt".into(),
                span: Span::default(),
            }],
        }),
        Span::default(),
    );
    let always_true = pack!(Type::Nat, always_true_body, ord.clone());

    let rec = record!(
        ("empty", Type::Projection(Box::new(Type::Var(0)), 0)),
        (
            "mem",
            arrow!(
                Type::Projection(Box::new(Type::Var(0)), 1),
                arrow!(Type::Projection(Box::new(Type::Var(0)), 0), Type::Bool)
            )
        )
    );
    let kinds = TyKind::Product(vec![kind!(*), kind!(*)]);
    let sig = exist!(kinds, rec);

    let functor_body = Term::new(
        terms::Kind::Record(terms::Record {
            fields: vec![
                Field {
                    expr: Box::new(unit!()),
                    label: "empty".into(),
                    span: Span::default(),
                },
                Field {
                    expr: Box::new(abs!(Type::Var(0), abs!(Type::Unit, bool!(true)))),
                    label: "mem".into(),
                    span: Span::default(),
                },
            ],
        }),
        Span::default(),
    );

    let adt = abs!(
        ord,
        unpack!(
            var!(0),
            pack!(product!(Type::Unit, Type::Var(0)), functor_body, sig)
        )
    );
    let adt = app!(adt, always_true.clone());
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
