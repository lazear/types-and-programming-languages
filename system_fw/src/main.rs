#[macro_use]
pub mod macros;
pub mod diagnostics;
pub mod functor;
pub mod stack;
pub mod syntax;
pub mod terms;
pub mod typecheck;
pub mod types;

use std::io::prelude::*;
use syntax::parser::{Error, ErrorKind, Parser};
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
    let mut ctx = typecheck::Context::default();

    loop {
        let mut buffer = String::new();
        print!("repl: ");
        std::io::stdout().flush().unwrap();
        std::io::stdin().read_to_string(&mut buffer).unwrap();
        let mut p = syntax::parser::Parser::new(&buffer);

        // loop {
        match p.parse_decl() {
            Ok(d) => {
                println!("====> {:?}", d);
            }
            Err(Error {
                kind: ErrorKind::EOF,
                ..
            }) => {}
            Err(e) => {
                println!("[err] {:?}", e);
            }
        }
        // }
    }

    // let ord = exist!(
    //     kind!(*),
    //     record!(("lt", arrow!(Type::Var(0), arrow!(Type::Var(0), Type::Bool))))
    // );

    // let always_true_body = Term::new(
    //     terms::Kind::Record(terms::Record {
    //         fields: vec![Field {
    //             expr: Box::new(abs!(Type::Nat, abs!(Type::Nat, bool!(true)))),
    //             label: "lt".into(),
    //             span: Span::default(),
    //         }],
    //     }),
    //     Span::default(),
    // );
    // let always_true = pack!(Type::Nat, always_true_body, ord.clone());

    // let rec = record!(
    //     ("empty", Type::Projection(Box::new(Type::Var(0)), 0)),
    //     (
    //         "mem",
    //         arrow!(
    //             Type::Projection(Box::new(Type::Var(0)), 1),
    //             arrow!(Type::Projection(Box::new(Type::Var(0)), 0), Type::Bool)
    //         )
    //     )
    // );
    // let kinds = TyKind::Product(vec![kind!(*), kind!(*)]);
    // let sig = exist!(kinds, rec);

    // let functor_body = Term::new(
    //     terms::Kind::Record(terms::Record {
    //         fields: vec![
    //             Field {
    //                 expr: Box::new(unit!()),
    //                 label: "empty".into(),
    //                 span: Span::default(),
    //             },
    //             Field {
    //                 expr: Box::new(abs!(Type::Var(0), abs!(Type::Unit, bool!(true)))),
    //                 label: "mem".into(),
    //                 span: Span::default(),
    //             },
    //         ],
    //     }),
    //     Span::default(),
    // );

    // // let adt = tyabs!(
    // //     kind!(*),
    // //     pack!(product!(Type::Unit, Type::Var(0)), functor_body, sig)
    // // );

    // let adt = abs!(
    //     ord,
    //     unpack!(
    //         var!(0),
    //         pack!(product!(Type::Unit, Type::Var(0)), functor_body, sig)
    //     )
    // );
    // let adt = app!(adt, always_true.clone());

    // println!("\n\n{}", adt);
    // println!("{}", ctx.typecheck(&adt).unwrap());

    // let mut list = list_type();

    // println!("\n\n{}", list);
    // println!("{}", ctx.kinding(&list).unwrap());
    // // list = unfold(op_app!(list, Type::Nat));
    // list = op_app!(unfold(list), Type::Nat);

    // ctx.simplify_ty(&mut list);
    // // list = unfold(list);
    // // ctx.simplify_ty(&mut list);
    // println!("\n\n{}", list);
    // println!("k {}", ctx.kinding(&list).unwrap());

    // let mut ty = list.label("Cons").unwrap().label("tail").unwrap().clone();
    // println!("{}", ty);
    // ty = unfold(ty);
    // ctx.simplify_ty(&mut ty);
    // println!("{}", ty);

    // ty = ty.label("Cons").unwrap().label("tail").unwrap().clone();
    // // ty = unfold(ty);
    // ctx.simplify_ty(&mut ty);
    // println!("{}", ty);
}

fn list_type() -> Type {
    // type = rec /\ X. /\A. Nil | Cons A * X A
    // datatype 'a List = Cons 'a * 'a List | Nil
    // List = /\A. Nil | Cons A * List A
    let inner = tyop!(
        kind!(* => *),
        tyop!(
            kind!(*),
            sum!(
                ("Nil", Type::Unit),
                (
                    "Cons",
                    record!(
                        ("head", Type::Var(0)),
                        ("tail", op_app!(Type::Var(1), Type::Var(0)))
                    )
                )
            )
        )
    );
    Type::Recursive(Box::new(inner))
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
