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
use syntax::ast;
use syntax::parser::{Error, ErrorKind, Parser};
use syntax::validate;
use syntax::visit::TypeVisitor;
use terms::{Field, Kind, Record, Term};
use types::{TyKind, Type};
use util::span::Span;

fn recursive_labels(tyvars: Vec<ast::Type>, name: String, ty: ast::Type) -> ast::Type {
    let vars = ty.kind.variants();
    let mut coll = validate::TyNameCollector::default();
    coll.visit_ty(&ty);

    let recur = coll.definitions.contains(&name.as_ref());
    // let mut ty = ty;
    let sp = ty.span;

    let ty = tyvars.iter().fold(ty, |ty, tv| {
        ast::Type::new(
            ast::TypeKind::Abstraction(Box::new(ast::Kind::Star), Box::new(ty)),
            sp,
        )
    });

    let ty = if recur {
        ty
    } else {
        ast::Type::new(ast::TypeKind::Recursive(Box::new(ty)), sp)
    };
    ty
}

fn main() {
    let mut ctx = typecheck::Context::default();

    // syntax::pmc::experiment();

    loop {
        let mut buffer = String::new();
        print!("repl: ");
        std::io::stdout().flush().unwrap();
        std::io::stdin().read_to_string(&mut buffer).unwrap();
        let mut p = syntax::parser::Parser::new(&buffer);

        // loop {
        match p.parse_program() {
            Ok(d) => {
                println!("====> {:?}", &d.decls);
                println!("Validate: {:?}", validate::ProgramValidation::validate(&d));
                // match d.kind {
                //     ast::DeclKind::Datatype(tyvars, name, ty) => {
                //         println!("elab test: {:?}", recursive_labels(tyvars,
                // name, ty))     }
                //     _ => {}
                // }
            }
            Err(Error {
                kind: ErrorKind::EOF,
                ..
            }) => {}
            Err(e) => {
                println!("[err] {:?}", e);
            }
        }

        println!("parser defs: {:?}", p.definitions);
        // }
    }
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
