#![allow(dead_code)]
#[macro_use]
pub mod macros;
pub mod diagnostics;
pub mod functor;
pub mod hir;
pub mod stack;
pub mod syntax;
pub mod terms;
pub mod typecheck;
pub mod types;

use std::io::prelude::*;
use syntax::ast;
use syntax::parser::{Error, ErrorKind, Parser};
use syntax::validate;
use syntax::visit::{DeclVisitor, TypeVisitor};
use terms::Term;
use types::Type;
use util::span::Span;

fn recursive_labels(tyvars: Vec<ast::Type>, name: String, ty: ast::Type) -> ast::Type {
    // let vars = ty.kind.variants();
    let mut coll = validate::TyNameCollector::default();
    coll.visit_ty(&ty);

    let recur = coll.definitions.contains(&name.as_ref());
    // let mut ty = ty;
    let sp = ty.span;

    let ty = tyvars.iter().fold(ty, |ty, tv| {
        ast::Type::new(
            ast::TypeKind::Abstraction(
                tv.kind.as_tyvar().into(),
                Box::new(ast::Kind::Star),
                Box::new(ty),
            ),
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
    // let mut ctx = typecheck::Context::default();

    // syntax::pmc::experiment();

    loop {
        let mut buffer = String::new();
        print!("repl: ");
        std::io::stdout().flush().unwrap();
        std::io::stdin().read_to_string(&mut buffer).unwrap();
        let mut p = Parser::new(&buffer);
        let mut ctx = validate::elaborate::ElaborationContext::new();
        // loop {
        match p.parse_program() {
            Ok(d) => {
                println!("====> {:?}", &d.decls);
                // println!("Validate: {:?}", validate::ProgramValidation::validate(&d));
                ctx.elab_program(&d).unwrap();
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
}

fn unfold(ty: Type) -> Type {
    match &ty {
        Type::Recursive(inner) => op_app!(*inner.clone(), ty),
        Type::App(a, b) => match a.as_ref() {
            Type::Recursive(_) => op_app!(unfold(*a.clone()), *b.clone()),
            _ => ty,
        },
        _ => ty,
    }
}
