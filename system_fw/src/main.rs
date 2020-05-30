#![allow(dead_code)]
#[macro_use]
pub mod macros;
pub mod diagnostics;
pub mod elaborate;
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
use terms::Term;
use types::Type;
use util::span::Span;

fn main() {
    loop {
        let mut buffer = String::new();
        print!("repl: ");
        std::io::stdout().flush().unwrap();
        std::io::stdin().read_to_string(&mut buffer).unwrap();
        let mut p = Parser::new(&buffer);
        // let mut ctx = elaborate::ElaborationContext::new();
        // loop {
        match p.parse_program() {
            Ok(d) => {
                println!("====> {:?}", &d.decls);
                // println!("Validate: {:?}", validate::ProgramValidation::validate(&d));
                let elab = elaborate::ElaborationContext::elaborate(&d).unwrap();
                println!("-----");
                hir::bidir::test(elab);
            }
            Err(Error {
                kind: ErrorKind::EOF,
                ..
            }) => {}
            Err(e) => {
                println!("[err] {:?}", e);
            }
        }
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
