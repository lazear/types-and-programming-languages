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

fn main() {
    let mut ctx = typecheck::Context::default();

    syntax::pmc::experiment();

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
