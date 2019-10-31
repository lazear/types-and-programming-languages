#![allow(unused_variables, unused_macros)]
#[macro_use]
mod terms;
mod syntax;
mod types;

use syntax::{parser, parser::Parser, Token, TokenKind};
use types::{Type, Type::*, TypeError};
use util;

fn main() {
    let mut ctx = types::Context::default();

    let id = tyabs!(Type::Var(0), abs!(Type::Var(0), var!(0)));
    let id_bool = tyapp!(id.clone(), Nat);

    // dbg!(ctx.type_of(&id_bool));
    // dbg!(ctx.type_of(&app!(id_bool, lit!(true))));

    let input = "(\\X (\\x: X. 0)) Nat 0";
    let mut p = Parser::new(input);

    loop {
        match p.parse() {
            Ok(term) => match ctx.type_of(&term) {
                Ok(ty) => println!("{:?}\n-: {:?}", term, ty),
                Err(tyerr) => {
                    let mut diag = util::diagnostic::Diagnostic::new(input);
                    diag.push(format!("{:?}", tyerr.kind), tyerr.span);
                    println!("Type {}", diag.emit())
                }
            },
            Err(_) => {
                break;
            }
        }
    }

    let diag = p.diagnostic();
    if diag.error_count() > 0 {
        println!("Parsing {}", p.diagnostic().emit());
    }
}
