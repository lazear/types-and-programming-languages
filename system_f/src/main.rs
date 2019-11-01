#![allow(unused_variables, unused_macros)]
#[macro_use]
mod terms;
mod syntax;
mod types;

use syntax::{parser, parser::Parser, Token, TokenKind};
use types::{Type, Type::*, TypeError, TypeErrorKind};
use util;

pub fn code_format(src: &str, msgs: &[(String, util::span::Span)]) {
    println!("{}", src);
    for (msg, span) in msgs {
        let empty = (0..span.start.col).map(|_| ' ').collect::<String>();
        let tilde = (1..span.end.col.saturating_sub(span.start.col))
            .map(|_| '~')
            .collect::<String>();
        println!("{}^{}^ --- {}", empty, tilde, msg);
    }
}
fn main() {
    let mut ctx = types::Context::default();

    let id = tyabs!(Type::Var(0), abs!(Type::Var(0), var!(0)));
    let id_bool = tyapp!(id.clone(), Nat);

    // dbg!(ctx.type_of(&id_bool));
    // dbg!(ctx.type_of(&app!(id_bool, lit!(true))));

    let input = "(λX (λx: X. 0)) Bool 0";
    let mut p = Parser::new(input);

    loop {
        match p.parse() {
            Ok(term) => match ctx.type_of(&term) {
                Ok(ty) => println!("{:?}\n-: {:?}", term, ty),
                Err(tyerr) => match tyerr.kind {
                    TypeErrorKind::ParameterMismatch(t1, t2, sp) => code_format(
                        input,
                        &[
                            (format!("abstraction requires type {:?}", t1), tyerr.span),
                            (format!("but it is applied to type {:?}", t2), sp),
                        ],
                    ),
                    _ => {
                        let mut diag = util::diagnostic::Diagnostic::new(input);
                        diag.push(format!("{:?}", tyerr.kind), tyerr.span);
                        println!("Type {}", diag.emit())
                    }
                },
            },
            Err(_) => {
                break;
            }
        }
    }

    let diag = p.diagnostic();
    if diag.error_count() > 0 {
        println!("Parsing {}", diag.emit());
    }
}
