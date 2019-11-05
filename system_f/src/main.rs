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

    let input = "   let id = (\\X \\x: X. x) in 
                    let y = id Nat 0 in 
                    let z = id Bool true in 
                    z";

    let mut p = Parser::new(input);

    ctx.alias("Type".into(), arrow!(Type::Nat, Type::Bool));

    loop {
        match p.parse() {
            Ok(mut term) => {
                // replace any type aliases with the actual type
                ctx.de_alias(&mut term);
                dbg!(&term);
                match ctx.type_of(&term) {
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
                }
            }
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
