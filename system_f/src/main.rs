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
    // let input = "(λX λY λx: X->Y. x) Nat Unit";
    // let input = "((λX λY (λx: (Y->Y->X). x)) Nat) Bool";
    // let input = "(λX λf:X->X. λa:X. f (f a)) Nat->Nat (\\x: Nat->Nat. x)";
    // letrec y = \\x: Nat. y succ x
    // let y = fix (\\y: Nat->Bool. \\x: Nat. y succ x) in y 0
    let input = "let y = fix (\\y: Nat->Bool. (\\x: Nat. iszero (succ x))) in y ";
    // let input = "let x = (\\y: Nat. succ 1) in x 0";
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
