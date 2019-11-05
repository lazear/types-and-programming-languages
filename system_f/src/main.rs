#![allow(unused_variables, unused_macros)]
#[macro_use]
mod terms;
mod eval;
mod syntax;
mod types;

use syntax::{parser, parser::Parser, Token, TokenKind};
use terms::{Kind, Term};
use types::{Type, TypeError, TypeErrorKind, Variant};
use util;

fn bool_variant() -> Type {
    Type::Variant(vec![
        Variant {
            label: "True".into(),
            ty: Type::Unit,
        },
        Variant {
            label: "False".into(),
            ty: Type::Unit,
        },
    ])
}

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
    let input = "(\\x: Boolv. x)";
    let mut p = Parser::new(input);

    ctx.alias("Type".into(), arrow!(Type::Nat, Type::Bool));
    ctx.alias("Boolv".into(), bool_variant());

    let mut test = Term::new(
        Kind::Constructor(
            "True".into(),
            Box::new(Term::unit()),
            Box::new(Type::Alias("Boolv".into())),
        ),
        util::span::Span::default(),
    );
    ctx.de_alias(&mut test);
    dbg!(ctx.type_of(&test));

    loop {
        match p.parse() {
            Ok(mut term) => {
                // replace any type aliases with the actual type
                ctx.de_alias(&mut term);
                dbg!(&term);
                match ctx.type_of(&term) {
                    Ok(ty) => {
                        println!("{:?}\n-: {:?}", term, ty);
                        let ev = eval::Eval::with_context(&ctx);
                        let r = ev.small_step(term);
                        // let r = ev.small_step(r);
                        dbg!(r);
                    }
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
