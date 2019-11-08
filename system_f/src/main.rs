#![allow(unused_variables, unused_macros)]
#[macro_use]
mod terms;
mod eval;
mod syntax;
mod types;

use syntax::parser::Parser;
use terms::{Kind, Term};
use types::{Type, TypeError, TypeErrorKind, Variant};
use util;

fn test_variant() -> Type {
    Type::Variant(vec![
        Variant {
            label: "A".into(),
            ty: Type::Unit,
        },
        Variant {
            label: "B".into(),
            ty: Type::Nat,
        },
        Variant {
            label: "C".into(),
            ty: Type::Nat,
        },
    ])
}

fn bool_variant() -> Type {
    Type::Variant(vec![
        Variant {
            label: "T".into(),
            ty: Type::Unit,
        },
        Variant {
            label: "F".into(),
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

fn eval(ctx: &mut types::Context, mut term: Term) -> Result<Term, TypeError> {
    ctx.de_alias(&mut term);
    let ty = ctx.type_of(&term)?;
    println!("  -: {:?}", ty);

    let ev = eval::Eval::with_context(ctx);
    let mut t = term;
    let fin = loop {
        println!("---> {}", t);
        if let Some(res) = ev.small_step(t.clone()) {
            t = res;
        } else {
            break t;
        }
    };
    println!("===> {}", fin);
    Ok(fin)
}

// fn walk(src: &str, term: &Term) {
//     code_format(src, &[(format!("{}", term), term.span)]);
//     match &term.kind {
//         Kind::Abs(ty, tm) => {
//             walk(src, tm);
//         }
//         Kind::Fix(tm) => {
//             walk(src, tm);
//         }
//         Kind::Primitive(p) => {}
//         Kind::Constructor(label, tm, ty) => {
//             walk(src, tm);
//         }
//         Kind::Case(term, arms) => {
//             walk(src, term);
//             for a in arms {
//                 walk(src, &a.term);
//             }
//         }
//         Kind::Let(t1, t2) => {
//             walk(src, t1);
//             walk(src, t2);
//         }
//         Kind::App(t1, t2) => {
//             walk(src, t1);
//             walk(src, t2);
//         }
//         Kind::TyAbs(t) => walk(src, t),
//         Kind::TyApp(t, _) => walk(src, t),
//         _ => {}
//     }
// }

fn main() {
    let mut ctx = types::Context::default();
    let input = "let f = \\x: Var. case x of 
            | A _ => B 10 of Var 
            | B x => C succ x of Var 
            | C(y) => B succ y of Var 
        in f (f (f A of Var))";
    let mut p = Parser::new(input);

    ctx.alias("Var".into(), test_variant());
    ctx.alias("Boolv".into(), bool_variant());

    while let Ok(term) = p.parse() {
        // walk(input, &term);
        if let Err(tyerr) = eval(&mut ctx, term) {
            match tyerr.kind {
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
            }
        }
    }

    let diag = p.diagnostic();
    if diag.error_count() > 0 {
        println!("Parsing {}", diag.emit());
    }

    use std::io::Read;
    loop {
        let mut buffer = String::new();
        std::io::stdin().read_to_string(&mut buffer).unwrap();
        dbg!(&buffer);
        let mut p = Parser::new(&buffer);
        while let Ok(term) = p.parse() {
            if let Err(tyerr) = eval(&mut ctx, term) {
                match tyerr.kind {
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
                }
            }
        }

        let diag = p.diagnostic();
        if diag.error_count() > 0 {
            println!("Parsing {}", diag.emit());
        }
    }
}
