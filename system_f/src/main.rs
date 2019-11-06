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

    // let input = "   let id = (\\X \\x: X. x) in
    //                 let y = id Nat 0 in
    //                 let z = id Bool true in
    //                 z";
    // let input = "let id = (\\X (\\x: X. x)) Nat in let y = (\\z: Nat. id z) in y
    // 1";
    //
    // let input = "let x = 1 in case C 10 of Var of
    //     | A => 0
    //     | B x => succ x
    //     | C x => pred x";

    // let input = "let func = (\\x: Var. case x of | A => 0 | B x => succ x | C y
    // => pred y) in func C 2 of Var"; let input = "let polyid = (\\X \\x: X. x)
    // in (\\x: Nat. polyid [Bool] false) (polyid [Nat] 0)";
    let input = "let ifz = \\q: Bool. \\y: Nat. \\z: Nat. case q of | true => y | _ => false in ifz false 10 20";
    // let input = "let f = \\x: Var. case x of | A => B 10 of Var | B x => C succ x
    // of Var | C b => B succ b of Var in f (f (f A of Var))";
    let mut p = Parser::new(input);

    ctx.alias("Var".into(), test_variant());
    ctx.alias("Boolv".into(), bool_variant());

    loop {
        match p.parse() {
            Ok(term) => {
                // walk(input, &term);
                match eval(&mut ctx, term) {
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
                    _ => {}
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
