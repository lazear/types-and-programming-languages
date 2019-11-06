#![allow(unused_variables, unused_macros)]
#[macro_use]
mod terms;
mod eval;
mod syntax;
mod types;

use syntax::{parser, parser::Parser, Token, TokenKind};
use terms::{Arm, Kind, Literal, Pattern, Term};
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
            ty: Type::Unit,
        },
        Variant {
            label: "C".into(),
            ty: Type::Bool,
        },
    ])
}

fn case_expr() -> Term {
    let expr = Term::new(
        Kind::Constructor("C".into(), Box::new(lit!(false)), Box::new(test_variant())),
        util::span::Span::default(),
    );

    let arms = vec![
        Arm {
            pat: Pattern::Constructor("A".into()),
            term: Box::new(nat!(1)),
            span: util::span::Span::default(),
        },
        Arm {
            pat: Pattern::Constructor("B".into()),
            term: Box::new(nat!(2)),
            span: util::span::Span::default(),
        },
        Arm {
            pat: Pattern::Constructor("C".into()),
            term: Box::new(var!(0)),
            span: util::span::Span::default(),
        },
    ];

    Term::new(
        Kind::Case(Box::new(expr), arms),
        util::span::Span::default(),
    )
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

fn eval(ctx: &mut types::Context, mut term: Term) {
    ctx.de_alias(&mut term);
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
    println!("===> {}", fin)
}
fn main() {
    let mut ctx = types::Context::default();

    // let input = "   let id = (\\X \\x: X. x) in
    //                 let y = id Nat 0 in
    //                 let z = id Bool true in
    //                 z";
    // let input = "let id = (\\X (\\x: X. x)) Nat in let y = (\\z: Nat. id z) in y 1";
    //
    let input = "case C 10 of {A | B Nat | C Nat} of 
        | A => 0
        | B x => succ x 
        | C x => pred x";
    let mut p = Parser::new(input);

    ctx.alias("Type".into(), arrow!(Type::Nat, Type::Bool));

    // let mut tm = case_expr();
    // ctx.de_alias(&mut tm);
    // dbg!(ctx.type_of(&tm));
    // eval(&mut ctx, tm);

    loop {
        match p.parse() {
            Ok(mut term) => match ctx.type_of(&term) {
                Ok(ty) => {
                    println!("{}\n-: {:?}", term, ty);
                    eval(&mut ctx, term);
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
