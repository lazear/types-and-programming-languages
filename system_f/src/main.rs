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

fn case_expr() -> Term {
    let expr = Term::new(
        Kind::Constructor(
            "True".into(),
            Box::new(Term::unit()),
            Box::new(Type::Alias("Boolv".into())),
        ),
        util::span::Span::default(),
    );

    let arms = vec![
        Arm {
            pat: Pattern::Constructor("True".into()),
            term: Box::new(nat!(1)),
            span: util::span::Span::default(),
        },
        // Arm {
        //     pat: Pattern::Constructor("False".into()),
        //     term: Box::new(nat!(2)),
        //     span: util::span::Span::default(),
        // },
        Arm {
            pat: Pattern::Any,
            term: Box::new(nat!(3)),
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
    let input = "let id = (\\X (\\x: X. x)) Nat in let y = (\\z: Nat. id z) in y 1";
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

    let mut tm = case_expr();
    ctx.de_alias(&mut tm);
    dbg!(ctx.type_of(&tm));

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
