#![allow(unused_variables, unused_macros)]
#[macro_use]
pub mod macros;
pub mod eval;
pub mod syntax;
pub mod terms;
pub mod types;

use std::env;
use std::io::{Read, Write};
use syntax::parser::{self, Parser};
use terms::visit::MutVisitor;
use terms::*;
use types::*;
use util::span::Span;

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

pub fn code_format(src: &str, msgs: &[(String, util::span::Span)]) {
    let lines = msgs
        .iter()
        .map(|(_, sp)| sp.start.line)
        .collect::<std::collections::HashSet<_>>();
    let srcl = src.lines().collect::<Vec<&str>>();
    for line in lines {
        println!("{} {}", line, &srcl[line as usize]);
    }

    for (msg, span) in msgs {
        let empty = (0..span.start.col + 2).map(|_| ' ').collect::<String>();
        let tilde = (1..span.end.col.saturating_sub(span.start.col))
            .map(|_| '~')
            .collect::<String>();
        println!("{}^{}^ --- {}", empty, tilde, msg);
    }
}

fn eval(ctx: &mut types::Context, mut term: Term, verbose: bool) -> Result<Term, TypeError> {
    ctx.de_alias(&mut term);
    InjRewriter.visit(&mut term);
    let ty = ctx.type_check(&term)?;
    println!("  -: {:?}", ty);

    let ev = eval::Eval::with_context(ctx);
    let mut t = term;
    let fin = loop {
        if let Some(res) = ev.small_step(t.clone()) {
            t = res;
        } else {
            break t;
        }
        if verbose {
            println!("---> {}", t);
        }
    };
    println!("===> {}", fin);
    let fty = ctx.type_check(&fin)?;
    if fty != ty {
        panic!(
            "Type of term after evaluation is different than before!\n1 {:?}\n2 {:?}",
            ty, fty
        );
    }
    Ok(fin)
}

struct InjRewriter;

impl MutVisitor for InjRewriter {
    fn visit(&mut self, term: &mut Term) {
        match &mut term.kind {
            Kind::Injection(label, val, ty) => {
                // dbg!(&ty);
                match *ty.clone() {
                    Type::Rec(inner) => {
                        let ty_prime = subst(*ty.clone(), *inner.clone());
                        let rewrite_ty = Term::new(
                            Kind::Injection(label.clone(), val.clone(), Box::new(ty_prime)),
                            term.span,
                        );

                        *term = Term::new(Kind::Fold(ty.clone(), Box::new(rewrite_ty)), term.span);

                        // let s = subst(*ty.clone(), *inner.clone());
                        // *term = Term::new(
                        //     Kind::App(
                        //         Box::new(tm),
                        //         Box::new(Term::new(
                        //             Kind::Injection(label.clone(),
                        // val.clone(), Box::new(s)),
                        //             term.span,
                        //         )),
                        //     ),
                        //     term.span,
                        // );
                    }
                    _ => {}
                }
                self.walk(term);
            }
            _ => self.walk(term),
        }
    }
}

fn parse_and_eval(ctx: &mut types::Context, input: &str, verbose: bool) -> bool {
    let mut p = Parser::new(input);
    loop {
        let term = match p.parse() {
            Ok(term) => term,
            Err(parser::Error {
                kind: parser::ErrorKind::Eof,
                ..
            }) => break,
            Err(e) => {
                dbg!(e);
                break;
            }
        };
        if let Err(tyerr) = eval(ctx, term, verbose) {
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
            return false;
        }
    }
    let diag = p.diagnostic();
    if diag.error_count() > 0 {
        println!("Parsing {}", diag.emit());
        false
    } else {
        true
    }
}

fn nat_list() -> Type {
    Type::Rec(Box::new(Type::Variant(vec![
        variant!("Nil", Type::Unit),
        variant!("Cons", Type::Product(vec![Type::Nat, Type::Var(0)])),
    ])))
}

fn nat_list2() -> Type {
    Type::Variant(vec![
        variant!("Nil", Type::Unit),
        variant!("Cons", Type::Product(vec![Type::Nat, Type::Var(0)])),
    ])
}

fn main() {
    let mut ctx = types::Context::default();

    ctx.alias("Var".into(), test_variant());
    ctx.alias("NatList".into(), nat_list());
    ctx.alias("NB".into(), nat_list2());

    let args = env::args();
    if args.len() > 1 {
        for f in args.skip(1) {
            println!("reading {}", f);
            let file = std::fs::read_to_string(&f).unwrap();
            if !parse_and_eval(&mut ctx, &file, false) {
                panic!("test failed! {}", f);
            }
        }
    }

    loop {
        let mut buffer = String::new();
        print!("repl: ");
        std::io::stdout().flush().unwrap();
        std::io::stdin().read_to_string(&mut buffer).unwrap();

        parse_and_eval(&mut ctx, &buffer, true);
    }
}
