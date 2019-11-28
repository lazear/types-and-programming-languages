#![allow(unused_variables, unused_macros)]
#[macro_use]
pub mod macros;
pub mod diagnostics;
pub mod eval;
pub mod patterns;
pub mod syntax;
pub mod terms;
pub mod types;
pub mod visit;

use diagnostics::*;
use std::env;
use std::io::{Read, Write};
use syntax::parser::{self, Parser};
use terms::{visit::InjRewriter, Term};
use types::{Type, Variant};
use visit::MutTermVisitor;

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

pub fn code_format(src: &str, diag: Diagnostic) {
    // let lines = diag.ot
    //     .iter()
    //     .map(|(_, sp)| sp.start.line)
    //     .collect::<std::collections::HashSet<_>>();
    let srcl = src.lines().collect::<Vec<&str>>();

    let mut msgs = diag.other.clone();
    msgs.insert(0, diag.primary.clone());

    for line in diag.lines() {
        println!("| {} {}", line + 1, &srcl[line as usize]);
        for anno in &msgs {
            if anno.span.start.line != line {
                continue;
            }
            let empty = (0..anno.span.start.col + 3)
                .map(|_| ' ')
                .collect::<String>();
            let tilde = (1..anno.span.end.col.saturating_sub(anno.span.start.col))
                .map(|_| '~')
                .collect::<String>();
            println!("{}^{}^ --- {}", empty, tilde, anno.info);
        }
    }
}

fn eval(ctx: &mut types::Context, mut term: Term, verbose: bool) -> Result<Term, Diagnostic> {
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
        if let Err(diag) = eval(ctx, term, verbose) {
            code_format(input, diag);
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
        return;
    }

    loop {
        let mut buffer = String::new();
        print!("repl: ");
        std::io::stdout().flush().unwrap();
        std::io::stdin().read_to_string(&mut buffer).unwrap();

        parse_and_eval(&mut ctx, &buffer, true);
    }
}
