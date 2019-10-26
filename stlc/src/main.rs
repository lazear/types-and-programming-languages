#![allow(unused_variables)]
#[macro_use]
mod term;
mod eval;
mod lexer;
mod parser;
mod typing;
mod visitor;

use std::rc::Rc;
use term::Term;
use term::Term::*;
use typing::{Context, Type, TypeError};

fn ev(term: Rc<Term>) -> Result<Rc<Term>, eval::Error> {
    let ctx = Context::default();
    println!("EVAL {}  TYPE: {:?}", &term, ctx.type_of(&term));
    let r = eval::eval(&ctx, term)?;
    println!("===> {}", &r);
    println!("type {:?}", ctx.type_of(&r));
    Ok(r)
}

fn parse(input: &str) {
    let mut p = parser::Parser::new(input);
    while let Some(tok) = p.parse_term() {
        ev(tok);
    }
    println!("");

    let diag = p.diagnostic();
    if diag.error_count() > 0 {
        println!("\n{} error(s) detected while parsing!", diag.error_count());
        println!("{}", diag.emit());
    }
}

fn exercises() {
    let mut v = visitor::Shifting {
        cutoff: 0,
        shift: 2,
    };

    // Exercise 6.2.2 part 1
    let arr = arrow!(Type::Bool, Type::Bool);
    let ex1 = abs!(
        arr.clone(),
        abs!(arr.clone(), app!(app!(var!(1), var!(0)), var!(2)))
    );
    dbg!(ex1.accept(&mut v));

    // Exercise 6.2.2 part 2
    let ex2 = abs!(
        arr.clone(),
        app!(
            app!(var!(0), var!(1)),
            abs!(Type::Bool, app!(app!(var!(0), var!(1)), var!(2)))
        )
    );
    dbg!(ex2.accept(&mut v));
}

fn main() {
    let root: Context = Context::default();

    let id = abs!(Type::Bool, var!(0));
    let f = app!(id.clone(), False);
    let mistyped = if_!(f.clone(), id.clone(), False);

    assert_eq!(root.type_of(&var!(0)), Err(TypeError::UnknownVariable));
    assert_eq!(root.type_of(&id), Ok(arrow!(Type::Bool, Type::Bool)));
    assert_eq!(root.type_of(&f), Ok(Type::Bool));
    assert_eq!(root.type_of(&mistyped), Err(TypeError::ArmMismatch));

    let input = "(λx: Bool -> Bool. x true) (λx: Bool. if x then false else true) ";
    // parse(input);
    // parse("iszero pred succ 0");
    // parse("(\\ y: Nat -> Bool. (\\x: Nat. if y x then true else false))");
    // parse("(\\x: Nat. \\y: Nat. if iszero x then iszero y else false) (succ 0)");
    // parse("(\\z: Nat. iszero z)");
    parse("(\\x: Nat->Bool. \\y: Nat. if x y then true else false) (\\z: Nat. iszero z) succ 0");
    parse("(\\x: Nat->Nat. (\\y: Nat. x y)) (\\x: Nat. x)");
}
