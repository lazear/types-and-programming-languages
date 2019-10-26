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
use visitor::Visitable;

fn ev(ctx: &mut Context, term: Rc<Term>) -> Result<Rc<Term>, eval::Error> {
    let ty: Result<Type, TypeError> = term.accept(ctx);
    println!("TYPE: {:?}", ty);
    let r = eval::eval(&ctx, term)?;
    println!("===> {}", &r);
    println!("type {:?}", r.accept(ctx));
    Ok(r)
}

fn parse(ctx: &mut Context, input: &str) {
    let mut p = parser::Parser::new(input);
    while let Some(tok) = p.parse_term() {
        ev(ctx, tok).unwrap();
    }

    let diag = p.diagnostic();
    if diag.error_count() > 0 {
        println!("\n{} error(s) detected while parsing!", diag.error_count());
        println!("{}", diag.emit());
    }
}

fn main() {
    let mut root: Context = Context::default();

    let id = abs!(Type::Bool, var!(0));
    let f = app!(id.clone(), False);
    let mistyped = if_!(f.clone(), id.clone(), False);

    assert_eq!(root.type_of(&var!(0)), Err(TypeError::UnknownVariable));
    assert_eq!(root.type_of(&id), Ok(arrow!(Type::Bool, Type::Bool)));
    assert_eq!(root.type_of(&f), Ok(Type::Bool));
    assert_eq!(root.type_of(&mistyped), Err(TypeError::ArmMismatch));

    let ty: Result<Type, TypeError> = Rc::new(id).accept(&mut root);
    let input = "(λx: Bool -> Bool. x true) (λx: Bool. if x then false else true) ";
    // parse(input);
    // parse("iszero pred succ 0");
    // parse("(\\ y: Nat -> Bool. (\\x: Nat. if y x then true else false))");
    // parse("(\\x: Nat. \\y: Nat. if iszero x then iszero y else false) (succ 0)");
    // parse("(\\z: Nat. iszero z)");
    // parse("(\\x: Nat->Bool. \\y: Nat. if x y then true else false) (\\z: Nat. iszero z) succ 0");
    // parse("(\\x: Nat->Nat. (\\y: Nat. x succ y)) (\\x: Nat. x) succ 0");
    //

    // parse("let not = (\\x: Bool. if x then false else true) in not false");
    parse(
        &mut root,
        "let not = (\\x: Bool. if x then false else true) 
            in let x = succ 0 
                in let y = not iszero x 
                    in y",
    );

    parse(&mut root, "let not = \\x: Bool. if x then false else true in {a: 0, b: \\x: Bool. not x, c: unit}.b true");
    parse(&mut root, "type Struct = {valid: Bool, number: Nat}");
    // parse(&mut root, "(\\x: Struct. x.number) {valid: true, number: succ 0}");
    parse(
        &mut root,
        "(\\x: Struct. x.extra) {valid: false, number: succ 0, extra: unit}",
    )
    // dbg!(root);
}
