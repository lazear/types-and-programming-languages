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
    let ty = match term.accept(ctx) {
        Ok(ty) => ty,
        Err(err) => {
            println!("Mistyped term {} => {:?}", term, err);
            return Err(eval::Error::NoRuleApplies);
        }
    };
    let r = eval::eval(&ctx, term)?;

    // This is safe by our typing inference/induction rules
    // any well typed term t (checked previously) that evaluates to
    // a term t' [ t -> t' ] is also well typed
    //
    // Furthermore,  Γ t:T, t ->* t' => t':T
    let ty_ = r.accept(ctx).unwrap();
    assert_eq!(ty_, ty);
    println!("===> {} -- {:?}\n", r, ty_);

    Ok(r)
}

fn parse(ctx: &mut Context, input: &str) {
    let mut p = parser::Parser::new(input);
    while let Some(tok) = p.parse_term() {
        ev(ctx, tok);
    }

    let diag = p.diagnostic();
    if diag.error_count() > 0 {
        println!("\n{} error(s) detected while parsing!", diag.error_count());
        println!("{}", diag.emit());
    }
}

fn main() {
    let mut root: Context = Context::default();
    parse(
        &mut root,
        "let not = (\\x: Bool. if x then false else true) in
         let x = not false in 
         let y = not x in 
         if y then succ 0 else succ succ 0",
    );

    // parse(&mut root, "let not = \\x: Bool. if x then false else true in {a: 0, b: \\x: Bool. not x, c: unit}.b ");
    // parse(&mut root, "type Struct = {valid: Bool, number: Nat}");
    // parse(&mut root, "(\\x: Struct. x.number) {valid: true, number: succ 0}");
    // parse(
    //     &mut root,
    //     "(\\x: Struct. x.number) {valid: false, number: succ 0}",
    // )
    // dbg!(root);
}
