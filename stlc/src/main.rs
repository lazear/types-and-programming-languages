#[macro_use]
mod term;
mod eval;
mod typing;

use std::rc::Rc;
use term::Term;
use term::Term::*;
use typing::{Context, Type, TypeError};

fn ev(term: Term) -> Result<Rc<Term>, eval::Error> {
    let ctx = Context::default();
    println!("eval {:?}", &term);
    let r = eval::eval(&ctx, Rc::new(term))?;
    println!("===> {:?}", &r);
    println!("type {:?}", ctx.type_of(&r));
    Ok(r)
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

    // (\x: Bool. (\y: Bool. if x then y else false) true) true

    let func = abs!(
        Type::Bool,
        app!(abs!(Type::Bool, if_!(var!(1), var!(0), False)), True)
    );

    // The simply typed lambda calculus cannot type divergent combinator
    // or fixpoint
    let x = abs!(arrow!(Type::Bool, Type::Bool), app!(var!(0), var!(0)));
    let omega = app!(x.clone(), x.clone());
    ev(x);
    ev(omega);
}
