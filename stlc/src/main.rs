#[macro_use]
mod term;
mod eval;
mod typing;

use eval::subst_top;
use term::Term;
use term::Term::*;
use typing::{Context, Type, TypeError};

fn ev(term: Term) -> Result<Term, eval::Error> {
    let ctx = Context::default();
    println!("eval {:?}", &term);
    let r = eval::eval(&ctx, term)?;
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

    let i = abs!(Type::Bool, var!(0));
    let k = abs!(Type::Bool, abs!(Type::Bool, var!(1)));
    // let s = abs!(arrow!(Type::Bool, Type::Bool), abs!());

    // if false then true else false

    // ev(app!(i, True));
    ev(i);
}
