#[macro_use]
mod term;
mod eval;
mod typing;
mod visitor;

use std::rc::Rc;
use term::Term;
use term::Term::*;
use typing::{Context, Type, TypeError};

fn ev(term: Rc<Term>) -> Result<Rc<Term>, eval::Error> {
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

    // The simply typed lambda calculus cannot type divergent combinator
    // or fixpoint
    let x = abs!(arrow!(Type::Bool, Type::Bool), app!(var!(0), var!(0)));
    let omega = app!(x.clone(), x.clone());
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
    // dbg!(ex1.accept(&mut v));

    // Exercise 6.2.2 part 2
    let ex2 = abs!(
        arr.clone(),
        app!(
            app!(var!(0), var!(1)),
            abs!(Type::Bool, app!(app!(var!(0), var!(1)), var!(2)))
        )
    );
    // dbg!(ex2.accept(&mut v));

    let a = abs!(arr.clone(), app!(app!(var!(1), var!(0)), var!(2)));
    let b = abs!(arr.clone(), app!(var!(1), var!(0)));

    // \ y: Bool (\ x: Bool if x then y else false)

    let c = abs!(Type::Bool, abs!(Type::Bool, if_!(var!(0), var!(1), False)));
    // let r = ev(app!(c, True).into()).unwrap();
    // ev(Term::App(r, Term::False.into()).into());

    let c = abs!(Type::Bool, abs!(Type::Bool, if_!(var!(0), var!(1), False)));
    ev(app!(app!(c, True), False).into());
}
