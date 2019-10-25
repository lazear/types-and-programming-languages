#[macro_use]
mod term;
mod typing;
use typing::{Context, Type, TypeError};

use term::Term::*;

fn main() {
    println!("Hello, world!");

    let root: Context = Context::default();

    assert_eq!(root.type_of(&var!(0)), Err(TypeError::UnknownVariable));

    let id = abs!(Type::Bool, var!(0));
    let f = app!(id.clone(), False);

    let mistyped = if_!(f.clone(), id.clone(), False);

    let test = app!(
        abs!(
            arrow!(Type::Bool, Type::Bool),
            if_!(app!(var!(0), False), True, False)
        ),
        abs!(Type::Bool, if_!(var!(0), False, True))
    );

    // (\x: Bool. (\y: Bool. if x then y else false) true) true

    let func = abs!(
        Type::Bool,
        app!(abs!(Type::Bool, if_!(var!(1), var!(0), False)), True)
    );
    dbg!(root.type_of(&func));

    println!("type_of (\\x:Bool->Bool. if x false then true else false) (\\ x:Bool. if x then false else true)\n==> {:?}\n{:?}",
        root.type_of(&test),
        &test
    );

    assert_eq!(root.type_of(&id), Ok(arrow!(Type::Bool, Type::Bool)));
    assert_eq!(root.type_of(&f), Ok(Type::Bool));
    assert_eq!(root.type_of(&mistyped), Err(TypeError::ArmMismatch));
}
