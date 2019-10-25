mod term;
use term::Term::*;
use term::Type;

macro_rules! app {
    ($ex:expr, $xy:expr) => {
        App(Box::new($ex), Box::new($xy))
    };
}

macro_rules! abs {
    ($var:expr, $ty:expr, $body:expr) => {
        Abs($var.into(), $ty, Box::new($body))
    };
}

macro_rules! var {
    ($var:expr) => {
        Var($var.into())
    };
}

macro_rules! if_ {
    ($a:expr, $b:expr, $c:expr) => {
        If(Box::new($a), Box::new($b), Box::new($c))
    };
}

macro_rules! arrow {
    ($a:expr, $b:expr) => {
        Type::Arrow(Box::new($a), Box::new($b))
    };
}

fn main() {
    println!("Hello, world!");

    let root = term::Context::root();

    let child = root.add("x".into(), term::Binding::Variable(term::Type::Bool));
    let child2 = child.add(
        "y".into(),
        term::Binding::Variable(term::Type::Arrow(
            Box::new(term::Type::Bool),
            Box::new(term::Type::Bool),
        )),
    );

    let id = abs!("x", Type::Bool, var!("x"));
    let f = app!(id.clone(), False);

    let mistyped = if_!(f.clone(), id.clone(), False);

    assert_eq!(root.type_of(&id), Ok(arrow!(Type::Bool, Type::Bool)));
    assert_eq!(root.type_of(&f), Ok(Type::Bool));
    assert_eq!(root.type_of(&mistyped), Err(term::TypeError::ArmMismatch));
}
