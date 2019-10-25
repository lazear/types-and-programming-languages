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

    let t = app!(Var("x".into()), Var("y".into()));
    let id = abs!("x", Type::Bool, Var("x".into()));
    dbg!(&id);
}
