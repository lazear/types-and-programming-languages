#![allow(unused_macros)]
/// Boolean term
macro_rules! bool {
    ($x:expr) => {
        crate::terms::Term::new(
            crate::terms::Kind::Const(crate::terms::Constant::Bool($x)),
            util::span::Span::dummy(),
        )
    };
}

/// Integer term
macro_rules! nat {
    ($x:expr) => {
        crate::terms::Term::new(
            crate::terms::Kind::Const(crate::terms::Constant::Nat($x)),
            util::span::Span::dummy(),
        )
    };
}

macro_rules! unit {
    () => {
        crate::terms::Term::new(
            crate::terms::Kind::Const(crate::terms::Constant::Unit),
            util::span::Span::dummy(),
        )
    };
}

/// TmVar term
macro_rules! var {
    ($x:expr) => {
        crate::terms::Term::new(crate::terms::Kind::Var($x), util::span::Span::dummy())
    };
}

/// Application term
macro_rules! app {
    ($t1:expr, $t2:expr) => {
        crate::terms::Term::new(
            crate::terms::Kind::App(Box::new($t1), Box::new($t2)),
            util::span::Span::dummy(),
        )
    };
}

/// Lambda abstraction term
macro_rules! abs {
    ($ty:expr, $t:expr) => {
        crate::terms::Term::new(
            crate::terms::Kind::Abs(Box::new($ty), Box::new($t)),
            util::span::Span::dummy(),
        )
    };
}

/// Type application term
macro_rules! tyapp {
    ($t1:expr, $t2:expr) => {
        crate::terms::Term::new(
            crate::terms::Kind::TyApp(Box::new($t1), Box::new($t2)),
            util::span::Span::dummy(),
        )
    };
}

/// Type abstraction term
macro_rules! tyabs {
    ($k:expr, $t:expr) => {
        crate::terms::Term::new(
            crate::terms::Kind::TyAbs(Box::new($k), Box::new($t)),
            util::span::Span::dummy(),
        )
    };
}

macro_rules! pack {
    ($ty1:expr, $t:expr, $ty2:expr) => {
        crate::terms::Term::new(
            crate::terms::Kind::Pack(Box::new($ty1), Box::new($t), Box::new($ty2)),
            util::span::Span::dummy(),
        )
    };
}

macro_rules! unpack {
    ($t1:expr, $t2:expr) => {
        crate::terms::Term::new(
            crate::terms::Kind::Unpack(Box::new($t1), Box::new($t2)),
            util::span::Span::dummy(),
        )
    };
}

macro_rules! access {
    ($t1:expr, $t2:expr) => {
        crate::terms::Term::new(
            crate::terms::Kind::Index(Box::new($t1), $t2.into()),
            util::span::Span::dummy(),
        )
    };
}

macro_rules! exist {
    ($k:expr, $ty:expr) => {
        crate::types::Type::Existential(Box::new($k), Box::new($ty))
    };
}

macro_rules! univ {
    ($ty:expr) => {
        crate::types::Type::Universal(Box::new(kind!(*)), Box::new($ty))
    };
    ($k:expr, $ty:expr) => {
        crate::types::Type::Universal(Box::new($k), Box::new($ty))
    };
}

macro_rules! arrow {
    ($ty1:expr, $ty2:expr) => {
        crate::types::Type::Arrow(Box::new($ty1), Box::new($ty2))
    };
}

macro_rules! field {
    ($name:expr, $ty:expr) => {
        crate::types::TyField {
            label: $name.to_string(),
            ty: Box::new($ty),
        }
    };
}

macro_rules! record {
    ($($name:expr),+) => {
        crate::types::Type::Record(vec![$(field!($name.0, $name.1)),+])
    }
}

macro_rules! sum {
    ($($name:expr),+) => {
        crate::types::Type::Sum(vec![$(field!($name.0, $name.1)),+])
    }
}

macro_rules! product {
    ($($name:expr),+) => {
        crate::types::Type::Product(vec![$($name),+])
    }
}

macro_rules! tyop {
    ($k:expr, $ty:expr) => {
        crate::types::Type::Abs(Box::new($k), Box::new($ty))
    };
}

macro_rules! op_app {
    ($ty1:expr, $ty2:expr) => {
        crate::types::Type::App(Box::new($ty1), Box::new($ty2))
    };
}

macro_rules! kind {
    (*) => { crate::types::TyKind::Star };
    (* => *) => { crate::types::TyKind::Arrow(Box::new(kind!(*)), Box::new(kind!(*))) };
    ($ex:expr => $ex2:expr) => { crate::types::TyKind::Arrow(Box::new($ex), Box::new($ex2)) };

}

macro_rules! diag {
    ($sp:expr, $str:expr) => { Err(crate::diagnostics::Diagnostic::error($sp, $str)) };
    ($sp:expr, $fmt:expr, $($args:expr),+) => { Err(crate::diagnostics::Diagnostic::error($sp, format!($fmt, $($args),+))) };
}
