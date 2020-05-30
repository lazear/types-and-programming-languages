//! Macros to make writing tests easier

/// Boolean term
macro_rules! lit {
    ($x:expr) => {
        crate::terms::Term::new(
            crate::terms::Kind::Lit(crate::terms::Literal::Bool($x)),
            util::span::Span::dummy(),
        )
    };
}

/// Integer term
macro_rules! nat {
    ($x:expr) => {
        crate::terms::Term::new(
            crate::terms::Kind::Lit(crate::terms::Literal::Nat($x)),
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
    ( $t:expr) => {
        crate::terms::Term::new(crate::terms::Kind::TyAbs(Box::new($t)), util::span::Span::dummy())
    };
}

/// Primitive term
macro_rules! prim {
    ($t:expr) => {
        crate::terms::Term::new(crate::terms::Kind::Primitive($t), util::span::Span::dummy())
    };
}

macro_rules! inj {
    ($label:expr, $t:expr, $ty:expr) => {
        crate::terms::Term::new(
            crate::terms::Kind::Injection($label.to_string(), Box::new($t), Box::new($ty)),
            util::span::Span::dummy(),
        )
    };
}

/// Product term
macro_rules! tuple {
    ($($ex:expr),+) => { crate::terms::Term::new(crate::terms::Kind::Product(vec![$($ex),+]),
    util::span::Span::dummy()) }
}

/// Type arrow
macro_rules! arrow {
    ($ty1:expr, $ty2:expr) => {
        crate::types::Type::Arrow(Box::new($ty1), Box::new($ty2))
    };
}

/// Boolean pattern
macro_rules! boolean {
    ($ex:expr) => {
        crate::patterns::Pattern::Literal(crate::terms::Literal::Bool($ex))
    };
}

/// Numeric pattern
macro_rules! num {
    ($ex:expr) => {
        crate::patterns::Pattern::Literal(crate::terms::Literal::Nat($ex))
    };
}

/// Product pattern
macro_rules! prod {
    ($($ex:expr),+) => { crate::patterns::Pattern::Product(vec![$($ex),+]) }
}

/// Constructor pattern
macro_rules! con {
    ($label:expr, $ex:expr) => {
        crate::patterns::Pattern::Constructor($label.to_string(), Box::new($ex))
    };
}

/// Variant type
macro_rules! variant {
    ($label:expr, $ty:expr) => {
        crate::types::Variant {
            label: $label.to_string(),
            ty: $ty,
        }
    };
}
