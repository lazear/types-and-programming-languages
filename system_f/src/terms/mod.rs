//! Representation lambda calculus terms
use crate::patterns::Pattern;
use crate::types::Type;
use std::fmt;
use util::span::Span;
pub mod visit;

#[derive(Clone, PartialEq, PartialOrd)]
pub struct Term {
    pub span: Span,
    pub kind: Kind,
}

/// Primitive functions supported by this implementation
#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
pub enum Primitive {
    Succ,
    Pred,
    IsZero,
}

/// Abstract syntax of the parametric polymorphic lambda calculus
#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Kind {
    /// A literal value
    Lit(Literal),
    /// A bound variable, represented by it's de Bruijn index
    Var(usize),
    /// Fixpoint operator/Y combinator
    Fix(Box<Term>),

    Primitive(Primitive),

    /// Injection into a sum type
    /// fields: type constructor tag, term, and sum type
    Injection(String, Box<Term>, Box<Type>),

    /// Product type (tuple)
    Product(Vec<Term>),
    /// Projection into a term
    Projection(Box<Term>, usize),

    /// A case expr, with case arms
    Case(Box<Term>, Vec<Arm>),

    Let(Box<Pattern>, Box<Term>, Box<Term>),
    /// A lambda abstraction
    Abs(Box<Type>, Box<Term>),
    /// Application of a term to another term
    App(Box<Term>, Box<Term>),
    /// Type abstraction
    TyAbs(Box<Term>),
    /// Type application
    TyApp(Box<Term>, Box<Type>),

    Fold(Box<Type>, Box<Term>),
    Unfold(Box<Type>, Box<Term>),
}

/// Arm of a case expression
#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Arm {
    pub span: Span,
    pub pat: Pattern,
    pub term: Box<Term>,
}

/// Constant literal expression or pattern
#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Eq, Hash)]
pub enum Literal {
    Unit,
    Bool(bool),
    Nat(u32),
}

impl Term {
    pub fn new(kind: Kind, span: Span) -> Term {
        Term { span, kind }
    }

    #[allow(dead_code)]
    pub const fn unit() -> Term {
        Term {
            span: Span::dummy(),
            kind: Kind::Lit(Literal::Unit),
        }
    }

    #[allow(dead_code)]
    #[inline]
    pub fn span(&self) -> Span {
        self.span
    }

    #[inline]
    pub fn kind(&self) -> &Kind {
        &self.kind
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Nat(n) => write!(f, "{}", n),
            Literal::Bool(b) => write!(f, "{}", b),
            Literal::Unit => write!(f, "unit"),
        }
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            Kind::Lit(lit) => write!(f, "{}", lit),
            Kind::Var(v) => write!(f, "#{}", v),
            Kind::Abs(ty, term) => write!(f, "(λ_:{:?}. {})", ty, term),
            Kind::Fix(term) => write!(f, "Fix {:?}", term),
            Kind::Primitive(p) => write!(f, "{:?}", p),
            Kind::Injection(label, tm, ty) => write!(f, "{}({})", label, tm),
            Kind::Projection(term, idx) => write!(f, "{}.{}", term, idx),
            Kind::Product(terms) => write!(
                f,
                "({})",
                terms
                    .iter()
                    .map(|t| format!("{}", t))
                    .collect::<Vec<String>>()
                    .join(",")
            ),
            Kind::Case(term, arms) => {
                writeln!(f, "case {} of", term)?;
                for arm in arms {
                    writeln!(f, "\t| {:?} => {},", arm.pat, arm.term)?;
                }
                write!(f, "")
            }
            Kind::Let(pat, t1, t2) => write!(f, "let {:?} = {} in {}", pat, t1, t2),
            Kind::App(t1, t2) => write!(f, "({} {})", t1, t2),
            Kind::TyAbs(term) => write!(f, "(λTy {})", term),
            Kind::TyApp(term, ty) => write!(f, "({} [{:?}])", term, ty),
            Kind::Fold(ty, term) => write!(f, "fold [{:?}] {}", ty, term),
            Kind::Unfold(ty, term) => write!(f, "unfold [{:?}] {}", ty, term),
        }
    }
}

impl fmt::Debug for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.kind)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn pattern_matches() {
        let ty = Type::Variant(vec![
            variant!("A", Type::Nat),
            variant!("B", Type::Product(vec![Type::Nat, Type::Bool])),
        ]);

        let a_pats = vec![
            con!("A", Pattern::Any),
            con!("A", num!(9)),
            con!("A", num!(10)),
        ];

        let b_pats = vec![
            con!("B", Pattern::Any),
            con!("B", prod!(num!(1), boolean!(true))),
            con!("B", prod!(Pattern::Any, boolean!(false))),
        ];

        let res = [true, false, true];

        let a = inj!("A", nat!(10), ty.clone());
        let b = inj!("B", tuple!(nat!(1), lit!(false)), ty.clone());

        for (pat, result) in a_pats.iter().zip(&res) {
            assert_eq!(pat.matches(&a), *result);
        }

        for (pat, result) in b_pats.iter().zip(&res) {
            assert_eq!(pat.matches(&b), *result, "{:?}", pat);
        }
    }
}
