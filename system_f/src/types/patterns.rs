//! Naive, inefficient exhaustiveness checking for pattern matching
//!
//! Inspired somewhat by the docs for the Rust compiler (and linked paper), we
//! create a "usefulness" predicate. We store current patterns in a row-wise
//! [`Matrix`], and iterate through each row in the matrix every time we want
//! to add a new pattern. If no existing rows completely overlap the new row,
//! then we can determine that the new row is "useful", and add it.
//!
//! To check for exhaustiveness, we simply create a row of Wildcard matches,
//! and see if it would be useful to add
//!
//! https://doc.rust-lang.org/nightly/nightly-rustc/src/rustc_mir/hair/pattern/_match.rs.html
//! http://moscova.inria.fr/~maranget/papers/warn/index.html
//!

use super::*;
use crate::terms::{Arm, Kind, Literal, Pattern, Primitive, Term};
use std::collections::HashSet;

fn overlap(existing: &Pattern, new: &Pattern) -> bool {
    use Pattern::*;
    match (existing, new) {
        (Any, _) => true,
        (Variable(_), _) => true,
        (Constructor(l, a), Constructor(l2, b)) => {
            if l == l2 {
                overlap(a, b)
            } else {
                false
            }
        }
        (Product(a), Product(b)) => a.iter().zip(b.iter()).all(|(a, b)| overlap(a, b)),
        (x, y) => x == y,
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Matrix<'pat> {
    pub expr_ty: Type,
    pub result_ty: Type,
    len: usize,
    matrix: Vec<Vec<&'pat Pattern>>,
}

impl<'pat> Matrix<'pat> {
    pub fn from_case_expr(
        ctx: &mut Context,
        expr: &Term,
        arms: &'pat [Arm],
    ) -> Result<Matrix<'pat>, TypeError> {
        let ty = ctx.type_of(expr)?;
        let mut m = Matrix::new(ty);

        let mut set = HashSet::new();
        for arm in arms {
            if pattern_type_eq(&arm.pat, &m.expr_ty) {
                let arm_ty = ctx.type_of(&arm.term)?;
                set.insert(arm_ty);
                if !m.add_pattern(&arm.pat) {
                    return Err(TypeError {
                        kind: TypeErrorKind::UnreachablePattern,
                        span: arm.span,
                    });
                }
            } else {
                return Err(TypeError {
                    kind: TypeErrorKind::InvalidPattern,
                    span: arm.span,
                });
            }
        }

        if set.len() != 1 {
            return Err(TypeError {
                kind: TypeErrorKind::IncompatibleArms,
                span: expr.span,
            });
        }

        m.result_ty = match set.into_iter().next() {
            Some(s) => s,
            None => unreachable!(),
        };

        Ok(m)
    }

    pub fn new(expr_ty: Type) -> Matrix<'pat> {
        let len = match &expr_ty {
            Type::Product(p) => p.len(),
            _ => 1,
        };

        Matrix {
            expr_ty,
            result_ty: Type::Unit,
            len,
            matrix: Vec::new(),
        }
    }

    /// Is the pattern [`Matrix`] exhaustive for this type?
    /// Invariant: a deconstructed pattern row will have the same length
    /// as the Type (primararily for tuples).
    pub fn exhaustive(&self) -> bool {
        match &self.expr_ty {
            Type::Variant(v) => v.iter().all(|variant| {
                let con = Pattern::Constructor(variant.label.clone(), Box::new(Pattern::Any));
                let temp = [&con];
                let mut ret = false;
                for row in &self.matrix {
                    if row.iter().zip(&temp).all(|(a, b)| overlap(a, b)) {
                        ret = true;
                        break;
                    }
                }
                ret
            }),
            Type::Product(_) | Type::Nat => {
                let filler = (0..self.len).map(|_| Pattern::Any).collect::<Vec<_>>();
                for row in &self.matrix {
                    if row.iter().zip(filler.iter()).all(|(a, b)| overlap(a, b)) {
                        return true;
                    }
                }
                false
            }
            Type::Bool => {
                let tru = Pattern::Literal(Literal::Bool(true));
                let fal = Pattern::Literal(Literal::Bool(false));
                !(self.can_add_row(vec![&tru]) && self.can_add_row(vec![&fal]))
            }
            Type::Unit => {
                let unit = Pattern::Literal(Literal::Unit);
                !self.can_add_row(vec![&unit])
            }
            _ => false,
        }
    }

    fn can_add_row(&self, new_row: Vec<&'pat Pattern>) -> bool {
        assert_eq!(self.len, new_row.len());
        for row in &self.matrix {
            if row.iter().zip(new_row.iter()).all(|(a, b)| overlap(a, b)) {
                return false;
            }
        }
        true
    }

    fn try_add_row(&mut self, new_row: Vec<&'pat Pattern>) -> bool {
        assert_eq!(self.len, new_row.len());
        for row in &self.matrix {
            if row.iter().zip(new_row.iter()).all(|(a, b)| overlap(a, b)) {
                return false;
            }
        }
        self.matrix.push(new_row);
        true
    }

    /// Attempt to add a new [`Pattern`] to the [`Matrix`]
    ///
    /// Returns true on success, and false if the new pattern is
    /// unreachable
    pub fn add_pattern(&mut self, pat: &'pat Pattern) -> bool {
        match pat {
            Pattern::Any | Pattern::Variable(_) => {
                let filler = (0..self.len).map(|_| &Pattern::Any).collect::<Vec<_>>();
                self.try_add_row(filler)
            }
            Pattern::Product(tuple) => self.try_add_row(tuple.iter().collect()),
            Pattern::Literal(lit) => {
                if self.len == 1 {
                    self.try_add_row(vec![pat])
                } else {
                    false
                }
            }
            Pattern::Constructor(label, inner) => self.try_add_row(vec![pat]),
        }
    }
}

fn pattern_type_eq(pat: &Pattern, ty: &Type) -> bool {
    match pat {
        Pattern::Any => true,
        Pattern::Variable(_) => true,
        Pattern::Literal(lit) => match (lit, ty) {
            (Literal::Bool(_), Type::Bool) => true,
            (Literal::Nat(_), Type::Nat) => true,
            (Literal::Unit, Type::Unit) => true,
            _ => false,
        },
        Pattern::Product(patterns) => match ty {
            Type::Product(types) => patterns
                .iter()
                .zip(types.iter())
                .all(|(pt, tt)| pattern_type_eq(pt, tt)),
            _ => false,
        },
        Pattern::Constructor(label, inner) => match ty {
            Type::Variant(v) => {
                for discriminant in v {
                    if label == &discriminant.label && pattern_type_eq(&inner, &discriminant.ty) {
                        return true;
                    }
                }
                false
            }
            _ => false,
        },
    }
}

impl Context {
    /// Helper function for pattern to type equivalence
    pub(crate) fn pattern_type_eq(&self, pat: &Pattern, ty: &Type) -> bool {
        match pat {
            Pattern::Any => true,
            Pattern::Variable(_) => true,
            Pattern::Literal(lit) => match (lit, ty) {
                (Literal::Bool(_), Type::Bool) => true,
                (Literal::Nat(_), Type::Nat) => true,
                (Literal::Unit, Type::Unit) => true,
                _ => false,
            },
            Pattern::Product(patterns) => match ty {
                Type::Product(types) => patterns
                    .iter()
                    .zip(types.iter())
                    .all(|(pt, tt)| self.pattern_type_eq(pt, tt)),
                _ => false,
            },
            Pattern::Constructor(label, inner) => match ty {
                Type::Variant(v) => {
                    for discriminant in v {
                        if label == &discriminant.label
                            && self.pattern_type_eq(&inner, &discriminant.ty)
                        {
                            return true;
                        }
                    }
                    false
                }
                _ => false,
            },
        }
    }

    fn case_type(&mut self, expr: &Term, arms: &[Arm]) -> Result<Type, TypeError> {
        let expr_ty = self.type_of(expr)?;
        // Go through and make sure all of the patterns are appropriate for
        // the expression type
        for arm in arms {
            if !self.pattern_type_eq(&arm.pat, &expr_ty) {
                println!(
                    "Pattern in case arm {:?} does not match type of {}: {:?}",
                    arm, expr, expr_ty
                );
                return Context::error(expr, TypeErrorKind::IncompatibleArms);
            }
        }
        Context::error(expr, TypeErrorKind::IncompatibleArms)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use Pattern::*;

    macro_rules! boolean {
        ($ex:expr) => {
            Literal(crate::terms::Literal::Bool($ex))
        };
    }

    macro_rules! num {
        ($ex:expr) => {
            Literal(crate::terms::Literal::Nat($ex))
        };
    }

    macro_rules! prod {
        ($($ex:expr),+) => { Product(vec![$($ex),+]) }
    }

    macro_rules! con {
        ($label:expr, $ex:expr) => {
            Constructor($label.to_string(), Box::new($ex))
        };
    }

    #[test]
    fn product() {
        let ty = Type::Product(vec![Type::Bool, Type::Bool, Type::Nat]);
        let pat = prod!(boolean!(true), boolean!(true), num!(10));
        let ctx = Context::default();
        assert!(ctx.pattern_type_eq(&pat, &ty));
    }

    #[test]
    #[should_panic]
    fn product_mistyped() {
        let ty = Type::Product(vec![Type::Bool, Type::Bool, Type::Bool]);
        let pat = prod!(boolean!(true), boolean!(true), num!(10));
        let ctx = Context::default();
        assert!(ctx.pattern_type_eq(&pat, &ty));
    }

    #[test]
    fn constructor() {
        let ty = Type::Variant(vec![
            Variant {
                label: "A".into(),
                ty: Type::Unit,
            },
            Variant {
                label: "B".into(),
                ty: Type::Nat,
            },
        ]);

        let pat1 = con!("A", Pattern::Any);
        let pat2 = con!("A", boolean!(true));
        let pat3 = con!("B", num!(1));

        let ctx = Context::default();
        assert!(ctx.pattern_type_eq(&pat1, &ty));
        assert!(!ctx.pattern_type_eq(&pat2, &ty));
        assert!(ctx.pattern_type_eq(&pat3, &ty));
    }

    #[test]
    fn constructor_product() {
        let ty = Type::Variant(vec![
            Variant {
                label: "A".into(),
                ty: Type::Unit,
            },
            Variant {
                label: "B".into(),
                ty: Type::Product(vec![Type::Nat, Type::Nat]),
            },
        ]);

        let pat1 = con!("A", Any);
        let pat2 = con!("B", Any);
        let pat3 = con!("B", prod!(Any, Variable("x".into())));
        let pat4 = con!("B", prod!(num!(1), Variable("x".into())));
        let pat5 = con!("A", num!(1));

        let ctx = Context::default();
        assert!(ctx.pattern_type_eq(&pat1, &ty));
        assert!(ctx.pattern_type_eq(&pat2, &ty));
        assert!(ctx.pattern_type_eq(&pat3, &ty));
        assert!(ctx.pattern_type_eq(&pat4, &ty));
        assert!(!ctx.pattern_type_eq(&pat5, &ty));
    }

    #[test]
    fn matrix_tuple() {
        let pats = vec![
            prod!(num!(0), num!(1)),
            prod!(num!(1), num!(1)),
            prod!(Any, num!(2)),
            prod!(num!(2), Any),
            prod!(num!(1), num!(4)),
            prod!(Any, Variable(String::default())),
        ];
        let ty = Type::Product(vec![Type::Nat, Type::Nat]);
        let mut matrix = Matrix::new(ty);
        for pat in &pats {
            assert!(matrix.add_pattern(pat));
        }
        assert!(!matrix.add_pattern(&Any));
        assert!(matrix.exhaustive());
    }

    macro_rules! variant {
        ($label:expr, $ty:expr) => {
            Variant {
                label: $label.to_string(),
                ty: $ty,
            }
        };
    }

    #[test]
    fn matrix_constructor() {
        let ty = Type::Variant(vec![
            variant!("A", Type::Nat),
            variant!("B", Type::Nat),
            variant!("C", Type::Product(vec![Type::Nat, Type::Nat])),
        ]);

        let pats = vec![
            con!("A", num!(20)),
            con!("A", Any),
            con!("B", Any),
            con!("C", prod!(num!(1), num!(1))),
            con!("C", prod!(Any, num!(1))),
            con!("C", prod!(num!(1), Any)),
        ];

        let ctx = Context::default();
        assert!(pats.iter().all(|p| ctx.pattern_type_eq(p, &ty)));
        let mut matrix = Matrix::new(ty);

        for p in &pats {
            assert!(matrix.add_pattern(p));
        }
        let last = con!("C", Any);

        assert!(!matrix.exhaustive());
        assert!(matrix.add_pattern(&last));
        assert!(matrix.exhaustive());
    }

    #[test]
    fn matrix_bool() {
        let pats = vec![boolean!(true), boolean!(false)];

        let ty = Type::Bool;
        let ctx = Context::default();
        assert!(pats.iter().all(|p| ctx.pattern_type_eq(p, &ty)));

        let mut matrix = Matrix::new(ty);
        for p in &pats {
            assert!(matrix.add_pattern(p));
        }
        assert!(!matrix.add_pattern(&pats[1]));
        assert!(matrix.exhaustive());
    }
}
