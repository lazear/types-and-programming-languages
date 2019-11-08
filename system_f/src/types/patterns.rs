//! https://doc.rust-lang.org/nightly/nightly-rustc/src/rustc_mir/hair/pattern/_match.rs.html#352

use super::*;
use crate::terms::{Arm, Kind, Literal, Pattern, Primitive, Term};
use std::{collections::HashSet, hash::Hash};

fn overlap(existing: &Pattern, new: &Pattern) -> bool {
    use Pattern::*;
    match (existing, new) {
        (Any, _) => true,
        (Variable(_), _) => true,
        (x, y) => x == y,
    }
}

fn row_overlap(existing: &[&Pattern], new: &[Pattern]) -> bool {
    existing.iter().zip(new.iter()).all(|(a, b)| overlap(a, b))
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Matrix<'pat> {
    len: usize,
    matrix: Vec<Vec<&'pat Pattern>>,
}

pub fn pattern_is_useful(pats: &[Pattern], len: usize) -> Option<Matrix<'_>> {
    let mut matrix: Vec<Vec<&Pattern>> = Vec::new();

    for pat in pats {
        match pat {
            Pattern::Variable(_) | Pattern::Any => {
                let filler = (0..len).map(|_| Pattern::Any).collect::<Vec<_>>();
                for row in &matrix {
                    if row_overlap(row, &filler) {
                        println!("pattern {:?} is not useful\n{:?}", pat, matrix);
                        return None;
                    }
                }
            }
            Pattern::Product(tuple) => {
                for row in &matrix {
                    if row_overlap(row, tuple) {
                        println!("pattern {:?} is not useful\n{:?}", pat, matrix);
                        return None;
                    }
                }
                matrix.push(tuple.iter().collect());
            }
            Pattern::Literal(_) | Pattern::Constructor(_, _) => {
                panic!("invalid pattern given to pattern_is_useful")
            }
        }
    }

    Some(Matrix { matrix, len })
}

impl<'pat> Matrix<'pat> {
    pub fn exhaustive(&self) -> bool {
        let filler = (0..self.len).map(|_| Pattern::Any).collect::<Vec<_>>();
        for row in &self.matrix {
            if row_overlap(row, &filler) {
                return true;
            }
        }
        false
    }
}

fn exhaustive_product(pats: &[Pattern], product: &[Type]) -> bool {
    // Strategy here is to go through each type in the product, and make sure
    // that our pattern is exhaustive for that type
    //
    // Given a product type (ty1 * ty2 * ty3) we will first see if ty1
    // is exhausted, then ty2, etc
    let mut ex = (0..product.len()).map(|_| false).collect::<Vec<_>>();
    let mut v = (0..product.len()).map(|_| Vec::new()).collect::<Vec<_>>();
    for pat in pats {
        match pat {
            Pattern::Variable(_) | Pattern::Any => return true,
            Pattern::Product(prod) => {
                for (idx, p) in prod.iter().enumerate() {
                    v[idx].push(p.clone());
                }
            }
            _ => unreachable!(),
        }
    }
    for (idx, ty) in product.iter().enumerate() {
        ex[idx] = exhaustive(&v.remove(0), ty);
    }
    ex.into_iter().all(|b| b)
}

/// Invariants:
/// - `pats` are valid for the type `ty`
/// - `pats` are unique (no duplicates)
fn exhaustive(pats: &[Pattern], ty: &Type) -> bool {
    for p in pats {
        match p {
            Pattern::Any | Pattern::Variable(_) => return true,
            _ => {}
        }
    }

    match ty {
        // Type::Variant(v) => {},
        Type::Product(p) => exhaustive_product(pats, p),
        _ => false,
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
    #[test]
    fn product() {
        let ty = Type::Product(vec![Type::Bool, Type::Bool, Type::Nat]);
        let pat = Pattern::Product(vec![
            Pattern::Literal(Literal::Bool(true)),
            Pattern::Literal(Literal::Bool(true)),
            Pattern::Literal(Literal::Nat(10)),
        ]);
        let ctx = Context::default();
        assert!(ctx.pattern_type_eq(&pat, &ty));
    }

    #[test]
    #[should_panic]
    fn product_mistyped() {
        let ty = Type::Product(vec![Type::Bool, Type::Bool, Type::Bool]);
        let pat = Pattern::Product(vec![
            Pattern::Literal(Literal::Bool(true)),
            Pattern::Literal(Literal::Bool(true)),
            Pattern::Literal(Literal::Nat(10)),
        ]);
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

        let pat1 = Pattern::Constructor("A".into(), Box::new(Pattern::Any));
        let pat2 =
            Pattern::Constructor("A".into(), Box::new(Pattern::Literal(Literal::Bool(true))));
        let pat3 = Pattern::Constructor("B".into(), Box::new(Pattern::Literal(Literal::Nat(1))));

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

        let pat1 = Pattern::Constructor("A".into(), Box::new(Pattern::Any));
        let pat2 = Pattern::Constructor("B".into(), Box::new(Pattern::Any));
        let pat3 = Pattern::Constructor(
            "B".into(),
            Box::new(Pattern::Product(vec![
                Pattern::Any,
                Pattern::Variable("x".into()),
            ])),
        );
        let pat4 = Pattern::Constructor(
            "B".into(),
            Box::new(Pattern::Product(vec![
                Pattern::Literal(Literal::Nat(1)),
                Pattern::Variable("x".into()),
            ])),
        );
        let pat5 = Pattern::Constructor("A".into(), Box::new(Pattern::Literal(Literal::Nat(1))));

        let ctx = Context::default();
        assert!(ctx.pattern_type_eq(&pat1, &ty));
        assert!(ctx.pattern_type_eq(&pat2, &ty));
        assert!(ctx.pattern_type_eq(&pat3, &ty));
        assert!(ctx.pattern_type_eq(&pat4, &ty));
        assert!(!ctx.pattern_type_eq(&pat5, &ty));
    }
}
