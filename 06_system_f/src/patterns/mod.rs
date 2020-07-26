use crate::terms::{Kind, Literal, Term};
use crate::types::{variant_field, Type};
use crate::visit::PatternVisitor;
use util::span::Span;

/// Patterns for case and let expressions
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Hash)]
pub enum Pattern {
    /// Wildcard pattern, this always matches
    Any,
    /// Constant pattern
    Literal(Literal),
    /// Variable binding pattern, this always matches
    Variable(String),
    /// Tuple of pattern bindings
    Product(Vec<Pattern>),
    /// Algebraic datatype constructor, along with binding pattern
    Constructor(String, Box<Pattern>),
}

#[derive(Clone, Debug, Default)]
pub struct PatVarStack {
    pub inner: Vec<String>,
}

impl PatVarStack {
    pub fn collect(pat: &mut Pattern) -> Vec<String> {
        let mut p = Self::default();
        p.visit_pattern(pat);
        p.inner
    }
}

impl PatternVisitor for PatVarStack {
    fn visit_variable(&mut self, var: &String) {
        self.inner.push(var.clone());
    }
}

/// Visitor that simply counts the number of binders (variables) within a
/// pattern
pub struct PatternCount(usize);

impl PatternCount {
    pub fn collect(pat: &mut Pattern) -> usize {
        let mut p = PatternCount(0);
        p.visit_pattern(pat);
        p.0
    }
}

impl PatternVisitor for PatternCount {
    fn visit_variable(&mut self, var: &String) {
        self.0 += 1;
    }
}

impl Pattern {
    /// Does this pattern match the given [`Term`]?
    pub fn matches(&self, term: &Term) -> bool {
        match self {
            Pattern::Any => return true,
            Pattern::Variable(_) => return true,
            Pattern::Literal(l) => {
                if let Kind::Lit(l2) = &term.kind {
                    return l == l2;
                }
            }
            Pattern::Product(vec) => {
                if let Kind::Product(terms) = &term.kind {
                    return vec.iter().zip(terms).all(|(p, t)| p.matches(t));
                }
            }
            Pattern::Constructor(label, inner) => {
                if let Kind::Injection(label_, tm, _) = &term.kind {
                    if label == label_ {
                        return inner.matches(&tm);
                    }
                }
            }
        }
        false
    }
}

/// Helper struct to traverse a [`Pattern`] and bind variables
/// to the typing context as needed.
///
/// It is the caller's responsibiliy to track stack growth and pop off
/// types after calling this function
pub struct PatTyStack<'ty> {
    pub ty: &'ty Type,
    pub inner: Vec<&'ty Type>,
}

impl<'ty> PatTyStack<'ty> {
    pub fn collect(ty: &'ty Type, pat: &Pattern) -> Vec<&'ty Type> {
        let mut p = PatTyStack {
            ty,
            inner: Vec::with_capacity(16),
        };
        p.visit_pattern(pat);
        p.inner
    }
}

impl<'ty> PatternVisitor for PatTyStack<'_> {
    fn visit_product(&mut self, pats: &Vec<Pattern>) {
        if let Type::Product(tys) = self.ty {
            let ty = self.ty;
            for (ty, pat) in tys.iter().zip(pats.iter()) {
                self.ty = ty;
                self.visit_pattern(pat);
            }
            self.ty = ty;
        }
    }

    fn visit_constructor(&mut self, label: &String, pat: &Pattern) {
        if let Type::Variant(vs) = self.ty {
            let ty = self.ty;
            self.ty = variant_field(&vs, label, Span::zero()).unwrap();
            self.visit_pattern(pat);
            self.ty = ty;
        }
    }

    fn visit_pattern(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Any | Pattern::Literal(_) => {}
            Pattern::Variable(_) => self.inner.push(self.ty),
            Pattern::Constructor(label, pat) => self.visit_constructor(label, pat),
            Pattern::Product(pats) => self.visit_product(pats),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn pattern_count() {
        let mut pat = Pattern::Variable(String::new());
        assert_eq!(PatternCount::collect(&mut pat), 1);
    }

    #[test]
    fn pattern_ty_stack() {
        let mut pat = Pattern::Variable(String::new());
        let ty = Type::Nat;
        assert_eq!(PatTyStack::collect(&ty, &mut pat), vec![&ty]);
    }

    #[test]
    fn pattern_var_stack() {
        let mut pat = Pattern::Variable("x".into());
        assert_eq!(PatVarStack::collect(&mut pat), vec![String::from("x")]);
    }
}
