pub mod ast;
pub mod lexer;
pub mod parser;
pub mod tokens;
pub mod visit;

pub mod pmc;

use ast::{Type, TypeKind, Variant};
use visit::TypeVisitor;

/// Determine if an AST datatype is recursive (type of a constructor references)
/// the datatype's defined name
struct RecursiveCon<'t> {
    name: &'t str,
    ty: &'t [Variant],
    cur_label: usize,
    recursive_labels: Vec<usize>,
}

/// We want to transform datatype 'a list = Nil | Cons of 'a * 'a list
/// to something resembling this eventually.
///
/// fn list_type() -> Type {
///     let inner = tyop!(
///         kind!(* => *),
///         tyop!(
///             kind!(*),
///             sum!(
///                 ("Nil", Type::Unit),
///                 (
///                     "Cons",
///                     record!(
///                         ("head", Type::Var(0)),
///                         ("tail", op_app!(Type::Var(1), Type::Var(0)))
///                     )
///                 )
///             )
///         )
///     );
///     Type::Recursive(Box::new(inner))
/// }

impl<'t> RecursiveCon<'t> {
    fn new(name: &'t str, ty: &'t [Variant]) -> RecursiveCon<'t> {
        RecursiveCon {
            name,
            ty,
            cur_label: 0,
            recursive_labels: Vec::new(),
        }
    }

    fn collect_recursive_labels(mut self) -> Vec<usize> {
        self.visit_sum(self.ty);
        self.recursive_labels
    }
}

impl<'t> TypeVisitor for RecursiveCon<'t> {
    fn visit_defined(&mut self, s: &str) {
        if self.name == s {
            self.recursive_labels.push(self.cur_label);
        }
    }

    fn visit_sum(&mut self, constructors: &[Variant]) {
        for (idx, v) in constructors.iter().enumerate() {
            self.cur_label = idx;
            if let Some(ty) = &v.ty {
                self.visit_ty(ty);
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn recursive_labels() {
        let mut p = parser::Parser::new("datatype 'a list = Nil | Cons of 'a * 'a list");
        let t = p.parse_decl().unwrap();

        let indices = match t.kind {
            ast::DeclKind::Datatype(_, name, ty) => {
                let vars = ty.kind.variants();
                RecursiveCon::new(&name, vars).collect_recursive_labels()
            }
            _ => panic!("Didn't parse as a datatype decl!"),
        };

        assert_eq!(indices, vec![1]);
    }
}
