use super::visit::TypeVisitor;
use std::collections::HashSet;

/// Collect type variables and references to defined names
#[derive(Default, Debug, Clone)]
pub struct TyNameCollector<'s> {
    pub tyvars: HashSet<&'s str>,
    pub definitions: HashSet<&'s str>,
}

impl<'s> TypeVisitor<'s> for TyNameCollector<'s> {
    fn visit_variable(&mut self, s: &'s str) {
        self.tyvars.insert(s);
    }

    fn visit_defined(&mut self, s: &'s str) {
        self.definitions.insert(s);
    }
}
