use super::ast::{PatKind, Pattern, Type};

pub trait MutPatVisitor<'p>: Sized {
    fn visit_constructor(&mut self, s: &mut str) {}
    fn visit_variable(&mut self, s: &mut str) {}
    fn visit_pattern(&mut self, pat: &mut Pattern) {
        self.walk_pattern(pat);
    }

    fn visit_ascribe(&mut self, pat: &mut Pattern, ty: &mut Type) {
        self.visit_pattern(pat);
    }

    fn walk_pattern(&mut self, pat: &mut Pattern) {
        use PatKind::*;
        match &mut pat.kind {
            Any => {}
            Unit => {}
            Literal(_) => {}
            Ascribe(pat, ty) => self.visit_ascribe(pat, ty),
            Variable(s) => self.visit_variable(s),
            Constructor(c) => self.visit_constructor(c),
            Product(p) | Record(p) => {
                for pat in p {
                    self.visit_pattern(pat);
                }
            }
            Application(c, pat) => {
                self.visit_pattern(c);
                self.visit_pattern(pat);
            }
        }
    }
}

pub trait PatVisitor<'p>: Sized {
    fn visit_constructor(&mut self, s: &str) {}
    fn visit_variable(&mut self, s: &str) {}
    fn visit_pattern(&mut self, pat: &Pattern) {
        self.walk_pattern(pat);
    }

    fn visit_ascribe(&mut self, pat: &Pattern, ty: &Type) {
        self.visit_pattern(pat);
    }

    fn walk_pattern(&mut self, pat: &Pattern) {
        use PatKind::*;
        match &pat.kind {
            Any => {}
            Unit => {}
            Literal(_) => {}
            Ascribe(pat, ty) => self.visit_ascribe(pat, ty),
            Variable(s) => self.visit_variable(s),
            Constructor(c) => self.visit_constructor(c),
            Product(p) | Record(p) => {
                for pat in p {
                    self.visit_pattern(pat);
                }
            }
            Application(c, pat) => {
                self.visit_pattern(c);
                self.visit_pattern(pat);
            }
        }
    }
}

impl<'p, T> PatVisitor<'p> for T where T: MutPatVisitor<'p> {}
