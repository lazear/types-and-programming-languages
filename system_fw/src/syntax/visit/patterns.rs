use super::ast::{PatKind, Pattern, Type};

pub trait MutPatVisitor<'p>: Sized {
    fn visit_constructor(&mut self, s: &'p mut str) {}
    fn visit_variable(&mut self, s: &'p mut str) {}
    fn visit_pattern(&mut self, pat: &'p mut Pattern) {
        self.walk_pattern(pat);
    }

    fn visit_ascribe(&mut self, pat: &'p mut Pattern, ty: &'p mut Type) {
        self.visit_pattern(pat);
    }

    fn visit_product_pat(&mut self, pats: &'p mut [Pattern]) {
        for pat in pats {
            self.visit_pattern(pat);
        }
    }

    fn visit_record(&mut self, pats: &'p mut [Pattern]) {
        for pat in pats {
            self.visit_pattern(pat);
        }
    }

    fn walk_pattern(&mut self, pat: &'p mut Pattern) {
        use PatKind::*;
        match &mut pat.kind {
            Any => {}
            Unit => {}
            Literal(_) => {}
            Ascribe(pat, ty) => self.visit_ascribe(pat, ty),
            Variable(s) => self.visit_variable(s),
            Constructor(c) => self.visit_constructor(c),
            Product(p) => self.visit_product_pat(p),
            Record(p) => self.visit_record(p),
            Application(c, pat) => {
                self.visit_pattern(c);
                self.visit_pattern(pat);
            }
        }
    }
}

pub trait PatVisitor<'p>: Sized {
    fn visit_constructor(&mut self, s: &'p str) {}
    fn visit_variable(&mut self, s: &'p str) {}
    fn visit_pattern(&mut self, pat: &'p Pattern) {
        self.walk_pattern(pat);
    }

    fn visit_ascribe(&mut self, pat: &'p Pattern, ty: &'p Type) {
        self.visit_pattern(pat);
    }

    fn visit_product_pat(&mut self, pats: &'p [Pattern]) {
        for pat in pats {
            self.visit_pattern(pat);
        }
    }

    fn visit_record(&mut self, pats: &'p [Pattern]) {
        for pat in pats {
            self.visit_pattern(pat);
        }
    }

    fn walk_pattern(&mut self, pat: &'p Pattern) {
        use PatKind::*;
        match &pat.kind {
            Any => {}
            Unit => {}
            Literal(_) => {}
            Ascribe(pat, ty) => self.visit_ascribe(pat, ty),
            Variable(s) => self.visit_variable(s),
            Constructor(c) => self.visit_constructor(c),
            Product(p) => self.visit_product_pat(p),
            Record(p) => self.visit_record(p),
            Application(c, pat) => {
                self.visit_pattern(c);
                self.visit_pattern(pat);
            }
        }
    }
}

impl<'p, T> PatVisitor<'p> for T where T: MutPatVisitor<'p> {}
