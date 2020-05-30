//! Visitor traits for [`Pattern`], [`Term`], and [`Type`] objects
use crate::patterns::Pattern;
use crate::terms::{Arm, Kind, Literal, Primitive, Term};
use crate::types::{Type, Variant};
use util::span::Span;

pub trait MutTypeVisitor: Sized {
    fn visit_var(&mut self, var: &mut usize) {}
    fn visit_alias(&mut self, alias: &mut String) {}

    fn visit_arrow(&mut self, ty1: &mut Type, ty2: &mut Type) {
        self.visit(ty1);
        self.visit(ty2);
    }

    fn visit_universal(&mut self, inner: &mut Type) {
        self.visit(inner);
    }

    fn visit_existential(&mut self, inner: &mut Type) {
        self.visit(inner);
    }

    fn visit_variant(&mut self, variant: &mut Vec<Variant>) {
        for v in variant {
            self.visit(&mut v.ty);
        }
    }

    fn visit_product(&mut self, product: &mut Vec<Type>) {
        for v in product {
            self.visit(v);
        }
    }

    fn visit_rec(&mut self, ty: &mut Type) {
        self.visit(ty);
    }

    fn visit(&mut self, ty: &mut Type) {
        match ty {
            Type::Unit | Type::Bool | Type::Nat => {}
            Type::Var(v) => self.visit_var(v),
            Type::Variant(v) => self.visit_variant(v),
            Type::Product(v) => self.visit_product(v),
            Type::Alias(s) => self.visit_alias(s),
            Type::Arrow(ty1, ty2) => self.visit_arrow(ty1, ty2),
            Type::Universal(ty) => self.visit_universal(ty),
            Type::Existential(ty) => self.visit_existential(ty),
            Type::Rec(ty) => self.visit_rec(ty),
        }
    }
}

pub trait MutTermVisitor: Sized {
    fn visit_lit(&mut self, sp: &mut Span, lit: &mut Literal) {}
    fn visit_var(&mut self, sp: &mut Span, var: &mut usize) {}

    fn visit_abs(&mut self, sp: &mut Span, ty: &mut Type, term: &mut Term) {
        self.visit(term);
    }

    fn visit_app(&mut self, sp: &mut Span, t1: &mut Term, t2: &mut Term) {
        self.visit(t1);
        self.visit(t2);
    }

    fn visit_let(&mut self, sp: &mut Span, pat: &mut Pattern, t1: &mut Term, t2: &mut Term) {
        self.visit(t1);
        self.visit(t2);
    }

    fn visit_tyabs(&mut self, sp: &mut Span, term: &mut Term) {
        self.visit(term);
    }

    fn visit_tyapp(&mut self, sp: &mut Span, term: &mut Term, ty: &mut Type) {
        self.visit(term);
    }

    fn visit_primitive(&mut self, sp: &mut Span, prim: &mut Primitive) {}
    fn visit_injection(&mut self, sp: &mut Span, label: &mut String, term: &mut Term, ty: &mut Type) {
        self.visit(term);
    }

    fn visit_case(&mut self, sp: &mut Span, term: &mut Term, arms: &mut Vec<Arm>) {
        self.visit(term);
        for arm in arms {
            self.visit(&mut arm.term);
        }
    }

    fn visit_product(&mut self, sp: &mut Span, product: &mut Vec<Term>) {
        for t in product {
            self.visit(t);
        }
    }

    fn visit_projection(&mut self, sp: &mut Span, term: &mut Term, index: &mut usize) {
        self.visit(term);
    }

    fn visit_fold(&mut self, sp: &mut Span, ty: &mut Type, term: &mut Term) {
        self.visit(term);
    }
    fn visit_unfold(&mut self, sp: &mut Span, ty: &mut Type, term: &mut Term) {
        self.visit(term);
    }

    fn visit_pack(&mut self, sp: &mut Span, witness: &mut Type, evidence: &mut Term, signature: &mut Type) {
        self.visit(evidence);
    }

    fn visit_unpack(&mut self, sp: &mut Span, package: &mut Term, term: &mut Term) {
        self.visit(package);
        self.visit(term);
    }

    fn visit(&mut self, term: &mut Term) {
        self.walk(term);
    }

    fn walk(&mut self, term: &mut Term) {
        let sp = &mut term.span;
        match &mut term.kind {
            Kind::Lit(l) => self.visit_lit(sp, l),
            Kind::Var(v) => self.visit_var(sp, v),
            Kind::Abs(ty, term) => self.visit_abs(sp, ty, term),
            Kind::App(t1, t2) => self.visit_app(sp, t1, t2),
            // Do we need a separate branch?
            Kind::Fix(term) => self.visit(term),
            Kind::Primitive(p) => self.visit_primitive(sp, p),
            Kind::Injection(label, tm, ty) => self.visit_injection(sp, label, tm, ty),
            Kind::Projection(term, idx) => self.visit_projection(sp, term, idx),
            Kind::Product(terms) => self.visit_product(sp, terms),
            Kind::Case(term, arms) => self.visit_case(sp, term, arms),
            Kind::Let(pat, t1, t2) => self.visit_let(sp, pat, t1, t2),
            Kind::TyAbs(term) => self.visit_tyabs(sp, term),
            Kind::TyApp(term, ty) => self.visit_tyapp(sp, term, ty),
            Kind::Fold(ty, term) => self.visit_fold(sp, ty, term),
            Kind::Unfold(ty, term) => self.visit_unfold(sp, ty, term),
            Kind::Pack(wit, term, sig) => self.visit_pack(sp, wit, term, sig),
            Kind::Unpack(package, term) => self.visit_unpack(sp, package, term),
        }
    }
}

pub trait PatternVisitor: Sized {
    fn visit_literal(&mut self, lit: &Literal) {}
    fn visit_variable(&mut self, var: &String) {}
    fn visit_product(&mut self, pats: &Vec<Pattern>) {
        for p in pats {
            self.visit_pattern(p);
        }
    }

    fn visit_constructor(&mut self, label: &String, pat: &Pattern) {
        self.visit_pattern(pat);
    }

    fn visit_pattern(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Any => {}
            Pattern::Constructor(label, pat) => self.visit_constructor(label, pat),
            Pattern::Product(pat) => self.visit_product(pat),
            Pattern::Literal(lit) => self.visit_literal(lit),
            Pattern::Variable(var) => self.visit_variable(var),
        }
    }
}
