use super::ast::*;

pub trait MutExprVisitor<'e>: Sized {
    fn visit_var(&mut self, s: &'e mut str) {}
    fn visit_constr(&mut self, c: &'e mut str) {}
    fn visit_if(&mut self, e1: &'e mut Expr, e2: &'e mut Expr, e3: &'e mut Expr) {
        self.visit_expr(e1);
        self.visit_expr(e2);
        self.visit_expr(e3);
    }
    fn visit_abs(&mut self, pat: &'e mut Pattern, body: &'e mut Expr) {
        self.visit_expr(body);
    }
    fn visit_app(&mut self, e1: &'e mut Expr, e2: &'e mut Expr) {
        self.visit_expr(e1);
        self.visit_expr(e2);
    }
    fn visit_tyabs(&mut self, k: &'e mut Kind, e: &'e mut Expr) {
        self.visit_expr(e);
    }
    fn visit_tyapp(&mut self, e: &'e mut Expr, ty: &'e mut Type) {
        self.visit_expr(e);
    }
    fn visit_record(&mut self, fields: &'e mut [Field]) {
        for f in fields {
            self.visit_expr(&mut f.expr);
        }
    }
    fn visit_tuple(&mut self, exprs: &'e mut [Expr]) {
        for e in exprs {
            self.visit_expr(e);
        }
    }
    fn visit_projection(&mut self, e1: &'e mut Expr, e2: &'e mut Expr) {
        self.visit_expr(e1);
        self.visit_expr(e2);
    }
    fn visit_case(&mut self, e: &'e mut Expr, arms: &'e mut [Arm]) {
        self.visit_expr(e);
        for arm in arms {
            self.visit_expr(&mut arm.expr);
        }
    }
    fn visit_let(&mut self, decls: &mut [Decl], e: &'e mut Expr) {
        self.visit_expr(e);
    }
    fn visit_expr(&mut self, expr: &'e mut Expr) {
        self.walk_expr(expr);
    }

    fn walk_expr(&mut self, expr: &'e mut Expr) {
        use ExprKind::*;
        match &mut expr.kind {
            Unit => {}
            Int(_) => {}
            Var(s) => self.visit_var(s),
            Constr(c) => self.visit_constr(c),
            If(e1, e2, e3) => self.visit_if(e1, e2, e3),
            Abs(pat, expr) => self.visit_abs(pat, expr),
            App(e1, e2) => self.visit_app(e1, e2),
            TyAbs(k, e) => self.visit_tyabs(k, e),
            TyApp(e, t) => self.visit_tyapp(e, t),
            Record(fields) => self.visit_record(fields),
            Tuple(exprs) => self.visit_tuple(exprs),
            Projection(e1, e2) => self.visit_projection(e1, e2),
            Case(e, arms) => self.visit_case(e, arms),
            Let(decls, e) => self.visit_let(decls, e),
        }
    }
}

pub trait ExprVisitor<'e>: Sized {
    fn visit_var(&mut self, s: &'e str) {}
    fn visit_constr(&mut self, c: &'e str) {}
    fn visit_if(&mut self, e1: &'e Expr, e2: &'e Expr, e3: &'e Expr) {
        self.visit_expr(e1);
        self.visit_expr(e2);
        self.visit_expr(e3);
    }
    fn visit_abs(&mut self, pat: &'e Pattern, body: &'e Expr) {
        self.visit_expr(body);
    }
    fn visit_app(&mut self, e1: &'e Expr, e2: &'e Expr) {
        self.visit_expr(e1);
        self.visit_expr(e2);
    }
    fn visit_tyabs(&mut self, k: &'e Kind, e: &'e Expr) {
        self.visit_expr(e);
    }
    fn visit_tyapp(&mut self, e: &'e Expr, ty: &'e Type) {
        self.visit_expr(e);
    }
    fn visit_record(&mut self, fields: &'e [Field]) {
        for f in fields {
            self.visit_expr(&f.expr);
        }
    }
    fn visit_tuple(&mut self, exprs: &'e [Expr]) {
        for e in exprs {
            self.visit_expr(e);
        }
    }
    fn visit_projection(&mut self, e1: &'e Expr, e2: &'e Expr) {
        self.visit_expr(e1);
        self.visit_expr(e2);
    }
    fn visit_case(&mut self, e: &'e Expr, arms: &'e [Arm]) {
        self.visit_expr(e);
        for arm in arms {
            self.visit_expr(&arm.expr);
        }
    }
    fn visit_let(&mut self, decls: &'e [Decl], e: &'e Expr) {
        self.visit_expr(e);
    }
    fn visit_expr(&mut self, expr: &'e Expr) {
        self.walk_expr(expr);
    }

    fn walk_expr(&mut self, expr: &'e Expr) {
        use ExprKind::*;
        match &expr.kind {
            Unit => {}
            Int(_) => {}
            Var(s) => self.visit_var(s),
            Constr(c) => self.visit_constr(c),
            If(e1, e2, e3) => self.visit_if(e1, e2, e3),
            Abs(pat, expr) => self.visit_abs(pat, expr),
            App(e1, e2) => self.visit_app(e1, e2),
            TyAbs(k, e) => self.visit_tyabs(k, e),
            TyApp(e, t) => self.visit_tyapp(e, t),
            Record(fields) => self.visit_record(fields),
            Tuple(exprs) => self.visit_tuple(exprs),
            Projection(e1, e2) => self.visit_projection(e1, e2),
            Case(e, arms) => self.visit_case(e, arms),
            Let(decls, e) => self.visit_let(decls, e),
        }
    }
}
