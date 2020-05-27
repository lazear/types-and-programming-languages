use super::ast::*;

pub trait DeclVisitor<'d> {
    fn visit_datatype(&mut self, tyvars: &'d [Type], name: &'d str, ty: &'d Type);
    fn visit_type(&mut self, tyvars: &'d [Type], name: &'d str, ty: &'d Type);
    fn visit_value(&mut self, tyvars: &'d [Type], pat: &'d Pattern, expr: &'d Expr);
    fn visit_function(&mut self, tyvars: &'d [Type], name: &'d str, arms: &'d [FnArm]);
    fn visit_toplevel_expr(&mut self, expr: &'d Expr);

    fn visit_decl(&mut self, decl: &'d Decl) {
        use DeclKind::*;
        match &decl.kind {
            Datatype(tyvars, name, ty) => self.visit_datatype(tyvars, name, ty),
            Type(tyvars, name, ty) => self.visit_type(tyvars, name, ty),
            Value(tyvars, pat, expr) => self.visit_value(tyvars, pat, expr),
            Function(tyvars, name, arms) => self.visit_function(tyvars, name, arms),
            Expr(e) => self.visit_toplevel_expr(e),
        }
    }

    fn visit_program(&mut self, prog: &'d Program) {
        for d in &prog.decls {
            self.visit_decl(d);
        }
    }
}
