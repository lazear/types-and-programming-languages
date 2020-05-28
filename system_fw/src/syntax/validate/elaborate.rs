use super::*;
use crate::diagnostics::Diagnostic;
use crate::hir::Decl as HDecl;
use crate::hir::Expr as HExpr;
use crate::hir::Type as HType;
use crate::hir::{self, DeBruijn, HirId};
use crate::stack::Stack;
use crate::syntax::pmc::InitMatrix;
use std::collections::{HashMap, HashSet};
use std::iter::IntoIterator;
use util::span::Span;

/// Validate that a [`Program`] is closed, e.g. it has no free
/// term or type variables. We traverse the program in execution order,
/// adding bindings for top-level declarations, and also keeping track of
/// bindings that occur in local scopes for de Bruijn index tracking
pub struct ElaborationContext<'s> {
    tyvars: Stack<&'s str>,
    tmvars: Stack<&'s str>,
    defined_values: HashMap<String, HirId>,
    defined_types: HashMap<String, HirId>,

    elaborated: HashMap<HirId, hir::Decl>,
    next_hir_id: HirId,
}

#[derive(Clone, Debug, PartialEq)]

pub enum ElabError {
    Undefined(String, util::span::Span),
    InvalidBinding(String, util::span::Span),
}

#[derive(Copy, Clone, PartialEq)]
enum Scope {
    Local,
    Global,
}

impl<'s> ElaborationContext<'s> {
    /// Keep track of the type variable stack, while executing the combinator
    /// function `f` on `self`. Any stack growth is popped off after `f`
    /// returns.
    fn with_tyvars<T, F: Fn(&mut ElaborationContext<'s>) -> T>(&mut self, f: F) -> T {
        let n = self.tyvars.len();
        let r = f(self);
        let to_pop = self.tyvars.len() - n;
        self.tyvars.popn(to_pop);
        r
    }

    /// Keep track of the term variable stack, while executing the combinator
    /// function `f` on `self`. Any stack growth is popped off after `f`
    /// returns.
    fn with_tmvars<T, F: Fn(&mut ElaborationContext<'s>) -> T>(&mut self, f: F) -> T {
        let n = self.tmvars.len();
        let r = f(self);
        let to_pop = self.tmvars.len() - n;
        self.tmvars.popn(to_pop);
        r
    }

    fn elab_kind(&self, k: &Kind) -> hir::Kind {
        match k {
            Kind::Star => hir::Kind::Star,
            Kind::Arrow(k1, k2) => {
                hir::Kind::Arrow(Box::new(self.elab_kind(k1)), Box::new(self.elab_kind(k2)))
            }
        }
    }

    fn allocate_hir_id(&mut self) -> HirId {
        let id = self.next_hir_id;
        self.next_hir_id = HirId(self.next_hir_id.0 + 1);
        id
    }

    fn define_value(&mut self, name: String, expr: hir::Expr) -> HirId {
        let id = self.allocate_hir_id();
        self.elaborated.insert(id, hir::Decl::Value(expr));
        self.defined_values.insert(name, id);
        id
    }

    fn define_type(&mut self, name: String, ty: hir::Type) -> HirId {
        let id = self.allocate_hir_id();
        self.elaborated.insert(id, hir::Decl::Type(ty));
        self.defined_types.insert(name, id);
        id
    }

    fn elab_constructor(
        &mut self,
        name: &'s str,
        tag: usize,
        tyvar_arity: usize,
        type_signature: Option<&hir::Type>,
        type_id: HirId,
    ) -> HirId {
        let expr = match type_signature {
            Some(ty) => hir::Expr::Abs(
                Box::new(ty.clone()),
                Box::new(hir::Expr::App(
                    Box::new(hir::Expr::Constr(type_id, tag)),
                    Box::new(hir::Expr::LocalVar(DeBruijn {
                        name: String::from("x"),
                        idx: 0,
                    })),
                )),
            ),
            None => hir::Expr::Constr(type_id, tag),
        };

        let expr = (0..tyvar_arity).fold(expr, |e, _| {
            hir::Expr::TyAbs(Box::new(hir::Kind::Star), Box::new(e))
        });

        self.define_value(name.into(), expr)
    }

    pub fn dump(&self) {
        println!("Current value bindings:");
        for (name, key) in &self.defined_values {
            println!(
                "[{:?}] {}: {:?}",
                key,
                name,
                self.elaborated.get(key).unwrap()
            );
        }
        println!("Current type bindings:");
        for (name, key) in &self.defined_types {
            println!(
                "[{:?}] {}: {:?}",
                key,
                name,
                self.elaborated.get(key).unwrap()
            );
        }
    }

    fn ty_lookup(&self, s: &str, scope: Scope) -> Option<hir::Type> {
        match scope {
            Scope::Local => self.tyvars.lookup(&s).map(|idx| {
                hir::Type::Var(DeBruijn {
                    idx,
                    name: s.into(),
                })
            }),
            Scope::Global => self
                .defined_types
                .get(s)
                .map(|idx| hir::Type::Defined(*idx)),
        }
    }

    fn expr_lookup(&self, s: &str, scope: Scope) -> Option<hir::Expr> {
        let ret = match scope {
            Scope::Local => self.tmvars.lookup(&s).map(|idx| {
                hir::Expr::LocalVar(DeBruijn {
                    idx,
                    name: s.into(),
                })
            }),
            Scope::Global => self
                .defined_values
                .get(s)
                .map(|idx| hir::Expr::ProgramVar(*idx)),
        };
        // We don't have a sigil for local term variables like we do for type
        // variables, so just attempt to lexically scope it first.
        // We don't need to do this for Scope::Global, because that is only
        // for constructors.... or do we need to do this for let bindings?
        // TODO ^^
        if ret.is_none() && scope == Scope::Local {
            return self.expr_lookup(s, Scope::Global);
        }
        ret
    }

    pub fn new() -> ElaborationContext<'s> {
        ElaborationContext {
            tmvars: Stack::default(),
            tyvars: Stack::default(),
            defined_values: HashMap::default(),
            defined_types: HashMap::default(),
            elaborated: HashMap::default(),
            next_hir_id: HirId(0),
        }
    }
}

impl<'s> ElaborationContext<'s> {
    fn elab_ty_row(&mut self, row: &'s Row) -> Result<hir::Row, ElabError> {
        Ok(hir::Row {
            label: row.label.clone(),
            ty: self.elab_type(&row.ty)?,
        })
    }

    fn elab_ty_inner(&mut self, tv: &'s str, ty: &'s Type) -> Result<hir::Type, ElabError> {
        self.with_tyvars(|f| {
            f.tyvars.push(tv);
            f.elab_type(ty)
        })
    }

    fn elab_type(&mut self, ty: &'s Type) -> Result<hir::Type, ElabError> {
        use TypeKind::*;
        match &ty.kind {
            Int => Ok(hir::Type::Int),
            Bool => Ok(hir::Type::Bool),
            Unit => Ok(hir::Type::Unit),
            Infer => Ok(hir::Type::Infer),
            Defined(s) => self
                .ty_lookup(s, Scope::Global)
                .ok_or_else(|| ElabError::Undefined(s.into(), ty.span)),
            Variable(s) => self
                .ty_lookup(s, Scope::Local)
                .ok_or_else(|| ElabError::Undefined(s.into(), ty.span)),
            Function(ty1, ty2) => Ok(hir::Type::Arrow(
                Box::new(self.elab_type(ty1)?),
                Box::new(self.elab_type(ty2)?),
            )),

            // Sum types can only be constructed through a DeclKind::Datatype
            // so it's okay to be unreachable!() here, as it would be a fatal
            // bug if we reached this path somehow.
            Sum(_) => unreachable!(),
            Product(tys) => Ok(hir::Type::Product(
                tys.iter()
                    .map(|t| self.elab_type(t))
                    .collect::<Result<_, _>>()?,
            )),
            Record(rows) => Ok(hir::Type::Record(
                rows.iter()
                    .map(|t| self.elab_ty_row(t))
                    .collect::<Result<_, _>>()?,
            )),
            Existential(s, k, ty) => Ok(hir::Type::Existential(
                Box::new(self.elab_kind(k)),
                Box::new(self.elab_ty_inner(s, ty)?),
            )),
            Universal(s, k, ty) => Ok(hir::Type::Universal(
                Box::new(self.elab_kind(k)),
                Box::new(self.elab_ty_inner(s, ty)?),
            )),
            Abstraction(s, k, ty) => Ok(hir::Type::Abstraction(
                Box::new(self.elab_kind(k)),
                Box::new(self.elab_ty_inner(s, ty)?),
            )),
            Application(ty1, ty2) => Ok(hir::Type::Application(
                Box::new(self.elab_type(ty1)?),
                Box::new(self.elab_type(ty2)?),
            )),
            Recursive(ty) => self
                .elab_type(ty)
                .map(|ty| hir::Type::Recursive(Box::new(ty))),
        }
    }
}

impl<'s> ElaborationContext<'s> {
    fn elab_let(&mut self, decls: &'s [Decl], expr: &'s Expr) -> Result<hir::Expr, ElabError> {
        unimplemented!()
    }

    fn elab_case(&mut self, expr: &'s Expr, arms: &'s [Arm]) -> Result<hir::Expr, ElabError> {
        unimplemented!()
    }

    fn elab_field(&mut self, field: &'s Field) -> Result<hir::Field, ElabError> {
        Ok(hir::Field {
            label: field.label.clone(),
            expr: self.elab_expr(&field.expr)?,
        })
    }

    fn elab_abs(&mut self, pat: &'s Pattern, body: &'s Expr) -> Result<hir::Expr, ElabError> {
        unimplemented!()
    }

    fn elab_expr(&mut self, expr: &'s Expr) -> Result<hir::Expr, ElabError> {
        use ExprKind::*;
        match &expr.kind {
            Unit => Ok(hir::Expr::Unit),
            Int(i) => Ok(hir::Expr::Int(*i)),
            Var(s) => self
                .expr_lookup(s, Scope::Local)
                .ok_or_else(|| ElabError::Undefined(s.into(), expr.span)),
            Constr(s) => self
                .expr_lookup(s, Scope::Global)
                .ok_or_else(|| ElabError::Undefined(s.into(), expr.span)),
            If(e1, e2, e3) => Ok(hir::Expr::If(
                Box::new(self.elab_expr(e1)?),
                Box::new(self.elab_expr(e2)?),
                Box::new(self.elab_expr(e3)?),
            )),
            Abs(pat, expr) => self.elab_abs(pat, expr),
            App(e1, e2) => Ok(hir::Expr::App(
                Box::new(self.elab_expr(e1)?),
                Box::new(self.elab_expr(e2)?),
            )),
            TyAbs(s, k, e) => self.with_tyvars(|f| {
                f.tyvars.push(s);
                let e = f.elab_expr(e)?;
                Ok(hir::Expr::TyAbs(Box::new(f.elab_kind(k)), Box::new(e)))
            }),
            TyApp(e, t) => Ok(hir::Expr::TyApp(
                Box::new(self.elab_expr(e)?),
                Box::new(self.elab_type(t)?),
            )),
            Record(fields) => fields
                .iter()
                .map(|e| self.elab_field(e))
                .collect::<Result<_, _>>()
                .map(hir::Expr::Record),
            Tuple(exprs) => exprs
                .iter()
                .map(|e| self.elab_expr(e))
                .collect::<Result<_, _>>()
                .map(hir::Expr::Tuple),
            Projection(e1, e2) => match &e2.kind {
                ExprKind::Var(label) => Ok(hir::Expr::RecordProj(
                    Box::new(self.elab_expr(e1)?),
                    label.clone(),
                )),
                ExprKind::Int(idx) => Ok(hir::Expr::TupleProj(Box::new(self.elab_expr(e1)?), *idx)),
                _ => Err(ElabError::InvalidBinding(
                    format!("attempt to project using {:?}", e2),
                    expr.span,
                )),
            },
            Case(e, arms) => self.elab_case(e, arms),
            Let(decls, e) => self.elab_let(decls, e),
        }
    }
}

impl<'s> ElaborationContext<'s> {
    fn elab_pattern(&mut self, pat: &'s Pattern) -> Result<hir::Pattern, ElabError> {
        match &pat.kind {
            PatKind::Any => Ok(hir::Pattern::Any),
            PatKind::Unit => Ok(hir::Pattern::Unit),
            PatKind::Literal(i) => Ok(hir::Pattern::Literal(*i)),
            PatKind::Variable(s) => Ok(hir::Pattern::Variable(s.clone())),
            PatKind::Product(sub) => sub
                .iter()
                .map(|p| self.elab_pattern(p))
                .collect::<Result<_, _>>()
                .map(hir::Pattern::Product),
            PatKind::Record(sub) => Ok(hir::Pattern::Record(sub.clone())),
            PatKind::Ascribe(pat, ty) => Ok(hir::Pattern::Ascribe(
                Box::new(self.elab_pattern(pat)?),
                Box::new(self.elab_type(ty)?),
            )),
            PatKind::Constructor(s) => self
                .defined_types
                .get(s.as_ref() as &str)
                .copied()
                .ok_or_else(|| ElabError::Undefined(s.clone(), pat.span))
                .map(hir::Pattern::Constructor),
            PatKind::Application(con, arg) => {
                let econ = self.elab_pattern(con)?;
                let earg = self.elab_pattern(arg)?;
                let id = match econ {
                    hir::Pattern::Constructor(id) => id,
                    _ => {
                        return Err(ElabError::InvalidBinding(
                            format!("cannot apply {:?} to non-constructor {:?}", arg, con),
                            pat.span,
                        ))
                    }
                };
                Ok(hir::Pattern::Application(id, Box::new(earg)))
            }
            PatKind::Literal(i) => Ok(hir::Pattern::Literal(*i)),
        }
    }
}

impl<'s> ElaborationContext<'s> {
    fn elab_decl_type(
        &mut self,
        tyvars: &'s [Type],
        name: &'s str,
        ty: &'s Type,
    ) -> Result<HirId, ElabError> {
        let ty = self.elab_type(ty)?;
        let ty = tyvars.iter().fold(ty, |ty, var| {
            hir::Type::Abstraction(Box::new(hir::Kind::Star), Box::new(ty))
        });
        Ok(self.define_type(name.into(), ty))
    }

    fn elab_decl_datatype(
        &mut self,
        tyvars: &'s [Type],
        name: &'s str,
        ty: &'s Type,
    ) -> Result<HirId, ElabError> {
        // Quickly collection all names that this type points to
        let mut coll = super::TyNameCollector::default();
        coll.visit_ty(&ty);
        let is_recur = coll.definitions.contains(name);

        // Insert first, so we can be recursive if we need to
        let id = self.allocate_hir_id();
        self.defined_types.insert(name.into(), id);

        // We just do all of this inside of the closure, rather than delegrating
        // to visit_sum, because we need access to both `tyvars` for generating
        // value-bindings for the constructors
        let ty = self.with_tyvars(|f| {
            f.tyvars.extend(tyvars.iter().map(|t| t.kind.as_tyvar()));

            let mut elab = Vec::new();
            for (idx, v) in ty.kind.variants().iter().enumerate() {
                let ty = v.ty.as_ref().map(|ty| f.elab_type(ty)).transpose()?;

                // Generate a function or constant value for the constructor
                f.elab_constructor(&v.label, idx, tyvars.len(), ty.as_ref(), id);
                elab.push(hir::Variant {
                    label: v.label.clone(),
                    ty,
                });
            }
            Ok(hir::Type::Sum(elab))
        })?;

        // We have the raw sum type, so now wrap it in type abstractions
        let ty = tyvars.iter().fold(ty, |ty, var| {
            hir::Type::Abstraction(Box::new(hir::Kind::Star), Box::new(ty))
        });
        let ty = if is_recur {
            hir::Type::Recursive(Box::new(ty))
        } else {
            ty
        };

        self.elaborated.insert(id, hir::Decl::Type(ty));
        Ok(id)
    }

    /// Caller is responsible for checking tmvar and tyvar stack growth
    /// Note: this function directly adds bindings to the global definition states
    fn deconstruct_pat_binding(
        &mut self,
        pat: hir::Pattern,
        expr: hir::Expr,
        span: util::span::Span,
    ) -> Result<HirId, ElabError> {
        use hir::Pattern::*;
        match pat {
            Any | Unit => Ok(self.define_value("_".into(), expr)),
            Variable(s) => Ok(self.define_value(s, expr)),
            Product(sub) => {
                let id = self.define_value("$anon_bind_tuple".into(), expr);
                let base = Box::new(hir::Expr::ProgramVar(id));
                for (idx, pat) in sub.into_iter().enumerate() {
                    self.deconstruct_pat_binding(
                        pat,
                        hir::Expr::TupleProj(base.clone(), idx),
                        span,
                    )?;
                }
                Ok(id)
            }
            Record(sub) => {
                let id = self.define_value("$anon_bind_record".into(), expr);
                let base = Box::new(hir::Expr::ProgramVar(id));

                for (idx, pat) in sub.into_iter().enumerate() {
                    self.define_value(pat, hir::Expr::TupleProj(base.clone(), idx));
                }
                Ok(id)
            }
            Ascribe(pat, _) => self.deconstruct_pat_binding(*pat, expr, span),
            Constructor(s) => Err(ElabError::InvalidBinding(
                format!("cannot bind constructor to a value!"),
                span,
            )),
            Application(con, arg) => {
                // // val Some (x, xy) = expr
                // // val tup = case expr of Some (x, xy) => (x, xy)
                // // val x = tup.0
                // // val y = tup.1

                // let ast_arm = Arm { pat: pat, expr: }
                unimplemented!()
            }
            Literal(_) => Err(ElabError::InvalidBinding(
                format!("cannot bind a literal pattern to a value!"),
                span,
            )),
        }
    }

    fn elab_decl_value(
        &mut self,
        tyvars: &'s [Type],
        pat: &'s Pattern,
        expr: &'s Expr,
    ) -> Result<HirId, ElabError> {
        self.with_tyvars(|f| {
            f.tyvars.extend(tyvars.iter().map(|t| t.kind.as_tyvar()));
            f.with_tmvars(|f| {
                let sp = pat.span;
                let pat = f.elab_pattern(pat)?;
                let ex = f.elab_expr(expr)?;
                f.deconstruct_pat_binding(pat, ex, sp)
            })
        })
    }

    fn elab_decl(&mut self, decl: &'s Decl) -> Result<HirId, ElabError> {
        match &decl.kind {
            DeclKind::Datatype(tyvars, name, ty) => self.elab_decl_datatype(tyvars, name, ty),
            DeclKind::Type(tyvars, name, ty) => self.elab_decl_type(tyvars, name, ty),
            DeclKind::Value(tyvars, pat, expr) => self.elab_decl_value(tyvars, pat, expr),
            _ => unimplemented!(),
        }
    }

    pub fn elab_program(&mut self, prog: &'s Program) -> Result<Vec<HirId>, ElabError> {
        let mut v = Vec::with_capacity(prog.decls.len());
        for d in &prog.decls {
            v.push(self.elab_decl(d)?);
            self.dump();
        }
        Ok(v)
    }
}
