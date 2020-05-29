use super::*;

use crate::hir::{self, Constructor, DeBruijn, HirId};
use crate::stack::Stack;
use crate::syntax::pmc::InitMatrix;
use std::collections::{HashMap, HashSet};
use std::iter::IntoIterator;
use util::span::Span;

/// Validate that a [`Program`] is closed, e.g. it has no free
/// term or type variables. We traverse the program in execution order,
/// adding bindings for top-level declarations, and also keeping track of
/// bindings that occur in local scopes for de Bruijn index tracking
#[derive(Default)]
pub struct ElaborationContext<'s> {
    tyvars: Stack<&'s str>,
    tmvars: Stack<&'s str>,

    // defined_values: HashMap<String, HirId>,
    // defined_types: HashMap<String, HirId>,
    namespaces: Vec<Namespace>,
    current: usize,

    constructors: HashMap<HirId, Constructor>,
    elaborated: HashMap<HirId, hir::Decl>,
    next_hir_id: HirId,
}

#[derive(Default)]
pub struct Namespace {
    id: usize,
    parent: Option<usize>,
    values: HashMap<String, HirId>,
    types: HashMap<String, HirId>,
}

#[derive(Clone, Debug, PartialEq)]

pub enum ElabError {
    UndefinedType(String, util::span::Span),
    UndefinedValue(String, util::span::Span),
    UndefinedConstr(String, util::span::Span),
    InvalidBinding(String, util::span::Span),
}

/// Housekeeping, namespace methods
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
        self.namespaces[self.current].values.insert(name, id);
        id
    }

    fn define_type(&mut self, name: String, ty: hir::Type) -> HirId {
        let id = self.allocate_hir_id();
        self.elaborated.insert(id, hir::Decl::Type(ty));
        self.namespaces[self.current].types.insert(name, id);
        id
    }

    pub fn dump(&self) {
        for n in &self.namespaces {
            println!("Current value bindings:");
            for (name, key) in &n.values {
                println!(
                    "\t[{:?}] {}: {:?}",
                    key,
                    name,
                    self.elaborated.get(key).unwrap()
                );
            }
            println!("Current type bindings:");
            for (name, key) in &n.types {
                println!(
                    "\t[{:?}] {}: {:?}",
                    key,
                    name,
                    self.elaborated.get(key).unwrap()
                );
            }
        }

        println!("Current constr bindings:");
        for (name, key) in &self.constructors {
            println!("{:?}", key,);
        }
    }

    fn lexical_value(&self, s: &str) -> Option<HirId> {
        let mut ptr = &self.namespaces[self.current];
        loop {
            match ptr.values.get(s) {
                Some(idx) => return Some(*idx),
                None => ptr = &self.namespaces[ptr.parent?],
            }
        }
    }

    fn debruijn_value(&self, s: &str) -> Option<hir::Expr> {
        self.tmvars.lookup(&s).map(|idx| {
            hir::Expr::LocalVar(DeBruijn {
                idx,
                name: s.into(),
            })
        })
    }

    fn lookup_value(&self, s: &str) -> Option<hir::Expr> {
        if let Some(db) = self.debruijn_value(s) {
            return Some(db);
        }
        self.lexical_value(s).map(hir::Expr::ProgramVar)
    }

    fn lexical_type(&self, s: &str) -> Option<HirId> {
        let mut ptr = &self.namespaces[self.current];
        loop {
            match ptr.types.get(s) {
                Some(idx) => return Some(*idx),
                None => ptr = &self.namespaces[ptr.parent?],
            }
        }
    }

    fn debruijn_type(&self, s: &str) -> Option<hir::Type> {
        self.tyvars.lookup(&s).map(|idx| {
            hir::Type::Var(DeBruijn {
                idx,
                name: s.into(),
            })
        })
    }

    pub fn new() -> Self {
        let mut ec = Self::default();
        let global_ns = Namespace::default();
        ec.namespaces.push(global_ns);
        ec
    }

    fn enter_namespace(&mut self) -> usize {
        let id = self.namespaces.len();
        self.namespaces.push(Namespace {
            id,
            parent: Some(self.current),
            types: HashMap::new(),
            values: HashMap::new(),
        });
        self.current = id;
        id
    }

    fn leave_namespace(&mut self) {
        match self.namespaces[self.current].parent {
            Some(id) => self.current = id,
            None => panic!("trying to leave global namespace!"),
        }
    }

    fn with_new_namespace<T, F: Fn(&mut ElaborationContext<'s>) -> T>(&mut self, f: F) -> T {
        self.enter_namespace();
        let t = f(self);
        self.leave_namespace();
        t
    }
}

/// Type elaboration
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
                .lexical_type(s)
                .map(hir::Type::Defined)
                .ok_or_else(|| ElabError::UndefinedType(s.into(), ty.span)),
            Variable(s) => self
                .debruijn_type(s)
                .ok_or_else(|| ElabError::UndefinedType(s.into(), ty.span)),
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

/// Expr elaboration
impl<'s> ElaborationContext<'s> {
    fn elab_let(&mut self, decls: &'s [Decl], expr: &'s Expr) -> Result<hir::Expr, ElabError> {
        self.with_new_namespace(|f| {
            for d in decls {
                f.elab_decl(d)?;
            }
            f.elab_expr(expr)
        })
    }

    fn elab_arm(&mut self, arm: &'s Arm) -> Result<hir::Arm, ElabError> {
        Ok(hir::Arm {
            pat: self.elab_pattern(&arm.pat, true)?,
            expr: self.elab_expr(&arm.expr)?,
        })
    }

    fn elab_case(&mut self, expr: &'s Expr, arms: &'s [Arm]) -> Result<hir::Expr, ElabError> {
        let ex = self.elab_expr(expr)?;
        let arms = arms
            .iter()
            .map(|a| self.elab_arm(a))
            .collect::<Result<_, _>>()?;
        Ok(hir::Expr::Case(Box::new(ex), arms))
    }

    fn elab_field(&mut self, field: &'s Field) -> Result<hir::Field, ElabError> {
        Ok(hir::Field {
            label: field.label.clone(),
            expr: self.elab_expr(&field.expr)?,
        })
    }

    /// We desugar to a case expression
    /// fn (Some x) => x + 1
    /// fn $x : Infer option => case $x of (Some x) => x + 1
    fn elab_abs(&mut self, pat: &'s Pattern, body: &'s Expr) -> Result<hir::Expr, ElabError> {
        // Wow we have a lot of bindings
        self.with_tmvars(|f| {
            let pat = f.elab_pattern(pat, true)?;
            let expr = f.elab_expr(body)?;
            let ty = f.naive_type_infer(&pat)?;
            let arm = hir::Arm { pat, expr };
            let dummy = hir::Expr::LocalVar(DeBruijn {
                name: "$anon".into(),
                idx: 0,
            });
            let case = hir::Expr::Case(Box::new(dummy), vec![arm]);
            Ok(hir::Expr::Abs(Box::new(ty), Box::new(case)))
        })
    }

    fn elab_expr(&mut self, expr: &'s Expr) -> Result<hir::Expr, ElabError> {
        use ExprKind::*;
        match &expr.kind {
            Unit => Ok(hir::Expr::Unit),
            Int(i) => Ok(hir::Expr::Int(*i)),
            Var(s) => self
                .lookup_value(s)
                .ok_or_else(|| ElabError::UndefinedValue(s.into(), expr.span)),
            Constr(s) => self
                .lexical_value(s)
                .map(|id| self.constructors.get(&id))
                .flatten()
                .map(|c| hir::Expr::Constr(c.type_id, c.tag))
                .ok_or_else(|| ElabError::UndefinedConstr(s.into(), expr.span)),
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

/// Pattern elaboration
impl<'s> ElaborationContext<'s> {
    fn naive_type_infer(&self, pat: &hir::Pattern) -> Result<hir::Type, ElabError> {
        use hir::Pattern::*;
        match pat {
            Any => Ok(hir::Type::Infer),
            Unit => Ok(hir::Type::Unit),
            Literal(_) => Ok(hir::Type::Int),
            Ascribe(_, ty) => Ok(*ty.clone()),
            Constructor(id) => {
                let con = self.constructors.get(&id).expect("internal error");
                Ok(hir::Type::Defined(con.type_id))
            }
            Product(pats) => pats
                .into_iter()
                .map(|p| self.naive_type_infer(p))
                .collect::<Result<_, _>>()
                .map(hir::Type::Product),

            // Maybe we should go back to sub pats...
            Record(s) => Ok(hir::Type::Record(
                s.into_iter()
                    .map(|s| hir::Row {
                        label: s.clone(),
                        ty: hir::Type::Infer,
                    })
                    .collect(),
            )),
            Application(id, arg) => {
                let con = self.constructors.get(&id).expect("internal error");
                let cty = hir::Type::Defined(con.type_id);
                self.naive_type_infer(arg)
                    .map(|ty| hir::Type::Application(Box::new(cty), Box::new(ty)))
            }
            Variable(_) => Ok(hir::Type::Infer),
        }
    }
    fn elab_pattern(&mut self, pat: &'s Pattern, bind: bool) -> Result<hir::Pattern, ElabError> {
        match &pat.kind {
            PatKind::Any => Ok(hir::Pattern::Any),
            PatKind::Unit => Ok(hir::Pattern::Unit),
            PatKind::Literal(i) => Ok(hir::Pattern::Literal(*i)),
            PatKind::Variable(s) => {
                if bind {
                    self.tmvars.push(s);
                }
                Ok(hir::Pattern::Variable(s.clone()))
            }
            PatKind::Product(sub) => sub
                .iter()
                .map(|p| self.elab_pattern(p, bind))
                .collect::<Result<_, _>>()
                .map(hir::Pattern::Product),
            PatKind::Record(sub) => Ok(hir::Pattern::Record(sub.clone())),
            PatKind::Ascribe(pat, ty) => Ok(hir::Pattern::Ascribe(
                Box::new(self.elab_pattern(pat, bind)?),
                Box::new(self.elab_type(ty)?),
            )),
            PatKind::Constructor(s) => self
                .lexical_value(s)
                .ok_or_else(|| ElabError::UndefinedConstr(s.clone(), pat.span))
                .map(hir::Pattern::Constructor),
            PatKind::Application(con, arg) => {
                let econ = self.elab_pattern(con, bind)?;
                let earg = self.elab_pattern(arg, bind)?;
                let id = match econ {
                    hir::Pattern::Constructor(id) => {
                        let con_info = self.constructors.get(&id).unwrap();
                        if !con_info.arity {
                            return Err(ElabError::InvalidBinding(
                                format!("constructor {} doesn't accept arguments!", con_info.name),
                                pat.span,
                            ));
                        }
                        id
                    }
                    _ => {
                        return Err(ElabError::InvalidBinding(
                            format!("cannot apply {:?} to non-constructor {:?}", arg, con),
                            pat.span,
                        ))
                    }
                };
                Ok(hir::Pattern::Application(id, Box::new(earg)))
            }
        }
    }
}

/// Decl elaboration
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

        let arity = type_signature.is_some();

        let con_id = self.define_value(name.into(), expr);
        self.constructors.insert(
            con_id,
            Constructor {
                name: name.into(),
                type_id,
                con_id,
                tag,
                arity,
            },
        );
        con_id
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
        self.namespaces[self.current].types.insert(name.into(), id);

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
            Any | Unit => Ok(self.define_value(String::default(), expr)),
            Variable(s) => Ok(self.define_value(s, expr)),
            Product(sub) => {
                // No need for extra redirection
                let id = match expr {
                    hir::Expr::ProgramVar(id) => id,
                    _ => self.define_value(String::default(), expr),
                };

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
                let id = match expr {
                    hir::Expr::ProgramVar(id) => id,
                    _ => self.define_value(String::default(), expr),
                };
                let base = Box::new(hir::Expr::ProgramVar(id));

                for (idx, pat) in sub.into_iter().enumerate() {
                    self.define_value(pat, hir::Expr::TupleProj(base.clone(), idx));
                }
                Ok(id)
            }
            Ascribe(pat, _) => self.deconstruct_pat_binding(*pat, expr, span),
            Constructor(_) => Err(ElabError::InvalidBinding(
                format!("cannot bind constructor to a value!"),
                span,
            )),
            Application(con, arg) => {
                //      con  arg     expr
                // val Some (x, y) = Some (10, 9)
                // val Some (x, y) = funct 10
                //     $anon = func 10
                //      case $anon of

                let con_info = self.constructors.get(&con).unwrap();
                let e = hir::Expr::App(
                    Box::new(hir::Expr::Deconstr(con_info.type_id, con_info.tag)),
                    Box::new(expr),
                );

                let id = self
                    .with_new_namespace(|f| f.define_value("$anon_bind_decon".into(), e.clone()));
                let e = hir::Expr::ProgramVar(id);
                self.deconstruct_pat_binding(*arg, e, span)
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
                let pat = f.elab_pattern(pat, false)?;
                let ex = f.elab_expr(expr)?;
                f.deconstruct_pat_binding(pat, ex, sp)
            })
        })
    }

    fn build_pat_matrix(&mut self, arms: &'s [FnArm]) -> Result<PatternMatrix, ElabError> {
        let rows = arms.len();
        let mut pats: Vec<Vec<hir::Pattern>> = Vec::with_capacity(rows);
        let mut exprs = Vec::with_capacity(rows);

        let mut cols = 0;
        for arm in arms {
            cols = cols.max(arm.pats.len());
            pats.push(
                arm.pats
                    .iter()
                    .map(|p| self.elab_pattern(p, true))
                    .collect::<Result<_, _>>()?,
            );
            exprs.push(self.elab_expr(&arm.expr)?);
        }

        for r in pats.iter_mut() {
            if r.len() < cols {
                r.extend(std::iter::repeat(hir::Pattern::Any).take(cols - r.len()));
            }
        }

        Ok(PatternMatrix {
            pats,
            exprs,
            rows,
            cols,
        })
    }

    fn infer_type_matrix(&self, mat: &PatternMatrix) -> Result<Vec<HashSet<hir::Type>>, ElabError> {
        let mut cols: Vec<HashSet<hir::Type>> = (0..mat.cols).map(|_| HashSet::default()).collect();

        for i in 0..mat.cols {
            for j in 0..mat.rows {
                let ty = self.naive_type_infer(&mat.pats[j][i])?;
                cols[i].insert(ty);
            }
        }

        Ok(cols)
    }

    fn elab_decl_fun(
        &mut self,
        tyvars: &'s [Type],
        name: &'s str,
        arms: &'s [FnArm],
    ) -> Result<HirId, ElabError> {
        self.with_tyvars(|f| {
            f.tyvars.extend(tyvars.iter().map(|t| t.kind.as_tyvar()));
            f.with_tmvars(|f| {
                let matrix = f.build_pat_matrix(arms)?;
                let tys = f.infer_type_matrix(&matrix)?;
                dbg!(&matrix);
                dbg!(&tys);

                Ok(HirId(0))
            })
        })
    }

    fn elab_decl_expr(&mut self, expr: &'s Expr) -> Result<HirId, ElabError> {
        let e = self.elab_expr(expr)?;
        Ok(self.define_value(String::default(), e))
    }

    fn elab_decl(&mut self, decl: &'s Decl) -> Result<HirId, ElabError> {
        match &decl.kind {
            DeclKind::Datatype(tyvars, name, ty) => self.elab_decl_datatype(tyvars, name, ty),
            DeclKind::Type(tyvars, name, ty) => self.elab_decl_type(tyvars, name, ty),
            DeclKind::Value(tyvars, pat, expr) => self.elab_decl_value(tyvars, pat, expr),
            DeclKind::And(d1, d2) => unimplemented!(),
            DeclKind::Function(tyvars, name, arms) => self.elab_decl_fun(tyvars, name, arms),
            DeclKind::Expr(e) => self.elab_decl_expr(e),
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

#[derive(Debug, Clone)]
pub struct PatternMatrix {
    pats: Vec<Vec<hir::Pattern>>,
    exprs: Vec<hir::Expr>,
    rows: usize,
    cols: usize,
}
