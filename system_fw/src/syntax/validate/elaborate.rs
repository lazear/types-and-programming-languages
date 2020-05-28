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
    defined_values: HashMap<&'s str, HirId>,
    defined_types: HashMap<&'s str, HirId>,

    elaborated: HashMap<HirId, hir::Decl>,
    next_hir_id: HirId,
}

#[derive(Clone, Debug, PartialEq)]

pub enum ElabError {
    Undefined(String, util::span::Span),
}

#[derive(Copy, Clone)]
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
    fn with_tmvars<F: Fn(&mut ElaborationContext<'s>)>(&mut self, f: F) {
        let n = self.tmvars.len();
        f(self);
        let to_pop = self.tmvars.len() - n;
        self.tmvars.popn(to_pop);
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

    fn define_value(&mut self, name: &'s str, expr: hir::Expr) -> HirId {
        let id = self.allocate_hir_id();
        self.elaborated.insert(id, hir::Decl::Value(expr));
        self.defined_values.insert(name, id);
        id
    }

    fn define_type(&mut self, name: &'s str, ty: hir::Type) -> HirId {
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

        self.define_value(name, expr)
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
                .get(&s)
                .map(|idx| hir::Type::Defined(*idx)),
        }
    }

    fn expr_lookup(&self, s: &str, scope: Scope) -> Option<hir::Expr> {
        match scope {
            Scope::Local => self.tmvars.lookup(&s).map(|idx| {
                hir::Expr::LocalVar(DeBruijn {
                    idx,
                    name: s.into(),
                })
            }),
            Scope::Global => self
                .defined_values
                .get(&s)
                .map(|idx| hir::Expr::ProgramVar(*idx)),
        }
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
            ty: self.elaborate_type(&row.ty)?,
        })
    }

    fn elab_ty_inner(&mut self, tv: &'s str, ty: &'s Type) -> Result<hir::Type, ElabError> {
        self.with_tyvars(|f| {
            f.tyvars.push(tv);
            f.elaborate_type(ty)
        })
    }

    fn elaborate_type(&mut self, ty: &'s Type) -> Result<hir::Type, ElabError> {
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
                Box::new(self.elaborate_type(ty1)?),
                Box::new(self.elaborate_type(ty2)?),
            )),

            // Sum types can only be constructed through a DeclKind::Datatype
            // so it's okay to be unreachable!() here, as it would be a fatal
            // bug if we reached this path somehow.
            Sum(_) => unreachable!(),
            Product(tys) => Ok(hir::Type::Product(
                tys.iter()
                    .map(|t| self.elaborate_type(t))
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
                Box::new(self.elaborate_type(ty1)?),
                Box::new(self.elaborate_type(ty2)?),
            )),
            Recursive(ty) => self
                .elaborate_type(ty)
                .map(|ty| hir::Type::Recursive(Box::new(ty))),
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
        let ty = self.elaborate_type(ty)?;
        let ty = tyvars.iter().fold(ty, |ty, var| {
            hir::Type::Abstraction(Box::new(hir::Kind::Star), Box::new(ty))
        });
        let id = self.define_type(name, ty);
        self.dump();
        Ok(id)
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
        self.defined_types.insert(name, id);

        // We just do all of this inside of the closure, rather than delegrating
        // to visit_sum, because we need access to both `tyvars` for generating
        // value-bindings for the constructors
        let ty = self.with_tyvars(|f| {
            f.tyvars.extend(tyvars.iter().map(|t| t.kind.as_tyvar()));

            let mut elab = Vec::new();
            for (idx, v) in ty.kind.variants().iter().enumerate() {
                let ty = v.ty.as_ref().map(|ty| f.elaborate_type(ty)).transpose()?;

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
        self.dump();
        Ok(id)
    }

    fn elab_decl(&mut self, decl: &'s Decl) -> Result<HirId, ElabError> {
        match &decl.kind {
            DeclKind::Datatype(tv, name, ty) => self.elab_decl_datatype(tv, name, ty),
            DeclKind::Type(tv, name, ty) => self.elab_decl_type(tv, name, ty),
            _ => unimplemented!(),
        }
    }

    pub fn elab_program(&mut self, prog: &'s Program) -> Result<Vec<HirId>, ElabError> {
        let mut v = Vec::with_capacity(prog.decls.len());
        for d in &prog.decls {
            v.push(self.elab_decl(d)?);
        }
        Ok(v)
    }
}
