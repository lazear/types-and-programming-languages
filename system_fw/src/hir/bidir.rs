use super::*;
use crate::elaborate::Elaborated;
use std::collections::HashMap;

use self::TypeVar::*;
use super::Type::*;

#[derive(Debug)]
pub struct Context<'hir> {
    hir_map: &'hir HashMap<HirId, Decl>,
    defs: HashMap<HirId, TypeVar>,
    ctx: Vec<TypeVar>,
    gen: usize,
}

#[derive(Clone, Debug)]
pub enum TypeVar {
    // alpha hat
    Exist(usize),
    // alpha
    All(usize),
    // > alpha hat
    Marker(usize),
    // x: A
    Concrete(Type),
}

impl<'hir> Context<'hir> {
    pub fn infer(&mut self, e: &'hir Expr) -> Option<TypeVar> {
        match e {
            Expr::Unit => Some(Concrete(Unit)),
            Expr::Int(_) => Some(Concrete(Int)),
            Expr::LocalVar(DeBruijn { idx, .. }) => {
                self.ctx.get(self.ctx.len().checked_sub(idx + 1)?).cloned()
            }
            _ => None,
        }
    }
    pub fn check(&mut self, e: &'hir Expr, ty: TypeVar) -> Option<TypeVar> {
        println!("{:?}", e);
        match e {
            Expr::Unit => Some(Concrete(Unit)),
            Expr::Int(_) => Some(Concrete(Int)),
            _ => None,
        }
    }
}

pub fn test(prog: Elaborated) {
    let mut ctx = Context {
        hir_map: &prog.elaborated,
        defs: HashMap::new(),
        ctx: Vec::new(),
        gen: 0,
    };

    for id in prog.decls {
        match prog.elaborated.get(&id) {
            Some(Decl::Value(e)) => {
                ctx.infer(e);
            }
            _ => {}
        }

        dbg!(&ctx);
    }
}
