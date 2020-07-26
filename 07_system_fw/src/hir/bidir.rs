use super::*;
use crate::elaborate::Elaborated;
use std::collections::HashMap;

use super::Type::*;

#[derive(Debug)]
pub struct Context<'hir> {
    hir_map: &'hir HashMap<HirId, Decl>,
    defs: HashMap<HirId, Type>,
    ctx: Vec<Element>,
    gen: usize,
}

/// An element in the typing context
#[derive(Clone, Debug, PartialEq)]
pub enum Element {
    /// Universal type variable
    Var,
    /// Term variable typing x : A. We differ from the paper in that we use
    /// de Bruijn indices for variables, so we don't need to mark which var
    /// this annotation belongs to - it always belongs to the innermost binding (idx 0)
    /// and we will find this by traversing the stack
    Ann(Type),
    /// Unsolved existential type variable
    Exist(usize),
    /// Existential type variable that has been solved
    /// to some monotype
    Solved(usize, Type),
    /// I am actually unsure if we really need a marker, due to how we structure
    /// scoping, see `with_scope` method.
    Marker(usize),
}

pub enum Error {
    UnboundVariable,
}

impl<'hir> Context<'hir> {
    /// Find the term annotation corresponding to de Bruijn index `idx`.
    /// We traverse the stack in a reversed order, counting each annotation
    /// we come across
    fn find_annotation(&self, idx: usize) -> Option<&Type> {
        let mut ix = 0;
        for elem in self.ctx.iter().rev() {
            match &elem {
                Element::Ann(ty) => {
                    if ix == idx {
                        return Some(&ty);
                    }
                    ix += 1
                }
                _ => {}
            }
        }

        None
    }

    pub fn infer(&mut self, e: &'hir Expr) -> Result<Type, Error> {
        use Expr::*;
        match e {
            Unit => Ok(Type::Unit),
            Int(usize) => Ok(Type::Int),
            LocalVar(db) => self.find_annotation(db.idx).cloned().ok_or(Error::UnboundVariable),
            ProgramVar(id) => unimplemented!(),

            // Datatype constructor, pointing to type def and tag of the constr
            Constr(id, tag) => unimplemented!(),
            Deconstr(id, tag) => unimplemented!(),
            If(e1, e2, e3) => unimplemented!(),

            Abs(ty, ex) => {
                dbg!(ty);
                unimplemented!()
            }
            App(e1, e2) => unimplemented!(),
            TyAbs(k, ex) => unimplemented!(),
            TyApp(ex, ty) => unimplemented!(),
            Record(fields) => fields
                .iter()
                .map(|f| {
                    Ok(Row {
                        label: f.label.clone(),
                        ty: self.infer(&f.expr)?,
                    })
                })
                .collect::<Result<Vec<_>, _>>()
                .map(Type::Record),
            Tuple(exprs) => exprs
                .iter()
                .map(|e| self.infer(e))
                .collect::<Result<Vec<_>, _>>()
                .map(Type::Product),
            RecordProj(ex, idx) => unimplemented!(),
            TupleProj(ex, idx) => unimplemented!(),
            Case(ex, arms) => unimplemented!(),
            Let(decls, ex) => unimplemented!(),
            Fix(ex) => unimplemented!(),
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
