use super::*;
use terms::*;
use types::TyField;

#[derive(Clone, Debug)]
pub enum Decl {
    Value(String, Type),
}

#[derive(Clone, Debug)]
pub struct Signature {
    pub abstracts: Vec<String>,
    pub decls: Vec<Decl>,
}

pub struct Functor {
    pub arguments: Vec<(String, Signature)>,
}

pub fn eq_sig() -> Signature {
    Signature {
        abstracts: vec!["t".into()],
        decls: vec![Decl::Value(
            "eq".into(),
            arrow!(Type::Var(0), arrow!(Type::Var(0), Type::Bool)),
        )],
    }
}

pub fn set_sig() -> Signature {
    Signature {
        abstracts: vec!["elem".into(), "set".into()],
        decls: vec![
            Decl::Value("empty".into(), Type::Var(0)),
            Decl::Value(
                "mem".into(),
                arrow!(Type::Var(1), arrow!(Type::Var(0), Type::Bool)),
            ),
        ],
    }
}

impl Signature {
    pub fn to_existential(self) -> Type {
        let record = Type::Record(
            self.decls
                .into_iter()
                .map(|d| match d {
                    Decl::Value(label, ty) => TyField {
                        label,
                        ty: Box::new(ty),
                    },
                })
                .collect::<Vec<TyField>>(),
        );

        if self.abstracts.is_empty() {
            exist!(kind!(*), record)
        } else {
            let mut ty = exist!(kind!(*), record);
            for _ in 1..self.abstracts.len() {
                ty = exist!(kind!(*), ty);
            }
            ty
        }
    }

    pub fn to_record(self) -> Type {
        Type::Record(
            self.decls
                .into_iter()
                .map(|d| match d {
                    Decl::Value(label, ty) => TyField {
                        label,
                        ty: Box::new(ty),
                    },
                })
                .collect::<Vec<TyField>>(),
        )
    }

    pub fn instantiate(self, mut concrete: Vec<Type>, body: Term) -> Term {
        let exist = self.to_existential();
        let fst = concrete.pop().unwrap();
        let mut tm = pack!(fst, body, exist.clone());
        for conc in concrete {
            tm = pack!(conc, tm, exist.clone());
        }
        tm
    }
}
