use std::rc::Rc;
use std::ops::Deref;

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
pub enum Type {
    Nat,
    Bool,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Term {
    TmTrue,
    TmFalse,
    TmIf(RcTerm, RcTerm, RcTerm),
    TmZero,
    TmSucc(RcTerm),
    TmPred(RcTerm),
    TmIsZero(RcTerm),
}


#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
pub enum TyError {
    TypingError,
}

pub fn typing(tm: RcTerm) -> Result<Type, TyError> {
    match &tm as &Term {
        Term::TmTrue => Ok(Type::Bool),
        Term::TmFalse => Ok(Type::Bool),
        Term::TmZero => Ok(Type::Nat),
        Term::TmSucc(t) => {
            match typing(t.clone()) {
                Ok(Type::Nat) => Ok(Type::Nat),
                _ => Err(TyError::TypingError),
            }
        }
        Term::TmPred(t) => {
            match typing(t.clone()) {
                Ok(Type::Nat) => Ok(Type::Nat),
                _ => Err(TyError::TypingError),
            }
        },
        Term::TmIsZero(t) => {
            match typing(t.clone()) {
                Ok(Type::Nat) => Ok(Type::Bool),
                _ => Err(TyError::TypingError),
            }
        },
        Term::TmIf(a, b, c) => {
            match typing(a.clone()) {
                Ok(Type::Bool) => {
                    let ty_b = typing(b.clone())?;
                    let ty_c = typing(c.clone())?;
                    if ty_b == ty_c {
                        Ok(ty_b)
                    } else {
                        Err(TyError::TypingError)
                    }
                },
                _ =>  Err(TyError::TypingError)
            }
        }
    }
} 

#[derive(Clone, PartialEq, PartialOrd)]
pub struct RcTerm(pub Rc<Term>);

impl std::fmt::Debug for RcTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl From<Term> for RcTerm {
    fn from(term: Term) -> RcTerm {
        RcTerm(Rc::new(term))
    }
}

impl Deref for RcTerm {
    type Target = Term;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
