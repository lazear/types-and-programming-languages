use std::collections::VecDeque;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Term {
    True,
    False,
    // DeBrujin index
    Var(String),
    // Type of bound variable, and body of abstraction
    Abs(String, Type, Box<Term>),
    // Application (t1 t2)
    App(Box<Term>, Box<Term>),
    If(Box<Term>, Box<Term>, Box<Term>),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Type {
    Bool,
    Arrow(Box<Type>, Box<Type>),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum TypeError {
    Guard,
    ArmMismatch,
    ParameterMismatch,
    UnknownVariable,
    ExpectedArrow,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Context<'a> {
    parent: Option<&'a Context<'a>>,
    var: String,
    bind: Binding,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Binding {
    Name,
    Variable(Type),
}

impl<'a> Context<'a> {
    pub fn add<'ctx>(&'ctx self, var: String, bind: Binding) -> Context<'ctx> {
        Context {
            parent: Some(self),
            var,
            bind,
        }
    }

    pub fn get(&self, var: &str) -> Option<&Binding> {
        if self.var == var {
            Some(&self.bind)
        } else {
            if let Some(ctx) = self.parent {
                ctx.get(var)
            } else {
                None
            }
        }
    }

    pub fn type_of(&self, term: &Term) -> Result<Type, TypeError> {
        use Term::*;
        match term {
            True => Ok(Type::Bool),
            False => Ok(Type::Bool),
            If(guard, csq, alt) => {
                if let Ok(Type::Bool) = self.type_of(guard) {
                    let ty1 = self.type_of(csq)?;
                    let ty2 = self.type_of(alt)?;
                    if ty1 == ty2 {
                        Ok(ty2)
                    } else {
                        Err(TypeError::ArmMismatch)
                    }
                } else {
                    Err(TypeError::Guard)
                }
            }
            Var(s) => match self.get(s) {
                Some(Binding::Variable(ty)) => Ok(ty.clone()),
                _ => Err(TypeError::UnknownVariable),
            },
            Abs(v, ty, body) => {
                let ctx = self.add(v.clone(), Binding::Variable(ty.clone()));
                let ty_body = ctx.type_of(body)?;
                Ok(Type::Arrow(Box::new(ty.clone()), Box::new(ty_body)))
            }
            App(t1, t2) => {
                let ty1 = self.type_of(t1)?;
                let ty2 = self.type_of(t2)?;
                match ty1 {
                    Type::Arrow(ty11, ty12) => {
                        if *ty11 == ty2 {
                            Ok(*ty12)
                        } else {
                            Err(TypeError::ParameterMismatch)
                        }
                    }
                    _ => Err(TypeError::ExpectedArrow),
                }
            }
        }
    }

    pub fn root() -> Context<'a> {
        Context {
            parent: None,
            var: String::default(),
            bind: Binding::Name,
        }
    }
}
