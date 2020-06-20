use super::*;

fn var_bind(var: TypeVar, ty: Type) -> Result<HashMap<TypeVar, Type>, String> {
    if ty.occurs(var) {
        return Err(format!("Fails occurs check! {:?} {:?}", var, ty));
    }
    let mut sub = HashMap::new();
    match ty {
        Type::Var(x) if x == var => {}
        _ => {
            sub.insert(var, ty);
        }
    }
    Ok(sub)
}

pub fn unify(a: Type, b: Type) -> Result<HashMap<TypeVar, Type>, String> {
    println!("{:?} {:?}", a, b);
    match (a, b) {
        (Type::Con(a, a_args), Type::Con(b, b_args)) => {
            if a_args.len() == b_args.len() && a == b {
                solve(a_args.into_iter().zip(b_args.into_iter()))
            } else {
                Err(format!(
                    "Can't unify types: {:?} {:?}",
                    Type::Con(a, a_args),
                    Type::Con(b, b_args)
                ))
            }
        }
        (Type::Var(tv), b) => var_bind(tv, b),
        (a, Type::Var(tv)) => var_bind(tv, a),
    }
}

// pub fn solve<I: Iterator<Item = (Type, Type)>>(constraints: I) -> Result<HashMap<TypeVar, Type>, (Type, Type)> {

// }

pub fn solve<I: Iterator<Item = (Type, Type)>>(iter: I) -> Result<HashMap<TypeVar, Type>, String> {
    let mut sub = HashMap::new();
    for (a, b) in iter {
        let tmp = unify(a.clone().apply(&sub), b.clone().apply(&sub))?;
        sub = compose(tmp, sub);
    }
    Ok(sub)
}
