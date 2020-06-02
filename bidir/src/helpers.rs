use super::*;

macro_rules! var {
    ($x:expr) => {
        Expr::Var($x)
    };
}

macro_rules! app {
    ($x:expr, $y:expr) => {
        Expr::App(Box::new($x), Box::new($y))
    };
}

macro_rules! abs {
    ($x:expr) => {
        Expr::Abs(Box::new($x))
    };
}

macro_rules! ann {
    ($x:expr, $t:expr) => {
        Expr::Ann(Box::new($x), Box::new($t))
    };
}

macro_rules! ife {
    ($a:expr, $b:expr, $c:expr) => {
        Expr::If(Box::new($a), Box::new($b), Box::new($c))
    };
}

macro_rules! arrow {
    ($t1:expr, $t2:expr) => {
        Type::Arrow(Box::new($t1), Box::new($t2))
    };
}

macro_rules! forall {
    ($t1:expr) => {
        Type::Univ(Box::new($t1))
    };
}

macro_rules! case {
    ($ex:expr, $pat1:expr => $arm1:expr, $pat2:expr => $arm2:expr) => {
        Expr::Case(
            Box::new($ex),
            Arm {
                pat: Box::new($pat1),
                expr: Box::new($arm1),
            },
            Arm {
                pat: Box::new($pat2),
                expr: Box::new($arm2),
            },
        )
    };
}

macro_rules! inj {
    (l; $ex:expr, $ty:expr) => {
        Expr::Inj(LR::Left, Box::new($ex), Box::new($ty))
    };
    (r; $ex:expr, $ty:expr) => {
        Expr::Inj(LR::Right, Box::new($ex), Box::new($ty))
    };
}

macro_rules! proj {
    (l; $ex:expr) => {
        Expr::Proj(LR::Left, Box::new($ex))
    };
    (r; $ex:expr) => {
        Expr::Proj(LR::Right, Box::new($ex))
    };
}

macro_rules! pair {
    ($a:expr, $b:expr) => {
        Expr::Pair(Box::new($a), Box::new($b))
    };
}

macro_rules! sum {
    ($a:expr, $b:expr) => {
        Type::Sum(Box::new($a), Box::new($b))
    };
}

macro_rules! product {
    ($a:expr, $b:expr) => {
        Type::Product(Box::new($a), Box::new($b))
    };
}

fn ty_display(ty: &Type) -> String {
    use std::collections::HashMap;
    let mut map = HashMap::new();
    fn walk(ty: &Type, map: &mut HashMap<usize, char>) -> String {
        let nc = ('a' as u8 + map.len() as u8) as char;
        let vc = ('A' as u8 + map.len() as u8) as char;
        match ty {
            Type::Unit => "()".into(),
            Type::Bool => "bool".into(),
            Type::Int => "int".into(),
            Type::Arrow(a, b) => format!("({}->{})", walk(a, map), walk(b, map)),
            Type::Univ(ty) => format!("forall {}. {}", vc, walk(ty, map)),
            Type::Exist(idx) => format!("{}", map.entry(*idx).or_insert(nc)),
            Type::Var(idx) => format!("{}", map.entry(0xdeadbeef + *idx).or_insert(vc)),
            Type::Sum(a, b) => format!("{} + {}", walk(a, map), walk(b, map)),
            Type::Product(a, b) => format!("({} x {})", walk(a, map), walk(b, map)),
        }
    }
    walk(ty, &mut map)
}

fn expr_display(ex: &Expr) -> String {
    use std::collections::HashMap;
    let mut map = HashMap::new();

    fn walk(ex: &Expr, map: &mut HashMap<usize, char>) -> String {
        match ex {
            Expr::Unit => "()".into(),
            Expr::True => "true".into(),
            Expr::False => "false".into(),
            Expr::Int(i) => format!("{}", i),
            Expr::If(e1, e2, e3) => format!("if {} then {} else {}", walk(e1, map), walk(e2, map), walk(e3, map)),
            Expr::App(a, b) => format!("({} {})", walk(a, map), walk(b, map)),
            Expr::Abs(body) => {
                let vc = ('a' as u8 + map.len() as u8) as char;
                let vc = *map.entry(map.len()).or_insert(vc);
                format!("(\\{}. {})", vc, walk(body, map))
            }
            Expr::Var(idx) => {
                let i = map.len() - (*idx + 1);
                let vc = ('a' as u8 + i as u8) as char;
                format!("{}", map.entry(i).or_insert(vc))
            }
            Expr::Ann(e, ty) => format!("<{} : {}>", walk(e, map), ty_display(ty)),
            Expr::Inj(LR::Left, e, ty) => format!("inl {}", walk(e, map)),
            Expr::Inj(LR::Right, e, ty) => format!("inr {}", walk(e, map)),
            Expr::Case(e, la, ra) => format!(
                "case {} of {} => {}, {} => {}",
                walk(e, map),
                walk(&la.pat, map),
                walk(&la.expr, map),
                walk(&ra.pat, map),
                walk(&ra.expr, map)
            ),
            Expr::Proj(LR::Left, e) => format!("{}.0", walk(e, map)),
            Expr::Proj(LR::Right, e) => format!("{}.1", walk(e, map)),
            Expr::Pair(a, b) => format!("({},{})", walk(a, map), walk(b, map)),
        }
    }
    walk(ex, &mut map)
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", ty_display(self))
    }
}
impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", expr_display(self))
    }
}
