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
            Expr::Ann(e, ty) => format!("{} : {}", walk(e, map), ty_display(ty)),
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
