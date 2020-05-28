use std::fmt;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct DeBruijn {
    pub idx: usize,
    pub name: String,
}

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Eq, Hash)]
pub struct HirId(pub(crate) u32);

/// Arm of a case expression
#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Arm {
    pub pat: Pattern,
    pub expr: Expr,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Field {
    pub label: String,
    pub expr: Expr,
}

pub struct Program {
    pub decls: Vec<Decl>,
}

// A lot of desugaring goes on here
#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Decl {
    Type(Type),
    Value(Expr),
}

/// Patterns for case and let expressions
#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Pattern {
    /// Wildcard pattern, this always matches
    Any,
    Unit,
    Ascribe(Box<Pattern>, Box<Type>),
    /// Constant pattern
    Literal(usize),
    /// Datatype constructor, HirId points to the constructor value binding
    Constructor(HirId),
    /// Variable binding
    Variable(String),
    /// Tuple of pattern bindings (_, x)
    Product(Vec<Pattern>),
    /// Record pattern { label1, label2 }
    Record(Vec<String>),
    /// Algebraic datatype constructor, along with binding pattern
    Application(HirId, Box<Pattern>),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Expr {
    Unit,
    Int(usize),
    LocalVar(DeBruijn),
    ProgramVar(HirId),

    // Datatype constructor, pointing to type def and tag of the constr
    Constr(HirId, usize),
    Deconstr(HirId, usize),

    If(Box<Expr>, Box<Expr>, Box<Expr>),

    // Desugar into explicit type bindings
    Abs(Box<Type>, Box<Expr>),
    App(Box<Expr>, Box<Expr>),

    TyAbs(Box<Kind>, Box<Expr>),
    TyApp(Box<Expr>, Box<Type>),
    Record(Vec<Field>),
    Tuple(Vec<Expr>),

    RecordProj(Box<Expr>, String),
    TupleProj(Box<Expr>, usize),
    Case(Box<Expr>, Vec<Arm>),
    Let(Vec<Decl>, Box<Expr>),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Kind {
    Star,
    Arrow(Box<Kind>, Box<Kind>),
}

#[derive(Clone, PartialEq, PartialOrd)]
pub enum Type {
    Int,
    Bool,
    Unit,
    Infer,
    Error,
    /// Defined name
    Defined(HirId),
    /// Type variable 'a
    Var(DeBruijn),
    /// Type of functions from terms to terms
    Arrow(Box<Type>, Box<Type>),
    /// Sum type; None | Some of 'a
    Sum(Vec<Variant>),
    /// Tuple type (ty * ty * ... tyN), invariant that N >= 1
    Product(Vec<Type>),
    /// Record type { [label: ty],+ }, invariant that N >=1
    Record(Vec<Row>),
    /// Existential type: exists (a :: K) of ty
    Existential(Box<Kind>, Box<Type>),
    /// Universal type: forall (a :: K) of ty
    Universal(Box<Kind>, Box<Type>),
    /// Type level function abstraction
    Abstraction(Box<Kind>, Box<Type>),
    /// Type level function application
    Application(Box<Type>, Box<Type>),
    /// Recursive type
    Recursive(Box<Type>),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Variant {
    pub label: String,
    pub ty: Option<Type>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Row {
    pub label: String,
    pub ty: Type,
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Unit => write!(f, "unit"),
            Type::Int => write!(f, "int"),
            Type::Bool => write!(f, "bool"),
            Type::Infer => write!(f, "_"),
            Type::Error => write!(f, "!"),
            Type::Var(v) => write!(f, "{}", &v.name),
            Type::Sum(v) => write!(
                f,
                "{}",
                v.iter()
                    .map(|x| format!(
                        "{}{}",
                        x.label,
                        x.ty.as_ref()
                            .map(|i| format!(" of {:?}", i))
                            .unwrap_or(String::new())
                    ))
                    .collect::<Vec<String>>()
                    .join(" | ")
            ),
            Type::Product(v) => write!(
                f,
                "({})",
                v.iter()
                    .map(|x| format!("{:?}", x))
                    .collect::<Vec<String>>()
                    .join(",")
            ),
            Type::Record(v) => write!(
                f,
                "{{{}}}",
                v.iter()
                    .map(|x| format!("{}: {:?}", x.label, x.ty))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Type::Defined(s) => write!(f, "tctx#{:?}", s),
            Type::Arrow(t1, t2) => write!(f, "({:?}->{:?})", t1, t2),
            Type::Universal(k, ty) => write!(f, "forall X :: {:?}.{:?}", k, ty),
            Type::Existential(k, ty) => write!(f, "exists X. :: {:?}. {:?}", k, ty),
            Type::Abstraction(k, ty) => write!(f, "fn (X. :: {:?}) => {:?}", k, ty),
            Type::Application(a, b) => write!(f, "{:?} {:?}", b, a),
            Type::Recursive(ty) => write!(f, "rec {:?}", ty),
        }
    }
}
