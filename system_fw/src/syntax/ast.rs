use util::span::Span;

#[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd, Eq, Hash)]
pub struct AstId(pub(crate) u32);
pub const AST_DUMMY: AstId = AstId(std::u32::MAX);

macro_rules! container {
    ($id:ident, $id2:ident) => {
        #[derive(Clone, PartialEq, PartialOrd)]
        pub struct $id {
            pub kind: $id2,
            pub span: Span,
            pub id: AstId,
        }

        impl $id {
            pub fn new(kind: $id2, span: Span) -> $id {
                $id {
                    kind,
                    span,
                    id: AST_DUMMY,
                }
            }
        }

        impl std::fmt::Debug for $id {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "{:?}", self.kind)
            }
        }
    };
}

container!(Expr, ExprKind);
container!(Type, TypeKind);
container!(Decl, DeclKind);
container!(Pattern, PatKind);

/// Arm of a case expression
#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Arm {
    pub pat: Pattern,
    pub expr: Expr,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Field {
    pub label: String,
    pub expr: Expr,
    pub span: Span,
}

/// Patterns for case and let expressions
#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum PatKind {
    /// Wildcard pattern, this always matches
    Any,
    Unit,
    Ascribe(Box<Pattern>, Box<Type>),
    /// Constant pattern
    Literal(usize),
    /// Datatype constructor
    Constructor(String),
    /// Variable binding
    Variable(String),
    /// Tuple of pattern bindings (_, x)
    Product(Vec<Pattern>),
    /// Record pattern { label1, label2 }
    Record(Vec<Pattern>),
    /// Algebraic datatype constructor, along with binding pattern
    Application(Box<Pattern>, Box<Pattern>),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum ExprKind {
    Unit,
    Int(usize),
    Var(String),
    Constr(String),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Abs(Box<Pattern>, Box<Expr>),
    App(Box<Expr>, Box<Expr>),

    /// Explicit type abstraction `fn 'x value (arg: 'x) = arg`
    TyAbs(Box<Kind>, Box<Expr>),

    /// Explicit type application `e @ty`
    TyApp(Box<Expr>, Box<Type>),
    Record(Vec<Field>),
    Tuple(Vec<Expr>),
    Projection(Box<Expr>, Box<Expr>),
    Case(Box<Expr>, Vec<Arm>),
    Let(Vec<Decl>, Box<Expr>),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct FnArm {
    pub span: Span,
    pub pats: Vec<Pattern>,
    pub expr: Expr,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum DeclKind {
    Type(Vec<Type>, String, Type),
    Datatype(Vec<Type>, String, Type),
    Value(Vec<Type>, Pattern, Expr),
    Function(Vec<Type>, String, Vec<FnArm>),
    Expr(Expr),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Kind {
    Star,
    Arrow(Box<Kind>, Box<Kind>),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum TypeKind {
    Int,
    Bool,
    Unit,
    /// Defined name
    Defined(String),
    /// Type variable 'a
    Variable(String),
    /// Type of functions from terms to terms
    Function(Box<Type>, Box<Type>),
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

#[derive(Clone, PartialEq, PartialOrd)]
pub struct Variant {
    pub label: String,
    pub ty: Option<Type>,
    pub span: Span,
}

#[derive(Clone, PartialEq, PartialOrd)]
pub struct Row {
    pub label: String,
    pub ty: Type,
    pub span: Span,
}

impl TypeKind {
    pub fn variants(&self) -> &[Variant] {
        match self {
            TypeKind::Sum(v) => &v,
            _ => panic!("Not a sum type!"),
        }
    }

    pub fn as_tyvar(&self) -> String {
        match self {
            TypeKind::Variable(s) => s.clone(),
            _ => panic!("Not a type var!"),
        }
    }
}

impl Decl {
    fn definition_name(&self) -> Option<&str> {
        match &self.kind {
            DeclKind::Type(_, name, _) | DeclKind::Datatype(_, name, _) => Some(name),
            _ => None,
        }
    }
}

impl std::fmt::Debug for Variant {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} of {:?}", self.label, self.ty)
    }
}

impl std::fmt::Debug for Row {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}: {:?}", self.label, self.ty)
    }
}
