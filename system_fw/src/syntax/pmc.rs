/// Pattern Match Compiler
/// See:    Wadler87        "Efficient Compilation of Pattern-Matching"
///         Pettersson92    "A Term Pattern-Match Compiler Inspired by Finite
/// Automata Theory"         Maranget08      "Compiling Pattern Matching to good
/// Decision Trees"
///
/// Wadler's 'demo' function:
///
///
/// fun demo Nil ys = A ys
///   | demo xs Nil = B xs
///   | demo (Cons (x, xs)) (Cons (y, ys)) = C x xs y ys
///
/// We follow the algorithm from Pettersson92, some comments below are
/// directly taken from the paper
use super::*;
use ast::{Expr, PatKind, Pattern};

/// The intermediate data structure in the algorithm is an acylic
/// DFA.
/// Each state in the automaton is either final or a test state.
enum State {
    Final(Expr),
    Test(String, Vec<(Pattern, State)>),
}

/// Each state also has some additional attributes:
/// - A *unique* stamp for identification purposes
/// - A set of *free variables*
/// - A *reference count* indicating the number of direct references
pub struct DfaState {
    state: State,
    id: usize,
    rc: usize,
    fv: Vec<String>,
}

// Step 1: Renaming
// I think it's probably easiest to transform to dB indices here

pub struct Augmented {
    s: String,
    p: Pattern,
}

#[derive(Debug, Clone)]
pub struct InitMatrix {
    pats: Vec<Vec<Pattern>>,
    exprs: Vec<Expr>,
    rows: usize,
    cols: usize,
}

impl InitMatrix {
    fn from_arms(arms: Vec<ast::FnArm>) -> Self {
        let rows = arms.len();
        let mut pats = Vec::with_capacity(rows);
        let mut exprs = Vec::with_capacity(rows);

        let mut cols = 0;
        for arm in arms {
            cols = cols.max(arm.pats.len());
            pats.push(arm.pats);
            exprs.push(arm.expr);
        }

        InitMatrix {
            pats,
            exprs,
            rows,
            cols,
        }
    }

    fn variable_rule(&self) -> bool {
        let r = &self.pats[0];
        r.iter().all(pat_variable_rule)
    }

    fn column(&self, idx: usize) -> Vec<&Pattern> {
        let mut v = Vec::new();
        for i in 0..self.rows {
            v.push(&self.pats[i][idx]);
        }
        v
    }

    fn naive(&self) {
        for j in 0..self.cols {
            let col = self.column(j);
            println!("col {}: {:?}", j, col);
        }
    }
}

fn pat_augmentation(root_var: &str, pat: Pattern) -> Augmented {
    use PatKind::*;
    match pat.kind {
        Any => Augmented {
            s: root_var.into(),
            p: pat,
        },
        _ => Augmented {
            s: root_var.into(),
            p: pat,
        },
    }
    // Augmented {
    //     s: format!("{}", root_var),
    //     p: pat
    // }
}

fn pat_equivalence(a: &Pattern, b: &Pattern) -> bool {
    use PatKind::*;
    match (&a.kind, &b.kind) {
        (Any, Any) => true,
        (Variable(_), Variable(_)) => true,
        (Unit, Unit) => true,
        (Ascribe(c, d), Ascribe(e, f)) => pat_equivalence(c, e) && d == f,
        (Literal(c), Literal(d)) => c == d,
        (Constructor(c), Constructor(d)) => c == d,
        (Application(c1, c2), Application(d1, d2)) => {
            pat_equivalence(c1, d1) && pat_equivalence(c2, d2)
        }
        (Product(c), Product(d)) => c.iter().zip(d.iter()).all(|(a, b)| pat_equivalence(a, b)),
        (Record(c), Record(d)) => c == d,
        _ => false,
    }
}

/// Are all bindings variables or _?
fn pat_variable_rule(a: &Pattern) -> bool {
    use PatKind::*;
    match &a.kind {
        Any => true,
        Variable(_) => true,
        Ascribe(p, _) => pat_variable_rule(p),
        Record(p) | Product(p) => p.iter().all(pat_variable_rule),
        _ => false,
    }
}

/// Extract constructor name, ignoring nested patterns
fn pat_constr(a: &Pattern) -> Option<&str> {
    match &a.kind {
        PatKind::Constructor(s) => Some(&s),
        PatKind::Application(c, _) => pat_constr(&c),
        _ => None,
    }
}

pub fn experiment() {
    let input = "fun demo Nil ys = A ys
        | demo xs Nil = B xs
        | demo (Cons (x, xs)) (Cons (y, ys)) = C x xs y ys
        | demo xs ys = D xs ys";
    let mut p = super::parser::Parser::new(input);
    let tm = p.parse_decl().unwrap();
    let (_, arms) = match tm.kind {
        ast::DeclKind::Function(_, ident, arms) => (ident, arms),
        _ => panic!("Invalid AST!"),
    };
    let mat = InitMatrix::from_arms(arms);

    dbg!(&mat);

    mat.naive();
}
