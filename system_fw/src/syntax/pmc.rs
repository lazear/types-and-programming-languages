/// Pattern Match Compiler
/// See:    Wadler87        "Efficient Compilation of Pattern-Matching"
///         Pettersson92    "A Term Pattern-Match Compiler Inspired by Finite Automata Theory"
///         Maranget08      "Compiling Pattern Matching to good Decision Trees"
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
use ast::{Expr, ExprKind, PatKind, Pattern};

/// The intermediate data structure in the algorithm is an acylic
/// DFA.
/// Each state in the automaton is either final or a test state.
enum State {
    Final(Expr),
    Test(Pattern),
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
