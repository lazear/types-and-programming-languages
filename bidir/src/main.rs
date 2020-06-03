//! "Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism"
//! Paper by J. Dunfield and N. Krishnaswami
//!
//! Also see very useful Haskell implementation:
//! https://github.com/lexi-lambda/higher-rank/

#![allow(non_snake_case)]
#[macro_use]
mod helpers;

#[derive(Clone, Debug, PartialEq)]
enum Kind {
    Star,
    Arrow(Box<Kind>, Box<Kind>),
}

/// A source-level type
#[derive(Clone, Debug, PartialEq)]
enum Type {
    Unit,
    Int,
    Bool,
    /// A type variable
    Var(usize),
    /// The type of functions
    Arrow(Box<Type>, Box<Type>),
    /// Existential type variable that can be instantiated to a monotype
    Exist(usize),
    /// Universally quantified type, forall. A
    Univ(Box<Kind>, Box<Type>),
    /// Class left/right sum type
    Sum(Box<Type>, Box<Type>),
    /// Simple pair type
    Product(Box<Type>, Box<Type>),

    Abs(Box<Kind>, Box<Type>),
    App(Box<Type>, Box<Type>),
}

impl Type {
    fn monotype(&self) -> bool {
        match &self {
            Type::Univ(_, _) => false,
            Type::Arrow(t1, t2) => t1.monotype() && t2.monotype(),
            _ => true,
        }
    }

    /// Collect the free existential variables of the type
    fn freevars(&self) -> Vec<usize> {
        fn walk(ty: &Type, vec: &mut Vec<usize>) {
            match ty {
                Type::Unit | Type::Int | Type::Bool | Type::Var(_) => {}
                Type::Exist(v) => vec.push(*v),
                Type::Arrow(a, b) => {
                    walk(a, vec);
                    walk(b, vec);
                }
                Type::Sum(a, b) => {
                    walk(a, vec);
                    walk(b, vec);
                }
                Type::Product(a, b) => {
                    walk(a, vec);
                    walk(b, vec);
                }
                Type::Univ(k, a) => walk(a, vec),
                Type::Abs(k, a) => walk(a, vec),
                Type::App(a, b) => {
                    walk(a, vec);
                    walk(b, vec);
                }
            }
        }
        let mut v = Vec::new();
        walk(self, &mut v);
        v
    }

    /// Perform de Bruijn shifting, algorithm from TAPL
    fn shift(&mut self, s: isize) {
        fn walk(t: &mut Type, c: usize, s: isize) {
            match t {
                Type::Var(n) if *n >= c => *n = (*n as isize + s) as usize,
                Type::Arrow(a, b) => {
                    walk(a, c, s);
                    walk(b, c, s);
                }
                Type::Sum(a, b) => {
                    walk(a, c, s);
                    walk(b, c, s);
                }
                Type::Product(a, b) => {
                    walk(a, c, s);
                    walk(b, c, s);
                }
                Type::Univ(k, a) => walk(a, c + 1, s),
                Type::Abs(k, a) => walk(a, c + 1, s),
                Type::App(a, b) => {
                    walk(a, c, s);
                    walk(b, c, s);
                }
                Type::Unit | Type::Int | Type::Bool | Type::Var(_) => {}
                Type::Exist(_) => {}
            }
        }
        walk(self, 0, s);
    }

    /// Perform subsitution of type `s` into self, algorithm from TAPL
    fn subst(&mut self, s: &mut Type) {
        fn walk<F: Fn(&mut Type, usize)>(t: &mut Type, c: usize, f: &F) {
            match t {
                Type::Var(n) if *n == c => f(t, c),
                Type::Arrow(a, b) => {
                    walk(a, c, f);
                    walk(b, c, f);
                }
                Type::Sum(a, b) => {
                    walk(a, c, f);
                    walk(b, c, f);
                }
                Type::Product(a, b) => {
                    walk(a, c, f);
                    walk(b, c, f);
                }
                Type::Univ(k, a) => walk(a, c + 1, f),
                Type::Abs(k, a) => walk(a, c + 1, f),
                Type::App(a, b) => {
                    walk(a, c, f);
                    walk(b, c, f);
                }
                Type::Unit | Type::Int | Type::Bool | Type::Var(_) => {}
                Type::Exist(_) => {}
            }
        }
        s.shift(1);
        walk(self, 0, &|f, c| {
            let mut s = s.clone();
            s.shift(c as isize);
            *f = s
        });
        self.shift(-1);
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum LR {
    Left,
    Right,
}

/// An expression in our simply typed lambda calculus
#[derive(Clone, Debug, PartialEq)]
enum Expr {
    /// The unit expression, ()
    Unit,
    True,
    False,
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Int(usize),
    /// A term variable, given in de Bruijn notation
    Var(usize),
    /// A lambda abstraction, with it's body. (\x. body)
    Abs(Box<Expr>),
    /// Application (e1 e2)
    App(Box<Expr>, Box<Expr>),
    /// Explicit type annotation of a term, (x : A)
    Ann(Box<Expr>, Box<Type>),
    /// Injection left/right into a sum type x1
    Inj(LR, Box<Expr>, Box<Type>),
    /// Simplified case expr
    Case(Box<Expr>, Arm, Arm),
    /// Introduction of a pair
    Pair(Box<Expr>, Box<Expr>),
    /// Projection left/right from a pair
    Proj(LR, Box<Expr>),
}

#[derive(Clone, Debug, PartialEq)]
struct Arm {
    pat: Box<Expr>,
    expr: Box<Expr>,
}

/// An element in the typing context
#[derive(Clone, Debug, PartialEq)]
enum Element {
    /// Universal type variable
    Var(Kind),
    /// Term variable typing x : A. We differ from the paper in that we use
    /// de Bruijn indices for variables, so we don't need to mark which var
    /// this annotation belongs to - it always belongs to the innermost binding (idx 0)
    /// and we will find this by traversing the stack
    Ann(Type),
    /// Unsolved existential type variable
    Exist(usize),
    /// Existential type variable that has been solved
    /// to some monotype
    Solved(usize, Type),
    /// I am actually unsure if we really need a marker, due to how we structure
    /// scoping, see `with_scope` method.
    Marker(usize),
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Context {
    /// We model the algorithmic context as a simple stack of elements
    ctx: Vec<Element>,
    /// We assign fresh exist. variables a unique, strictly increasing number
    ev: usize,
}

impl Context {
    /// Generate a fresh identifier
    fn fresh_ev(&mut self) -> usize {
        let e = self.ev;
        self.ev += 1;
        e
    }

    /// Requires a mutable reference to self because we need to push/pop onto the stack
    /// in the case of universally quantified variables. However, this can be considered
    /// mostly immutable, since self should be equal before and after the call
    fn well_formed(&mut self, ty: &Type) -> bool {
        match ty {
            Type::Exist(alpha) => self.ctx.contains(&Element::Exist(*alpha)) || self.find_solved(*alpha).is_some(),
            Type::Univ(k, alpha) => self.with_scope(Element::Var(*k.clone()), |f| f.well_formed(&alpha)),
            Type::Var(idx) => self.find_type_var(*idx).is_some(),
            Type::Arrow(a, b) => self.well_formed(&a) && self.well_formed(&b),
            Type::Sum(a, b) => self.well_formed(&a) && self.well_formed(&b),
            Type::Product(a, b) => self.well_formed(&a) && self.well_formed(&b),
            Type::Abs(k, a) => self.with_scope(Element::Var(*k.clone()), |f| f.well_formed(&a)),
            Type::App(a, b) => self.well_formed(&a) && self.well_formed(&b),
            Type::Unit | Type::Int | Type::Bool => true,
        }
    }

    fn check_wf(&mut self, ty: &Type) -> Result<bool, String> {
        if self.well_formed(ty) {
            Ok(true)
        } else {
            Err(format!("Type {:?} is not well formed!", ty))
        }
    }

    // Pop off any stack growth incurred from calling `f`
    fn with_scope<T, F: FnMut(&mut Context) -> T>(&mut self, e: Element, mut f: F) -> T {
        self.ctx.push(e.clone());
        let t = f(self);

        while let Some(elem) = self.ctx.pop() {
            if elem == e {
                break;
            }
        }
        t
    }

    /// Apply the context to a type, replacing any solved existential variables
    /// in the context onto the type, if it contains a matching existential
    fn apply(&self, ty: Type) -> Type {
        match ty {
            Type::Unit | Type::Int | Type::Bool | Type::Var(_) => ty,
            Type::Arrow(a, b) => Type::Arrow(Box::new(self.apply(*a)), Box::new(self.apply(*b))),
            Type::Sum(a, b) => Type::Sum(Box::new(self.apply(*a)), Box::new(self.apply(*b))),
            Type::Product(a, b) => Type::Product(Box::new(self.apply(*a)), Box::new(self.apply(*b))),
            Type::Abs(k, a) => Type::Abs(k, Box::new(self.apply(*a))),
            Type::App(a, b) => Type::App(Box::new(self.apply(*a)), Box::new(self.apply(*b))),
            Type::Univ(k, ty) => Type::Univ(k, Box::new(self.apply(*ty))),
            Type::Exist(n) => {
                match self.find_solved(n) {
                    // Apply to the solved variable also - this is important
                    // since we can have solved references deeper in the stack
                    Some(solved) => self.apply(solved.clone()),
                    None => ty,
                }
            }
        }
    }

    /// Find the term annotation corresponding to de Bruijn index `idx`.
    /// We traverse the stack in a reversed order, counting each annotation
    /// we come across
    fn find_annotation(&self, idx: usize) -> Option<&Type> {
        let mut ix = 0;
        for elem in self.ctx.iter().rev() {
            match &elem {
                Element::Ann(ty) => {
                    if ix == idx {
                        return Some(&ty);
                    }
                    ix += 1
                }
                _ => {}
            }
        }

        None
    }

    /// Find the term annotation corresponding to de Bruijn index `idx`.
    /// We traverse the stack in a reversed order, counting each annotation
    /// we come across
    fn find_type_var(&self, idx: usize) -> Option<&Kind> {
        let mut ix = 0;
        for elem in self.ctx.iter().rev() {
            match &elem {
                Element::Var(k) => {
                    if ix == idx {
                        return Some(&k);
                    }
                    ix += 1
                }
                _ => {}
            }
        }
        None
    }

    /// Find the monotype associated with a solved existential variable `alpha`
    /// in the context, if it exists.
    fn find_solved(&self, alpha: usize) -> Option<&Type> {
        for elem in &self.ctx {
            match &elem {
                Element::Solved(n, ty) if *n == alpha => return Some(ty),
                _ => {}
            }
        }
        None
    }

    /// This is one of the more confusing parts of the paper, IMO. We have to open
    /// a 'hole' in the context, where we can replace/insert some arbitrary amount
    /// of bindings where an unsolved existential (or marker, in the paper) was
    /// previously located
    fn splice_hole<F: Fn(&mut Vec<Element>)>(&mut self, exist: usize, f: F) -> Result<(), String> {
        let (l, r) = self.split_context(exist)?;
        f(&mut l.ctx);
        l.ctx.extend(r);
        Ok(())
    }

    fn split_context(&mut self, exist: usize) -> Result<(&mut Self, Vec<Element>), String> {
        let mut ret = None;
        for (idx, el) in self.ctx.iter().enumerate() {
            match el {
                Element::Exist(n) if *n == exist => ret = Some(idx),
                _ => {}
            }
        }
        let idx = ret.ok_or_else(|| format!("{} not bound in ctx", exist))?;
        let rest = self.ctx.split_off(idx + 1);
        self.ctx.pop();
        Ok((self, rest))
    }

    fn kinding(&mut self, ty: &Type) -> Option<Kind> {
        Some(Kind::Star)
    }

    fn beta_reduce(&mut self, ty: &mut Type) -> Result<(), String> {
        match ty {
            Type::Unit | Type::Int | Type::Bool | Type::Var(_) => Ok(()),
            Type::Exist(v) => Ok(()),
            Type::Arrow(a, b) => {
                self.beta_reduce(a)?;
                self.beta_reduce(b)
            }
            Type::Sum(a, b) => {
                self.beta_reduce(a)?;
                self.beta_reduce(b)
            }
            Type::Product(a, b) => {
                self.beta_reduce(a)?;
                self.beta_reduce(b)
            }
            Type::Univ(k, a) => self.with_scope(Element::Var(*k.clone()), |f| f.beta_reduce(a)),
            Type::Abs(k, a) => self.with_scope(Element::Var(*k.clone()), |f| f.beta_reduce(a)),
            Type::App(a, b) => {
                self.beta_reduce(a)?;
                self.beta_reduce(b)?;
                if let Type::Abs(k, body) = a.as_mut() {
                    match self.kinding(b) {
                        Some(arg_kind) => {
                            if &arg_kind == k.as_ref() {
                                body.subst(b);
                                *ty = *body.clone();
                                self.beta_reduce(ty)
                            } else {
                                Err(format!("kind mismatch"))
                            }
                        }
                        None => Err(format!("No kind!?")),
                    }
                } else {
                    Ok(())
                }
            }
        }
    }

    fn subtype(&mut self, mut a: Type, mut b: Type) -> Result<(), String> {
        println!("{:?} <: {:?}", a, b);
        self.beta_reduce(&mut a)?;
        self.beta_reduce(&mut b)?;
        println!("{:?} <: {:?}", a, b);
        use Type::*;
        match (a, b) {
            (Bool, Bool) => Ok(()),
            (Int, Int) => Ok(()),
            // Rule <: Unit
            (Unit, Unit) => Ok(()),
            // Rule <: Var
            (Var(a), Var(b)) if a == b => Ok(()),
            // Rule <: Exvar
            (Exist(a), Exist(b)) if a == b => Ok(()),
            // Rule <: ->
            (Arrow(a1, a2), Arrow(b1, b2)) => {
                self.subtype(*b1, *a1)?;
                self.subtype(self.apply(*a2), self.apply(*b2))
            }
            (Sum(l1, r1), Sum(l2, r2)) => {
                self.subtype(*l1, *l2)?;
                self.subtype(self.apply(*r1), self.apply(*r2))
            }
            (Product(l1, r1), Product(l2, r2)) => {
                self.subtype(*l1, *l2)?;
                self.subtype(self.apply(*r1), self.apply(*r2))
            }
            // Rule <: forall. L
            (Univ(k, a), b) => {
                let alpha = self.fresh_ev();
                let mut a_ = *a;
                a_.subst(&mut b.clone());
                self.with_scope(Element::Marker(alpha), |f| {
                    f.ctx.push(Element::Exist(alpha));
                    f.subtype(a_.clone(), b.clone())
                })
            }
            // Rule <: forall. R
            (a, Univ(k, b)) => {
                // let alpha = self.fresh_ev();
                self.with_scope(Element::Var(*k.clone()), |f| f.subtype(a.clone(), *b.clone()))
            }
            // Rule <: InstantiateL
            (Exist(alpha), a) if !a.freevars().contains(&alpha) => self.instantiateL(alpha, &a),
            // Rule <: InstantiateR
            (a, Exist(alpha)) if !a.freevars().contains(&alpha) => self.instantiateR(&a, alpha),
            (a, b) => Err(format!("{:?} is not a subtype of {:?}", a, b)),
        }
    }

    fn instantiateL(&mut self, alpha: usize, a: &Type) -> Result<(), String> {
        // We need to split our context into Γ1, alpha, Γ2 so that we
        // can ensure that alpha is a well formed Existenial in Γ1, e.g.
        // that alpha appears in Γ1. This ensures that alpha is declared
        // "to the left" (outer scope) of the type `a`
        let (l, r) = self.split_context(alpha)?;
        if a.monotype() && l.well_formed(a) {
            l.ctx.push(Element::Solved(alpha, a.clone()));
            l.ctx.extend(r);
            return Ok(());
        }

        // Okay, alpha is *not* well-formed, but that's okay. `split_context`
        // removed alpha from the context, so we add it back and then reform
        // Γ1, alpha, Γ2 into a full context again. When we add it back
        // and reform the context depends on how we dispatch below
        match a {
            // InstLArr
            Type::Arrow(A1, A2) => {
                let a1 = l.fresh_ev();
                let a2 = l.fresh_ev();

                // Rather than reforming, then calling splice, we can just
                // directly push to `l`, since it currently points at the
                // hole corresponding to [^]
                l.ctx.push(Element::Exist(a2));
                l.ctx.push(Element::Exist(a1));
                l.ctx.push(Element::Solved(
                    alpha,
                    Type::Arrow(Box::new(Type::Exist(a1)), Box::new(Type::Exist(a2))),
                ));
                l.ctx.extend(r);
                self.instantiateR(A1, a1)?;
                let A2_ = self.apply(*A2.clone());
                self.instantiateL(a2, &A2_)
            }
            // InstLAllR
            Type::Univ(k, beta) => {
                l.ctx.push(Element::Exist(alpha));
                l.ctx.extend(r);
                self.with_scope(Element::Var(*k.clone()), |f| {
                    f.instantiateL(alpha, &Type::Univ(k.clone(), beta.clone()))
                })
            }
            // InstLReach
            Type::Exist(beta) => {
                // We need to ensure that beta only appears to the right of alpha,
                // e.g. that beta is well-formed in Γ2, so we make a temporary
                // context so that we can call the splice_hole method
                let mut gamma = Context { ctx: r, ev: 0 };
                gamma.splice_hole(*beta, |ctx| ctx.push(Element::Solved(*beta, Type::Exist(alpha))))?;
                // As explained above, Exist(alpha) was popped off of `l` in the
                // `split_context` method, so we need to add it back in. We
                // now have some context such that Γ[alpha][beta=alpha]
                l.ctx.push(Element::Exist(alpha));
                l.ctx.extend(gamma.ctx);
                Ok(())
            }
            _ => Err(format!("Could not instantiate Exist({}) to {:?}", alpha, a)),
        }
    }

    fn instantiateR(&mut self, a: &Type, alpha: usize) -> Result<(), String> {
        let (l, r) = self.split_context(alpha)?;
        if a.monotype() && l.well_formed(a) {
            l.ctx.push(Element::Solved(alpha, a.clone()));
            l.ctx.extend(r);
            return Ok(());
        }
        match a {
            // InstRArr
            Type::Arrow(A1, A2) => {
                let a1 = l.fresh_ev();
                let a2 = l.fresh_ev();

                l.ctx.push(Element::Exist(a2));
                l.ctx.push(Element::Exist(a1));
                l.ctx.push(Element::Solved(
                    alpha,
                    Type::Arrow(Box::new(Type::Exist(a1)), Box::new(Type::Exist(a2))),
                ));
                l.ctx.extend(r);

                // Much the same as InstLArr, except the following lines are swapped
                self.instantiateL(a1, &A1)?;
                let A2_ = self.apply(*A2.clone());
                self.instantiateR(&A2_, a2)
            }
            // InstRAllL
            Type::Univ(k, beta) => {
                l.ctx.push(Element::Exist(alpha));
                l.ctx.extend(r);

                let b = self.fresh_ev();

                let mut beta_prime = *beta.clone();
                beta_prime.subst(&mut Type::Exist(b));

                self.with_scope(Element::Exist(b), |f| {
                    f.instantiateR(&Type::Univ(k.clone(), Box::new(beta_prime.clone())), alpha)
                })
            }
            // InstRReach
            Type::Exist(beta) => {
                let mut gamma = Context { ctx: r, ev: 0 };
                gamma.splice_hole(*beta, |ctx| ctx.push(Element::Solved(*beta, Type::Exist(alpha))))?;
                l.ctx.push(Element::Exist(alpha));
                l.ctx.extend(gamma.ctx);
                Ok(())
            }
            _ => Err(format!("Could not instantiate Exist({}) to {:?}", alpha, a)),
        }
    }

    fn infer(&mut self, e: &Expr) -> Result<Type, String> {
        match e {
            Expr::True | Expr::False => Ok(Type::Bool),
            Expr::Int(_) => Ok(Type::Int),
            // Rule 1l=>
            Expr::Unit => Ok(Type::Unit),
            // Rule Anno
            Expr::Ann(x, ty) => {
                self.check_wf(ty)?;
                self.check(x, ty)?;
                Ok(*ty.clone())
            }
            // Rule Var
            Expr::Var(x) => self.find_annotation(*x).cloned().ok_or(format!("unbound db {:?}", x)),
            // Rule ->I =>
            Expr::Abs(e) => {
                let alpha = self.fresh_ev();
                let beta = self.fresh_ev();
                // Fresh existential var for function domain
                self.ctx.push(Element::Exist(alpha));
                // And for codomain
                self.ctx.push(Element::Exist(beta));

                // Check the function body against Beta
                self.with_scope(Element::Ann(Type::Exist(alpha)), |f| f.check(e, &Type::Exist(beta)))?;

                // alpha and beta stay on the stack, since they appear in the output type
                Ok(Type::Arrow(Box::new(Type::Exist(alpha)), Box::new(Type::Exist(beta))))
            }
            // Rule ->E
            Expr::App(e1, e2) => {
                let a = self.infer(&e1)?;
                let a = self.apply(a);
                println!("{:?} {:?} {:?} {:?}", a, &e1, &e2, &self.ctx);
                self.infer_app(&a, e2)
            }
            Expr::If(e1, e2, e3) => {
                self.check(e1, &Type::Bool)?;
                let alpha = self.fresh_ev();
                self.ctx.push(Element::Exist(alpha));

                let exist = Type::Exist(alpha);
                self.check(&e2, &exist)?;
                self.check(&e3, &exist)?;
                Ok(exist)
            }
            Expr::Inj(lr, e, ty) => {
                self.check_wf(ty)?;
                let mut ty = self.apply(*ty.clone());
                self.beta_reduce(&mut ty)?;

                match &ty {
                    Type::Sum(l, r) => {
                        match lr {
                            LR::Left => self.check(e, l)?,
                            LR::Right => self.check(e, r)?,
                        }
                        Ok(ty.clone())
                    }
                    Type::Abs(_, _) => {
                        let arg_ty = self.infer(e)?;
                        self.infer(&Expr::Inj(
                            *lr,
                            e.clone(),
                            Box::new(Type::App(Box::new(ty.clone()), Box::new(arg_ty))),
                        ))
                    }
                    _ => Err(format!("#Expr::Inj {:?} is not a sum type!", ty)),
                }
            }
            Expr::Case(scrutinee, la, ra) => {
                let ty = self.infer(scrutinee)?;
                let ty = self.apply(ty);
                if let Type::Sum(left, right) = &ty {
                    self.check(&la.pat, &ty)?;
                    self.check(&ra.pat, &ty)?;

                    fn bind_infer(ctx: &mut Context, arm: &Arm, left: &Type, right: &Type) -> Result<Type, String> {
                        match arm.pat.as_ref() {
                            Expr::Inj(lr, ex, _) => match (ex.as_ref(), lr) {
                                (Expr::Var(_), LR::Left) => {
                                    ctx.with_scope(Element::Ann(left.clone()), |f| f.infer(&arm.expr))
                                }
                                (Expr::Var(_), LR::Right) => {
                                    ctx.with_scope(Element::Ann(right.clone()), |f| f.infer(&arm.expr))
                                }
                                _ => ctx.infer(&arm.expr),
                            },
                            _ => Err(format!("Not injection expressions!")),
                        }
                    }

                    let l = bind_infer(self, la, &left, &right)?;
                    let r = bind_infer(self, ra, &left, &right)?;
                    if l == r {
                        Ok(l)
                    } else {
                        Err(format!("Case arms have different return types!"))
                    }
                } else {
                    Err(format!("{:?} is not a sum type!", ty))
                }
            }
            Expr::Pair(a, b) => {
                let ta = self.infer(a)?;
                let ta = self.apply(ta);
                let tb = self.infer(b)?;
                let tb = self.apply(tb);
                Ok(Type::Product(Box::new(ta), Box::new(tb)))
            }
            Expr::Proj(lr, ex) => {
                let ty = self.infer(ex)?;
                let ty = self.apply(ty);
                match ty {
                    Type::Product(left, right) => match lr {
                        LR::Left => Ok(*left),
                        LR::Right => Ok(*right),
                    },
                    _ => Err(format!("{:?} is not a pair!", ex)),
                }
            } // _ => panic!("cant infer {:?}", e)
        }
    }

    fn infer_app(&mut self, ty: &Type, e2: &Expr) -> Result<Type, String> {
        match ty {
            // Rule alpha_hat App
            Type::Exist(alpha) => {
                let a1 = self.fresh_ev();
                let a2 = self.fresh_ev();

                self.splice_hole(*alpha, |ctx| {
                    ctx.push(Element::Exist(a2));
                    ctx.push(Element::Exist(a1));
                    ctx.push(Element::Solved(
                        *alpha,
                        Type::Arrow(Box::new(Type::Exist(a1)), Box::new(Type::Exist(a2))),
                    ));
                })?;

                self.check(e2, &Type::Exist(a1))?;
                Ok(Type::Exist(a2))
            }
            // Rule ->App
            Type::Arrow(a, b) => {
                self.check(e2, a)?;
                Ok(*b.clone())
            }
            // Rule forall. App
            Type::Univ(k, a) => {
                let alpha = self.fresh_ev();
                let mut a_prime = a.clone();
                a_prime.subst(&mut Type::Exist(alpha));
                self.ctx.push(Element::Exist(alpha));
                self.infer_app(&a_prime, e2)
            }
            _ => Err(format!("Cannot appl ty {:?} to expr {:?}", e2, ty)),
        }
    }

    fn check(&mut self, e: &Expr, a: &Type) -> Result<(), String> {
        match (e, a) {
            (Expr::Int(_), Type::Int) => Ok(()),
            (Expr::False, Type::Bool) => Ok(()),
            (Expr::True, Type::Bool) => Ok(()),
            (Expr::If(e1, e2, e3), a) => {
                self.check(&e1, &Type::Bool)?;
                self.check(&e2, a)?;
                self.check(&e3, a)
            }
            (Expr::Inj(lr, ex, tagged), Type::Sum(left, right)) => {
                self.subtype(*tagged.clone(), a.clone())?;
                match lr {
                    LR::Left => self.check(ex, left),
                    LR::Right => self.check(ex, right),
                }
            }
            (Expr::Pair(a, b), Type::Product(t1, t2)) => {
                self.check(a, t1)?;
                self.check(b, t2)
            }
            // (Expr::Proj(lr, expr), t) => {
            //     let ty = self.infer(expr)?;
            //     let ty = self.apply(ty);
            //     dbg!(&self.ctx);
            //     match (ty, lr) {
            //         (Type::Product(left, _), LR::Left) => self.subtype(&left, t),
            //         (Type::Product(right, _), LR::Right) => self.subtype(&right, t),
            //         _ => Err(format!("{:?} is not a product type", t))
            //     }
            // }
            // Rule 1l
            (Expr::Unit, Type::Unit) => Ok(()),
            // Rule ->I
            (Expr::Abs(body), Type::Arrow(a1, a2)) => self.with_scope(Element::Ann(*a1.clone()), |f| f.check(body, a2)),
            // Rule forall. I
            (e, Type::Univ(k, ty)) => self.with_scope(Element::Var(*k.clone()), |f| f.check(e, &ty)),
            // Rule Sub
            (e, b) => {
                // let mut a = a.clone();
                // let mut b = b.clone();

                // self.beta_reduce(&mut a)?;
                // self.beta_reduce(&mut b)?;

                let a = self.infer(e)?;
                let a = self.apply(a);
                let b = self.apply(b.clone());
                dbg!(&e, &self.ctx);
                self.subtype(a, b)?;
                Ok(())
            }
        }
    }
}

fn infer(ex: &Expr) -> Result<Type, String> {
    let mut ctx = Context::default();
    let inf = ctx.infer(ex)?;
    let mut ty = ctx.apply(inf);
    ctx.beta_reduce(&mut ty)?;
    Ok(ty)
}

fn main() {
    // \x. (x 1, x True) : forall A. (A -> A) -> (Int, Bool)
    let h = abs!(pair!(app!(var!(0), Expr::Int(1)), app!(var!(0), Expr::True)));
    let h = ann!(
        h,
        arrow!(
            forall!(arrow!(Type::Var(0), Type::Var(0))),
            product!(Type::Int, Type::Bool)
        )
    );
    let g = app!(h, abs!(var!(0)));

    let ty = Type::Abs(Box::new(Kind::Star), Box::new(Type::Var(0)));
    let ty = Type::App(Box::new(ty), Box::new(Type::Int));
    let f = ann!(Expr::Int(99), ty);

    let ty_opt = Type::Abs(Box::new(Kind::Star), Box::new(sum!(Type::Var(0), Type::Unit)));
    // \x. inl x of [\X::* => X + ()] @ 'A
    let some = abs!(inj!(l; var!(0), ty_opt.clone()));
    let some = ann!(
        some,
        forall!(arrow!(
            Type::Var(0),
            Type::App(Box::new(ty_opt.clone()), Box::new(Type::Var(0)))
        ))
    );

    // let some = app!(some, Expr::Int(10));

    println!("{} : {:?}", some, infer(&some));
}

#[cfg(test)]
mod test {
    use super::*;
    use helpers::*;

    #[test]
    fn identity() {
        let id = abs!(var!(0));
        let id_ann = ann!(id.clone(), forall!(arrow!(Type::Var(0), Type::Var(0))));

        let id_ex = arrow!(Type::Exist(0), Type::Exist(0));
        let id_ann_ex = forall!(arrow!(Type::Var(0), Type::Var(0)));
        assert_eq!(infer(&id), Ok(id_ex));
        assert_eq!(infer(&id_ann), Ok(id_ann_ex));
    }

    #[test]
    fn application() {
        // \f. \g. \x. f (g x)
        // x: C
        // g: C -> A
        // f: A -> B
        // (A -> B) -> (C -> A) -> C -> B
        let t = abs!(abs!(abs!(app!(var!(2), app!(var!(1), var!(0))))));
        let ty = infer(&t).unwrap();
        assert_eq!(ty.to_string(), "((a->b)->((c->a)->(c->b)))");
    }

    #[test]
    fn application2() {
        // \x. if x then 1 else 0 : Bool -> Int
        let f = abs!(ife!(var!(0), Expr::Int(1), Expr::Int(0)));

        // \f. \g. \x. f (g x)
        // : (e6 -> e7) -> ((e14 -> e6) -> (e14 -> e7))
        // : (a -> b) -> ((c -> a) -> (c -> b))
        let t1 = abs!(abs!(abs!(app!(var!(2), app!(var!(1), var!(0))))));

        // : ((c -> bool) -> (c -> int))
        let f2 = app!(t1.clone(), f);
        // : (c -> int)
        let f3 = app!(f2, abs!(Expr::True));
        let f4 = app!(f3, Expr::Unit);

        assert_eq!(infer(&f4), Ok(Type::Int))
    }

    #[test]
    fn sum_type() {
        let ty = sum!(Type::Unit, Type::Bool);
        let f_ty = arrow!(ty.clone(), Type::Int);

        // \x. case x of inl () => 0 | inr True => 1
        let f = abs!(
            case!(var!(0), inj!(l; Expr::Unit, ty.clone()) => Expr::Int(0), inj!(r; Expr::True, ty.clone()) => Expr::Int(1))
        );
        let f = ann!(f, f_ty);

        infer(&f).unwrap();

        let a = arrow!(forall!(arrow!(Type::Var(0), Type::Var(0))), ty.clone());
        // \x. if True then inl (x ()) else inr (x False) : (forall A. (A -> A)) -> (() + Bool)
        let f = abs!(ife!(
            Expr::True,
            inj!(l; app!(var!(0), Expr::Unit), ty.clone()),
            inj!(r; app!(var!(0), Expr::False), ty.clone())
        ));
        let f = ann!(f, a);

        infer(&f).unwrap();
    }

    #[test]
    fn product_type() {
        // \x. r.0 : forall A. (A, A) -> A
        let f = ann!(
            abs!(proj!(r; var!(0))),
            forall!(arrow!(product!(Type::Var(0), Type::Var(0)), Type::Var(0)))
        );
        // (\x. fst 0) (1, 1)
        let f = app!(f, pair!(Expr::Int(1), Expr::Int(1)));

        assert_eq!(infer(&f), Ok(Type::Int));

        // \x. (x 1, x True) : (forall A. (A -> A)) -> (Int, Bool)
        let h = abs!(pair!(app!(var!(0), Expr::Int(1)), app!(var!(0), Expr::True)));
        let h = ann!(
            h,
            arrow!(
                forall!(arrow!(Type::Var(0), Type::Var(0))),
                product!(Type::Int, Type::Bool)
            )
        );
        let g = app!(h, abs!(var!(0)));
        assert_eq!(infer(&g), Ok(product!(Type::Int, Type::Bool)))
    }
}
