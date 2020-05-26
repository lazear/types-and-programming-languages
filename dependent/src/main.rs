#[derive(Debug, Clone, PartialEq)]
enum Term {
    Universe(usize),
    Nat,
    Var(usize),
    Int(usize),
    App(Box<Term>, Box<Term>),
    Abs(Box<Term>, Box<Term>),
    Pi(Box<Term>, Box<Term>),
}

impl Term {
    fn normal(&self) -> bool {
        match self {
            Term::App(_, _) => false,
            Term::Pi(a, b) | Term::Abs(a, b) => a.normal() && b.normal(),
            _ => true,
        }
    }

    fn whnf(&self) -> bool {
        match self {
            Term::App(_, _) => false,
            Term::Pi(a, _) | Term::Abs(a, _) => a.normal(),
            _ => true,
        }
    }

    fn subst(&mut self, mut t2: Term) {
        println!("subst {:?} {:?}", self, t2);
        let mut v = Visitor::new();

        fn shift(v: &mut Visitor, t: &mut Term, s: isize) {
            v.visit(t, &|f, c| {
                if let Term::Var(n) = f {
                    if *n >= c {
                        *n = (*n as isize + s) as usize;
                    }
                }
            });
        }

        shift(&mut v, &mut t2, 1);
        v.visit(self, &|f, i| {
            let mut t = t2.clone();
            let mut v = Visitor::new();
            v.visit(&mut t, &|f, _| {
                if let Term::Var(n) = f {
                    *n += i;
                }
            });
            if let Term::Var(n) = f {
                if *n == i {
                    *f = t;
                }
            }
        });
        shift(&mut v, self, -1);
    }
}

#[derive(Default, Debug, Clone)]
struct Context {
    binding: Vec<Term>,
}

#[derive(Debug, Clone)]
enum Error {
    Unbound,
    NotPi(Term),
    Mismatch(Term, Term),
}

struct Visitor {
    cutoff: usize,
}

impl Visitor {
    fn new() -> Visitor {
        Visitor { cutoff: 0 }
    }

    fn visit<F: Fn(&mut Term, usize)>(&mut self, term: &mut Term, f: &F) {
        match term {
            Term::Universe(_) => {}
            Term::Nat | Term::Int(_) => {}
            Term::Var(_) => {
                f(term, self.cutoff);
            }
            Term::Pi(t1, t2) | Term::Abs(t1, t2) => {
                self.visit(t1, f);
                self.cutoff += 1;
                self.visit(t2, f);
                self.cutoff -= 1;
            }
            Term::App(t1, t2) => {
                self.visit(t1, f);
                self.visit(t2, f);
            }
        }
    }
}

impl Context {
    fn get(&self, idx: usize) -> Option<&Term> {
        self.binding.get(self.binding.len().checked_sub(idx + 1)?)
    }

    fn with_bind<T, F: Fn(&mut Context) -> T>(&mut self, bind: Term, f: F) -> T {
        self.binding.push(bind);
        let r = f(self);
        self.binding.pop();
        r
    }

    fn equiv(&mut self, t1: &Term, t2: &Term) -> bool {
        let mut t1p = t1.clone();
        let mut t2p = t2.clone();

        while !t1p.normal() {
            t1p = beta_reduce(t1p);
        }

        while !t2p.normal() {
            t2p = beta_reduce(t2p);
        }

        t1p == t2p
    }

    fn type_of(&mut self, term: &Term) -> Result<Term, Error> {
        println!("type of {:?}", term);
        match &term {
            Term::Universe(n) => Ok(Term::Universe(*n + 1)),
            Term::Var(i) => self.get(*i).cloned().ok_or(Error::Unbound),
            Term::Abs(S, t) => {
                let k = self.type_of(&S)?;
                let T = self.with_bind(k.clone(), |f| f.type_of(&t))?;
                Ok(Term::Pi(Box::new(k), Box::new(T)))
            }
            Term::App(t1, t2) => {
                let ty1 = self.type_of(&t1)?;
                let ty2 = self.type_of(&t2)?;
                match ty1 {
                    Term::Pi(S, mut T) => {
                        if self.equiv(&S, &ty2) {
                            T.subst(*t2.clone());
                            Ok(*T)
                        } else {
                            Err(Error::Mismatch(*S, ty2))
                        }
                    }
                    _ => Err(Error::NotPi(ty1)),
                }
            }
            Term::Pi(t1, t2) => {
                let ty1 = self.type_of(&t1)?;
                let ty2 = self.with_bind(ty1.clone(), |f| f.type_of(t2))?;
                Ok(ty2)
            }
            Term::Nat => Ok(Term::Universe(0)),
            Term::Int(_) => Ok(Term::Nat),
        }
    }
}

/// Small step beta reduction
fn beta_reduce(mut term: Term) -> Term {
    match term {
        Term::App(mut abs, arg) => match (abs.normal(), arg.normal()) {
            (false, _) => Term::App(Box::new(beta_reduce(*abs)), arg),
            (_, false) => Term::App(abs, Box::new(beta_reduce(*arg))),
            _ => match *abs {
                Term::Abs(_, mut body) => {
                    body.subst(*arg.clone());
                    *body
                }
                x => Term::App(Box::new(x), arg),
            },
        },
        Term::Abs(ty, body) => {
            if body.normal() {
                Term::Abs(ty, body)
            } else {
                Term::Abs(ty, Box::new(beta_reduce(*body)))
            }
        }
        _ => term,
    }
}

fn main() {
    macro_rules! term {
        (Abs; $ex1:expr, $ex2:expr) => {
            Term::Abs(Box::new($ex1), Box::new($ex2))
        };
        (App; $ex1:expr, $ex2:expr) => {
            Term::App(Box::new($ex1), Box::new($ex2))
        };
        (Pi; $ex1:expr, $ex2:expr) => {
            Term::Pi(Box::new($ex1), Box::new($ex2))
        };
        (Var; $ex1:expr) => {
            Term::Var($ex1)
        };
        (Star) => {
            Term::Universe(0)
        };
        (Universe; $ex:expr) => {
            Term::Universe($ex)
        };
        (Int; $ex1:expr) => {
            Term::Int($ex1)
        };
    };

    println!("Hello, world!");
    let mut ctx = Context::default();
    // let tm = term!(Abs; term!(Star), term!(App; Term::Nat, term!(Var; 0)));
    // let tm = term!(App; tm, term!(Int; 10));

    // Πx: Nat -> x
    let mut tm = term!(Abs; Term::Nat, Term::Var(0));
    tm = term!(App; tm, Term::Int(10));
    // tm.subst(Term::Int(10));

    dbg!(ctx.type_of(&tm));
    // dbg!(&tm);
    tm = beta_reduce(tm);
    dbg!(&tm);
}
