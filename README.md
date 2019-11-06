# types-and-programming-languages

![](https://github.com/lazear/types-and-programming-languages/workflows/rust.yml/badge.svg)

Several Rust implementations of exercises from Benjamin Pierce's "Types and Programming Languages" are organized into different folders, as described below:

- `lambda` is an implementation of the untyped lambda calculus as presented in chapter 5, 6, and 7.
- `typedarith` is the `arith` project extended with simple types: `Nat` and `Bool`
- `stlc` is an implementation of the simply typed lambda calculus, as discussed in chapters 9 and 10 of TAPL. This simply typed calculus has the types, `Unit`, `Nat`, `Bool`, `T -> T` (arrow), and `Record` types.
- `system_f` contains a parser, typechecker, and evaluator for the simply typed lambda calculus with parametric polymorphism (System F). 
