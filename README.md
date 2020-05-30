# types-and-programming-languages

![](https://github.com/lazear/types-and-programming-languages/workflows/Rust/badge.svg)

Several Rust implementations of exercises from Benjamin Pierce's "Types and Programming Languages" are organized into different folders, as described below:

- `lambda` is an implementation of the untyped lambda calculus as presented in chapter 5, 6, and 7.

- `arith` is an implementation of the untyped lambda calculus extended with simple numeric operations

- `typedarith` is the `arith` project extended with simple types: `Nat` and `Bool`

- `stlc` is an implementation of the simply typed lambda calculus, as discussed in chapters 9 and 10 of TAPL. This simply typed calculus has the types, `Unit`, `Nat`, `Bool`, `T -> T` (arrow), and `Record` types.

- `system_f` contains a parser, typechecker, and evaluator for the simply typed lambda calculus with parametric polymorphism (System F). 

The implementation of System F is the most complete so far, and I've tried to write a parser, typechecker and diagnostic system that can given meaningful messages, such as:

```
type Var = { A | B Nat | C Nat }
let func = (\x: Var. case x of | A => 0 | B y => succ x | C y => pred x) in func C 2 of Var
                                             ^~~~^ --- abstraction requires type Nat
                                                  ^^ --- but it is applied to type "A: Unit | B: Nat | C: Nat"
```

- `system_fw` contains a parser for a high-level, Standard ML like source language that is desugared into an HIR, and then System F-omega. This extends `system_f` with type operators and higher-kinded types. This is where most of the ongoing work is located, as I'd like to make this the basis of a toy (but powerful, and useable) programming language. Ideally we will have some form of bidirectional type inference

- `dependent` is WIP, implementing a simple, dependently typed lambda calculus as discussed in ATAPL.

- `bidir` is WIP, implementing the bidirectional typechecker from 'Complete and Easy Bidirectional Typechecking'