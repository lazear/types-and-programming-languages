# types-and-programming-languages

![](https://github.com/lazear/types-and-programming-languages/workflows/Rust/badge.svg)

Several Rust implementations of exercises from Benjamin Pierce's "Types and Programming Languages" are organized into different folders, as described below:

- `arith` is an implementation of the untyped lambda calculus extended with simple numeric operations

- `lambda` is an implementation of the untyped lambda calculus as presented in chapter 5, 6, and 7.

- `typedarith` is the `arith` project extended with simple types: `Nat` and `Bool`

- `stlc` is an implementation of the simply typed lambda calculus, as discussed in chapters 9 and 10 of TAPL. This simply typed calculus has the types, `Unit`, `Nat`, `Bool`, `T -> T` (arrow), and `Record` types.

- `recon` contains several implementations of Hindley-Milner based type reconstruction from the untyped lambda calculus to System F, with let-polymorphism. Both Algorithm W (the more common) and Algorithm J (the more efficient) are presented. For Alg. W, both a naive equality constraint solver, and a faster union-find (with path compression) solver are provided. Algorithm J makes use shared mutable references to promote type sharing instead.

- `system_f` contains a parser, typechecker, and evaluator for the simply typed lambda calculus with parametric polymorphism (System F). The implementation of System F is the most complete so far, and I've tried to write a parser, typechecker and diagnostic system that can given meaningful messages

- `system_fw` contains a parser for a high-level, Standard ML like source language that is desugared into an HIR, and then System F-omega. This extends `system_f` with type operators and higher-kinded types. This is where most of the ongoing work is located, as I'd like to make this the basis of a toy (but powerful, and useable) programming language. Ideally we will have some form of bidirectional type inference. Work on this has accidentally turned into a full fledged [SML compiler](https://github.com/SomewhatML/sml-compiler), so it's likely that I will roll back the work on the system_fw project to just type checking

- `bidir` is is an implementation of the bidirectional typechecker from 'Complete and Easy Bidirectional Typechecking', extended with booleans, product, and sum types. I make no claims on the correctness of the implementation for the extended features not present in the paper.

- `dependent` is WIP, implementing a simple, dependently typed lambda calculus as discussed in ATAPL.

