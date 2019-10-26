# types-and-programming-languages

Rust implementations of exercises from Benjamin Pierce's "Types and Programming Languages"

The `lambda` folder is an implementation of the untyped lambda calculus as presented in chapter 5, 6, and 7.

`typedarith` is the `arith` project extended with simple types: `Nat` and `Bool`

The `stlc` folder represents an implementation of the simply typed lambda calculus, as discussed in chapters 9 and 10 of TAPL. 
This simply typed calculus has two types, `Bool` and `T -> T` (arrow), and uses De Bruijn indices for nameless representation of terms