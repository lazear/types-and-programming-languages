# Parser

We use a handwritten recursive descent parser. In general, there is a top-level entry function for parsing of `types`, `expressions`, `declarations`, and `specifications`. Each of these functions attempts to match against the current token, without popping it. If a suitable match is found, then we dispatch to a function specifically for parsing that token, which may then pop off the current token. Once the current token has been popped, we actually begin to return real errors. Errors that occur before the current token has been popped may be generally ignored.

### Grammar

```
number  ::=     [0-9]+

const   ::=     ()
                true
                false
                number

var     ::=    [a-zA-Z]+
tvar    ::=     'var
tvars   ::=     tvar | (tvar, ... tvarN)
id      ::=     var
path    ::=     id | id.path


annotate ::=    : sig
                :> sig

exp     ::=     const 
                path
                exp @ty
                exp exp
                exp path exp
                ( exp )
                ( exp, ..., expN )
                { id = exp, ... }
                let decl in exp end
                fn pat => exp
                if exp then exp else exp
                case exp of match
                struct

struct  ::=     struct [decl]+ end <annotate>
                path
                path struct

match   ::=     <|> pat => expr < | match>
```

### Declarations 
Declarations are values or types that can be referenced by a `path`

```
decl    ::=     val <tvars> pat = exp
                fun <tvars> func
                type <tvars> id = ty
                datatype <tvars> id = con
                signature id = sig
                structure id <annoate> = struct
                open path
                infix path

func    ::=     id pat <: ty> = exp <| func >
con     ::=     id <of ty> <| condbin>
```

### Specifications

```
sig     ::=     sig [spec+] end

spec    ::=     val id : ty
                type <tvars> id
                type <tvars> id = ty
                datatype <tvars> id = con
                include path

kind    ::=     *
                kind -> kind

ty      ::=     path
                tvar
                ty * ty
                ty -> ty
                ty ty
                { lab: ty, ..., labN: tyN }
                (ty, ..., ty)
                rec ty
                forall (tvar :: kind) => ty
                exists (tvar :: kind) => ty
                fn (tvar :: kind) => ty
```