use super::lexer::Lexer;
use super::{Token, TokenKind};

use std::collections::VecDeque;
use util::diagnostic::Diagnostic;
use util::span::*;

use crate::patterns::{PatVarStack, Pattern};
use crate::terms::*;
use crate::types::*;

#[derive(Clone, Debug, Default)]
pub struct DeBruijnIndexer {
    inner: VecDeque<String>,
}

impl DeBruijnIndexer {
    pub fn push(&mut self, hint: String) -> usize {
        let idx = self.inner.len();
        self.inner.push_front(hint);
        idx
    }

    pub fn pop(&mut self) {
        self.inner.pop_front();
    }

    pub fn lookup(&self, key: &str) -> Option<usize> {
        for (idx, s) in self.inner.iter().enumerate() {
            if key == s {
                return Some(idx);
            }
        }
        None
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }
}

pub struct Parser<'s> {
    tmvar: DeBruijnIndexer,
    tyvar: DeBruijnIndexer,
    diagnostic: Diagnostic<'s>,
    lexer: Lexer<'s>,
    span: Span,
    token: Token,
}

#[derive(Clone, Debug)]
pub struct Error {
    pub span: Span,
    pub tok: Token,
    pub kind: ErrorKind,
}

#[derive(Clone, Debug)]
pub enum ErrorKind {
    ExpectedAtom,
    ExpectedIdent,
    ExpectedType,
    ExpectedPattern,
    ExpectedToken(TokenKind),
    UnboundTypeVar,
    Unknown,
    Eof,
}
impl<'s> Parser<'s> {
    /// Create a new [`Parser`] for the input `&str`
    pub fn new(input: &'s str) -> Parser<'s> {
        let mut p = Parser {
            tmvar: DeBruijnIndexer::default(),
            tyvar: DeBruijnIndexer::default(),
            diagnostic: Diagnostic::new(input),
            lexer: Lexer::new(input.chars()),
            span: Span::default(),
            token: Token::dummy(),
        };
        p.bump();
        p
    }

    pub fn diagnostic(self) -> Diagnostic<'s> {
        self.diagnostic
    }
}

impl<'s> Parser<'s> {
    /// Kleene Plus combinator
    fn once_or_more<T, F>(&mut self, func: F, delimiter: TokenKind) -> Result<Vec<T>, Error>
    where
        F: Fn(&mut Parser) -> Result<T, Error>,
    {
        let mut v = vec![func(self)?];
        while self.bump_if(&delimiter) {
            v.push(func(self)?);
        }
        Ok(v)
    }

    /// Expect combinator
    /// Combinator that must return Ok or a message will be pushed to
    /// diagnostic. This method should only be called after a token has
    /// already been bumped.
    fn once<T, F>(&mut self, func: F, message: &str) -> Result<T, Error>
    where
        F: Fn(&mut Parser) -> Result<T, Error>,
    {
        match func(self) {
            Ok(t) => Ok(t),
            Err(e) => {
                self.diagnostic.push(message, self.span);
                Err(e)
            }
        }
    }
}

impl<'s> Parser<'s> {
    fn error<T>(&self, kind: ErrorKind) -> Result<T, Error> {
        Err(Error {
            span: self.token.span,
            tok: self.token.clone(),
            kind,
        })
    }

    fn bump(&mut self) -> TokenKind {
        let prev = std::mem::replace(&mut self.token, self.lexer.lex());
        self.span = prev.span;
        prev.kind
    }

    fn bump_if(&mut self, kind: &TokenKind) -> bool {
        if &self.token.kind == kind {
            self.bump();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Result<(), Error> {
        if self.token.kind == kind {
            self.bump();
            Ok(())
        } else {
            self.diagnostic.push(
                format!("expected token {:?}, found {:?}", kind, self.token.kind),
                self.span,
            );
            self.error(ErrorKind::ExpectedToken(kind))
        }
    }

    fn kind(&self) -> &TokenKind {
        &self.token.kind
    }

    fn ty_variant(&mut self) -> Result<Variant, Error> {
        let label = self.uppercase_id()?;
        let ty = match self.ty() {
            Ok(ty) => ty,
            _ => Type::Unit,
        };

        Ok(Variant { label, ty })
    }

    fn ty_app(&mut self) -> Result<Type, Error> {
        if !self.bump_if(&TokenKind::LSquare) {
            return self.error(ErrorKind::ExpectedToken(TokenKind::LSquare));
        }
        let ty = self.ty()?;
        self.expect(TokenKind::RSquare)?;
        Ok(ty)
    }

    fn ty_atom(&mut self) -> Result<Type, Error> {
        match self.kind() {
            TokenKind::TyBool => {
                self.bump();
                Ok(Type::Bool)
            }
            TokenKind::TyNat => {
                self.bump();
                Ok(Type::Nat)
            }
            TokenKind::TyUnit => {
                self.bump();
                Ok(Type::Unit)
            }
            TokenKind::LParen => {
                self.bump();
                let r = self.ty()?;
                self.expect(TokenKind::RParen)?;
                Ok(r)
            }
            TokenKind::Forall => {
                self.bump();
                Ok(Type::Universal(Box::new(self.ty()?)))
            }
            TokenKind::Exists => {
                self.bump();
                let tvar = self.uppercase_id()?;
                self.expect(TokenKind::Proj)?;
                self.tyvar.push(tvar);
                let xs = Type::Existential(Box::new(self.ty()?));
                self.tyvar.pop();
                Ok(xs)
            }
            TokenKind::Uppercase(_) => {
                let ty = self.uppercase_id()?;
                match self.tyvar.lookup(&ty) {
                    Some(idx) => Ok(Type::Var(idx)),
                    None => Ok(Type::Alias(ty)),
                }
            }
            TokenKind::LBrace => {
                self.bump();
                let fields = self.once_or_more(|p| p.ty_variant(), TokenKind::Bar)?;
                self.expect(TokenKind::RBrace)?;
                Ok(Type::Variant(fields))
            }
            _ => self.error(ErrorKind::ExpectedType),
        }
    }

    fn ty_tuple(&mut self) -> Result<Type, Error> {
        if self.bump_if(&TokenKind::LParen) {
            let mut v = self.once_or_more(|p| p.ty(), TokenKind::Comma)?;
            self.expect(TokenKind::RParen)?;

            if v.len() > 1 {
                Ok(Type::Product(v))
            } else {
                Ok(v.remove(0))
            }
        } else {
            self.ty_atom()
        }
    }

    pub fn ty(&mut self) -> Result<Type, Error> {
        if self.bump_if(&TokenKind::Rec) {
            let name = self.uppercase_id()?;
            self.expect(TokenKind::Equals)?;
            self.tyvar.push(name);
            let ty = self.ty()?;
            self.tyvar.pop();
            return Ok(Type::Rec(Box::new(ty)));
        }

        let mut lhs = self.ty_tuple()?;
        if let TokenKind::TyArrow = self.kind() {
            self.bump();
            while let Ok(rhs) = self.ty() {
                lhs = Type::Arrow(Box::new(lhs), Box::new(rhs));
                if let TokenKind::TyArrow = self.kind() {
                    self.bump();
                } else {
                    break;
                }
            }
        }
        Ok(lhs)
    }

    fn tyabs(&mut self) -> Result<Term, Error> {
        let tyvar = self.uppercase_id()?;
        let sp = self.span;
        let ty = Box::new(Type::Var(self.tyvar.push(tyvar)));
        let body = self.once(|p| p.parse(), "abstraction body required")?;
        Ok(Term::new(Kind::TyAbs(Box::new(body)), sp + self.span))
    }

    fn tmabs(&mut self) -> Result<Term, Error> {
        let tmvar = self.lowercase_id()?;
        let sp = self.span;
        self.tmvar.push(tmvar);

        self.expect(TokenKind::Colon)?;
        let ty = self.once(|p| p.ty(), "type annotation required in abstraction")?;
        self.expect(TokenKind::Proj)?;
        let body = self.once(|p| p.parse(), "abstraction body required")?;
        self.tmvar.pop();
        Ok(Term::new(Kind::Abs(Box::new(ty), Box::new(body)), sp + self.span))
    }

    fn fold(&mut self) -> Result<Term, Error> {
        self.expect(TokenKind::Fold)?;
        let sp = self.span;
        let ty = self.once(|p| p.ty(), "type annotation required after `fold`")?;
        let tm = self.once(|p| p.parse(), "term required after `fold`")?;
        Ok(Term::new(Kind::Fold(Box::new(ty), Box::new(tm)), sp + self.span))
    }

    fn unfold(&mut self) -> Result<Term, Error> {
        self.expect(TokenKind::Unfold)?;
        let sp = self.span;
        let ty = self.once(|p| p.ty(), "type annotation required after `unfold`")?;
        let tm = self.once(|p| p.parse(), "term required after `unfold`")?;
        Ok(Term::new(Kind::Unfold(Box::new(ty), Box::new(tm)), sp + self.span))
    }

    fn fix(&mut self) -> Result<Term, Error> {
        let sp = self.span;
        self.expect(TokenKind::Fix)?;
        let t = self.parse()?;
        Ok(Term::new(Kind::Fix(Box::new(t)), sp + self.span))
    }

    fn letexpr(&mut self) -> Result<Term, Error> {
        let sp = self.span;
        self.expect(TokenKind::Let)?;
        let mut pat = self.once(|p| p.pattern(), "missing pattern")?;

        self.expect(TokenKind::Equals)?;

        let t1 = self.once(|p| p.parse(), "let binder required")?;
        let len = self.tmvar.len();
        for var in PatVarStack::collect(&mut pat).into_iter().rev() {
            self.tmvar.push(var);
        }
        self.expect(TokenKind::In)?;
        let t2 = self.once(|p| p.parse(), "let body required")?;
        while self.tmvar.len() > len {
            self.tmvar.pop();
        }
        Ok(Term::new(
            Kind::Let(Box::new(pat), Box::new(t1), Box::new(t2)),
            sp + self.span,
        ))
    }

    fn lambda(&mut self) -> Result<Term, Error> {
        self.expect(TokenKind::Lambda)?;
        match self.kind() {
            TokenKind::Uppercase(_) => self.tyabs(),
            TokenKind::Lowercase(_) => self.tmabs(),
            _ => {
                self.diagnostic
                    .push("expected identifier after lambda, found".to_string(), self.span);
                self.error(ErrorKind::ExpectedIdent)
            }
        }
    }

    fn paren(&mut self) -> Result<Term, Error> {
        self.expect(TokenKind::LParen)?;
        let span = self.span;
        let mut n = self.once_or_more(|p| p.parse(), TokenKind::Comma)?;
        self.expect(TokenKind::RParen)?;
        if n.len() > 1 {
            Ok(Term::new(Kind::Product(n), span + self.span))
        } else {
            // invariant, n.len() >= 1
            Ok(n.remove(0))
        }
    }

    fn uppercase_id(&mut self) -> Result<String, Error> {
        match self.bump() {
            TokenKind::Uppercase(s) => Ok(s),
            tk => {
                self.diagnostic
                    .push(format!("expected uppercase identifier, found {:?}", tk), self.span);
                self.error(ErrorKind::ExpectedIdent)
            }
        }
    }

    fn lowercase_id(&mut self) -> Result<String, Error> {
        match self.bump() {
            TokenKind::Lowercase(s) => Ok(s),
            tk => {
                self.diagnostic
                    .push(format!("expected lowercase identifier, found {:?}", tk), self.span);
                self.error(ErrorKind::ExpectedIdent)
            }
        }
    }

    fn literal(&mut self) -> Result<Term, Error> {
        let lit = match self.bump() {
            TokenKind::Nat(x) => Literal::Nat(x),
            TokenKind::True => Literal::Bool(true),
            TokenKind::False => Literal::Bool(false),
            TokenKind::Unit => Literal::Unit,
            _ => return self.error(ErrorKind::Unknown),
        };
        Ok(Term::new(Kind::Lit(lit), self.span))
    }

    fn primitive(&mut self) -> Result<Term, Error> {
        let p = match self.bump() {
            TokenKind::IsZero => Primitive::IsZero,
            TokenKind::Succ => Primitive::Succ,
            TokenKind::Pred => Primitive::Pred,
            _ => return self.error(ErrorKind::Unknown),
        };
        Ok(Term::new(Kind::Primitive(p), self.span))
    }

    /// Important to note that this function can push variable names to the
    /// de Bruijn naming context. Callers of this function are responsible for
    /// making sure that the stack is balanced afterwards
    fn pat_atom(&mut self) -> Result<Pattern, Error> {
        match self.kind() {
            TokenKind::LParen => self.pattern(),
            TokenKind::Wildcard => {
                self.bump();
                Ok(Pattern::Any)
            }
            TokenKind::Uppercase(_) => {
                let tycon = self.uppercase_id()?;
                let inner = match self.pattern() {
                    Ok(pat) => pat,
                    _ => Pattern::Any,
                };
                Ok(Pattern::Constructor(tycon, Box::new(inner)))
            }
            TokenKind::Lowercase(_) => {
                let var = self.lowercase_id()?;
                // self.tmvar.push(var.clone());
                Ok(Pattern::Variable(var))
            }
            TokenKind::True => {
                self.bump();
                Ok(Pattern::Literal(Literal::Bool(true)))
            }
            TokenKind::False => {
                self.bump();
                Ok(Pattern::Literal(Literal::Bool(false)))
            }
            TokenKind::Unit => {
                self.bump();
                Ok(Pattern::Literal(Literal::Unit))
            }
            TokenKind::Nat(n) => {
                // O great borrowck, may this humble offering appease thee
                let n = *n;
                self.bump();
                Ok(Pattern::Literal(Literal::Nat(n)))
            }
            _ => self.error(ErrorKind::ExpectedPattern),
        }
    }

    fn pattern(&mut self) -> Result<Pattern, Error> {
        match self.kind() {
            TokenKind::LParen => {
                self.bump();
                let mut v = self.once_or_more(|p| p.pat_atom(), TokenKind::Comma)?;
                self.expect(TokenKind::RParen)?;
                if v.len() > 1 {
                    Ok(Pattern::Product(v))
                } else {
                    // v must have length == 1, else we would have early returned
                    assert_eq!(v.len(), 1);
                    Ok(v.remove(0))
                }
            }
            _ => self.pat_atom(),
        }
    }

    fn case_arm(&mut self) -> Result<Arm, Error> {
        // match self.kind() {
        //     TokenKind::Bar => self.bump(),
        //     _ => return self.error(ErrorKind::ExpectedToken(TokenKind::Bar)),
        // };

        // We don't track the length of the debruijn index in other methods,
        // but we have a couple branches where variables might be bound,
        // and this is pretty much the easiest way of doing it

        let len = self.tmvar.len();
        let mut span = self.span;

        let mut pat = self.once(|p| p.pattern(), "missing pattern")?;

        for var in PatVarStack::collect(&mut pat).into_iter().rev() {
            self.tmvar.push(var);
        }

        self.expect(TokenKind::Equals)?;
        self.expect(TokenKind::Gt)?;

        let term = Box::new(self.once(|p| p.application(), "missing case term")?);

        self.bump_if(&TokenKind::Comma);

        // Unbind any variables from the parsing context
        while self.tmvar.len() > len {
            self.tmvar.pop();
        }

        span = span + self.span;

        Ok(Arm { span, pat, term })
    }

    fn case(&mut self) -> Result<Term, Error> {
        self.expect(TokenKind::Case)?;
        let span = self.span;
        let expr = self.once(|p| p.parse(), "missing case expression")?;
        self.expect(TokenKind::Of)?;

        self.bump_if(&TokenKind::Bar);
        let arms = self.once_or_more(|p| p.case_arm(), TokenKind::Bar)?;

        Ok(Term::new(Kind::Case(Box::new(expr), arms), span + self.span))
    }

    fn injection(&mut self) -> Result<Term, Error> {
        let label = self.uppercase_id()?;
        let sp = self.span;
        let term = match self.parse() {
            Ok(t) => t,
            _ => Term::new(Kind::Lit(Literal::Unit), self.span),
        };

        self.expect(TokenKind::Of)?;
        let ty = self.ty()?;
        Ok(Term::new(
            Kind::Injection(label, Box::new(term), Box::new(ty)),
            sp + self.span,
        ))
    }

    fn pack(&mut self) -> Result<Term, Error> {
        self.expect(TokenKind::Pack)?;
        let sp = self.span;
        let witness = self.ty()?;
        self.expect(TokenKind::Comma)?;
        let evidence = self.parse()?;
        self.expect(TokenKind::As)?;
        let signature = self.ty()?;

        Ok(Term::new(
            Kind::Pack(Box::new(witness), Box::new(evidence), Box::new(signature)),
            sp + self.span,
        ))
    }

    fn unpack(&mut self) -> Result<Term, Error> {
        self.expect(TokenKind::Unpack)?;
        let sp = self.span;
        let package = self.parse()?;
        self.expect(TokenKind::As)?;

        let tyvar = self.uppercase_id()?;
        self.expect(TokenKind::Comma)?;
        let name = self.lowercase_id()?;
        self.tyvar.push(tyvar);
        self.tmvar.push(name);
        self.expect(TokenKind::In)?;
        let expr = self.parse()?;
        self.tmvar.pop();
        self.tyvar.pop();
        Ok(Term::new(
            Kind::Unpack(Box::new(package), Box::new(expr)),
            sp + self.span,
        ))
    }

    fn atom(&mut self) -> Result<Term, Error> {
        match self.kind() {
            TokenKind::LParen => self.paren(),
            TokenKind::Fix => self.fix(),
            TokenKind::Fold => self.fold(),
            TokenKind::Unfold => self.unfold(),
            TokenKind::Pack => self.pack(),
            TokenKind::Unpack => self.unpack(),
            TokenKind::IsZero | TokenKind::Succ | TokenKind::Pred => self.primitive(),
            TokenKind::Uppercase(_) => self.injection(),
            TokenKind::Lowercase(s) => {
                let var = self.lowercase_id()?;
                match self.tmvar.lookup(&var) {
                    Some(idx) => Ok(Term::new(Kind::Var(idx), self.span)),
                    None => {
                        self.diagnostic.push(format!("unbound variable {}", var), self.span);
                        self.error(ErrorKind::UnboundTypeVar)
                    }
                }
            }
            TokenKind::Nat(_) | TokenKind::True | TokenKind::False | TokenKind::Unit => self.literal(),
            TokenKind::Eof => self.error(ErrorKind::Eof),
            TokenKind::Semicolon => {
                self.bump();
                self.error(ErrorKind::ExpectedAtom)
            }
            _ => self.error(ErrorKind::ExpectedAtom),
        }
    }

    /// Parse a term of form:
    /// projection = atom `.` projection
    /// projection = atom
    fn projection(&mut self) -> Result<Term, Error> {
        let atom = self.atom()?;
        if self.bump_if(&TokenKind::Proj) {
            let idx = match self.bump() {
                TokenKind::Nat(idx) => idx,
                _ => {
                    self.diagnostic
                        .push(format!("expected integer index after {}", atom), self.span);
                    return self.error(ErrorKind::ExpectedToken(TokenKind::Proj));
                }
            };
            let sp = atom.span + self.span;
            Ok(Term::new(Kind::Projection(Box::new(atom), idx as usize), sp))
        } else {
            Ok(atom)
        }
    }

    /// Parse an application of form:
    /// application = atom application' | atom
    /// application' = atom application' | empty
    fn application(&mut self) -> Result<Term, Error> {
        let mut app = self.projection()?;

        loop {
            let sp = app.span;
            if let Ok(ty) = self.ty_app() {
                // Full type inference for System F is undecidable
                // Additionally, even partial type reconstruction,
                // where only type application types are erased is also
                // undecidable, see TaPL 23.6.2, Boehm 1985, 1989
                //
                // Partial erasure rules:
                // erasep(x) = x
                // erasep(λx:T. t) = λx:T. erasep(t)
                // erasep(t1 t2) = erasep(t1) erasep(t2)
                // erasep(λX. t) = λX. erasep(t)
                // erasep(t T) = erasep(t) []      <--- erasure of TyApp
                app = Term::new(Kind::TyApp(Box::new(app), Box::new(ty)), sp + self.span);
            } else if let Ok(term) = self.projection() {
                app = Term::new(Kind::App(Box::new(app), Box::new(term)), sp + self.span);
            } else {
                break;
            }
        }
        Ok(app)
    }

    pub fn parse(&mut self) -> Result<Term, Error> {
        match self.kind() {
            TokenKind::Case => self.case(),
            TokenKind::Lambda => self.lambda(),
            TokenKind::Let => self.letexpr(),
            _ => self.application(),
        }
    }
}
