use super::*;

use TypeKind::*;

impl<'s> Parser<'s> {
    /// Parse a datatype Constructor [A-Z]+
    pub fn variant(&mut self) -> Result<Variant, Error> {
        let mut span = self.current.span;
        let label = self.expect_upper_id()?;
        let ty = if self.bump_if(&Token::Of) {
            Some(self.parse_type()?)
        } else {
            None
        };
        span += self.prev;
        Ok(Variant { label, ty, span })
    }

    pub fn type_sum(&mut self) -> Result<Type, Error> {
        let mut span = self.current.span;
        let vars = self.delimited(|p| p.variant(), Token::Bar)?;
        span += self.prev;
        Ok(Type::new(Sum(vars), span))
    }

    /// Parse a single type variable '[a-z]+
    fn parse_tyvar(&mut self) -> Result<Type, Error> {
        let mut span = self.current.span;
        self.expect(Token::Apostrophe)?;
        let v = self.expect_lower_id().map(Variable);
        span += self.prev;
        v.map(|t| Type::new(t, span))
    }

    /// Parse a sequence of type variables ('a, 'b), which are N-arity arguments
    /// to a type constructor.
    /// This may return an empty Vec<>
    pub(crate) fn parse_tyvar_sequence(&mut self) -> Result<Vec<Type>, Error> {
        if self.bump_if(&Token::LParen) {
            let ret = self.delimited(|p| p.parse_tyvar(), Token::Comma)?;
            self.expect(Token::RParen)?;
            return Ok(ret);
        }
        Ok(self.star(|p| p.parse_tyvar(), Some(&Token::Comma)))
    }

    /// Parse a type sequence (ty1, ty2) , which are N-arity arguments to a
    /// type constructor.
    /// If an `Ok(Vec)` is returned, then Vec<> will always have N>=1 items
    fn parse_type_sequence(&mut self) -> Result<Vec<Type>, Error> {
        self.expect(Token::LParen)?;
        let ret = self.delimited(|p| p.parse_type(), Token::Comma)?;
        self.expect(Token::RParen)?;
        Ok(ret)
    }

    /// Parse a existential type of form `forall ('tv :: K) of ty`
    fn existential(&mut self) -> Result<Type, Error> {
        let mut span = self.current.span;
        self.expect(Token::Exists)?;
        let (name, kind) = self.once(
            |p| p.abstraction_arg(),
            "existential type requires an arg of form ('t :: K)",
        )?;
        self.expect(Token::Of)?;
        let body = self.once(|p| p.parse_type(), "existential type requires a body")?;
        span += self.prev;

        // We should probably just parse tyvars as string directly...
        Ok(Type::new(
            Existential(name.kind.as_tyvar_d(), Box::new(kind), Box::new(body)),
            span,
        ))
    }

    /// Parse a universal type of form `forall ('tv :: K) of ty`
    fn universal(&mut self) -> Result<Type, Error> {
        let mut span = self.current.span;
        self.expect(Token::Forall)?;
        let (name, kind) = self.once(
            |p| p.abstraction_arg(),
            "universal type requires an arg of form ('t :: K)",
        )?;
        self.expect(Token::Of)?;
        let body = self.once(|p| p.parse_type(), "universal type requires a body")?;
        span += self.prev;
        Ok(Type::new(
            Universal(name.kind.as_tyvar_d(), Box::new(kind), Box::new(body)),
            span,
        ))
    }

    /// Parse a type row of form `label: ty`
    fn row(&mut self) -> Result<Row, Error> {
        let mut span = self.current.span;
        let label = self.expect_lower_id()?;
        self.expect(Token::Colon)?;
        let ty = self.once(
            |p| p.parse_type(),
            "record type row requires a type {label: ty, ...}",
        )?;
        span += self.prev;

        Ok(Row { label, ty, span })
    }

    /// Parse a type of form `{ label: ty, label2: ty2, ...}`
    fn record(&mut self) -> Result<Type, Error> {
        let mut span = self.current.span;
        self.expect(Token::LBrace)?;
        let rows = self.delimited(|p| p.row(), Token::Comma)?;
        self.expect(Token::RBrace)?;
        span += self.prev;
        Ok(Type::new(Record(rows), span))
    }

    /// Parse a type of form:
    /// ty ::=  'var
    ///         id
    ///         ( ty )
    ///         ( ty1, ... tyN) ty
    ///         fn (var :: kind) => ty
    ///         exists (var :: kind) of ty
    ///         forall (var :: kind) of ty
    ///         rec ty
    ///         { label: ty, ...}
    pub(crate) fn type_atom(&mut self) -> Result<Type, Error> {
        let mut span = self.current.span;
        match self.current.data {
            Token::TyInt => {
                self.bump();
                Ok(Type::new(Int, span))
            }
            Token::TyBool => {
                self.bump();
                Ok(Type::new(Bool, span))
            }
            Token::TyUnit => {
                self.bump();
                Ok(Type::new(Unit, span))
            }
            Token::Apostrophe => self.parse_tyvar(),
            Token::LowerId(_) => self.expect_lower_id().map(|p| Type::new(Defined(p), span)),
            Token::Lambda => self.abstraction(),
            Token::Exists => self.existential(),
            Token::Forall => self.universal(),
            Token::Rec => {
                self.expect(Token::Rec)?;
                let ty = self.parse_type()?;
                span += self.prev;
                Ok(Type::new(Recursive(Box::new(ty)), span))
            }
            Token::LBrace => self.record(),
            Token::LParen => {
                // Handle a set of N-arity arguments to constructors
                let mut v = self.parse_type_sequence()?;
                if v.len() == 1 {
                    Ok(v.pop().unwrap())
                } else {
                    let tycon = self.type_atom()?;
                    Ok(v.into_iter().fold(tycon, |ty, v| {
                        let sp = ty.span + v.span;
                        Type::new(Application(Box::new(ty), Box::new(v)), sp)
                    }))
                }
            }
            Token::Wildcard => {
                self.bump();
                Ok(Type::new(Infer, span))
            }
            _ => self.error(ErrorKind::ExpectedType),
        }
    }

    /// Parse an argument of form: `('t :: K)`
    fn abstraction_arg(&mut self) -> Result<(Type, Kind), Error> {
        self.expect(Token::LParen)?;
        let tyvar = self.parse_tyvar()?;
        self.expect(Token::Colon)?;
        self.expect(Token::Colon)?;
        let k = self.kind()?;
        self.expect(Token::RParen)?;
        Ok((tyvar, k))
    }

    /// Parse a type of form: `lambda ('t :: K) => ty`
    fn abstraction(&mut self) -> Result<Type, Error> {
        let mut span = self.current.span;
        self.expect(Token::Lambda)?;

        // let args = self.plus(|p| p.abstraction_arg())?;
        let (name, kind) = self.once(
            |p| p.abstraction_arg(),
            "type abstraction requires an arg of form ('t :: K)",
        )?;
        self.expect(Token::DoubleArrow)?;
        let body = self.parse_type()?;
        span += self.prev;
        Ok(Type::new(
            Abstraction(name.kind.as_tyvar_d(), Box::new(kind), Box::new(body)),
            span,
        ))
    }

    /// Parse an application of form: `('a, 'b, ...) ty1 ty2 ty3`
    fn application(&mut self) -> Result<Type, Error> {
        // TODO: Confirm this is incorrect for all cases
        let mut tys = self.plus(|p| p.type_atom(), None)?;
        tys.reverse();
        let ty = tys.pop().unwrap();
        Ok(tys.into_iter().rev().fold(ty, |ty, v| {
            let sp = ty.span + v.span;
            Type::new(Application(Box::new(v), Box::new(ty)), sp)
        }))
    }

    /// Parse a type of form: `ty` | `ty * ty2 * ...`
    fn product(&mut self) -> Result<Type, Error> {
        let mut span = self.current.span;
        let mut v = self.delimited(|p| p.application(), Token::Asterisk)?;
        span += self.prev;
        match v.len() {
            1 => Ok(v.pop().unwrap()),
            _ => Ok(Type::new(Product(v), span)),
        }
    }

    /// Parse a type of form: `ty * ty` | `ty -> ty`
    pub fn parse_type(&mut self) -> Result<Type, Error> {
        let mut span = self.current.span;
        let ty = self.product()?;
        if self.bump_if(&Token::SingleArrow) {
            let ty2 = self.parse_type()?;
            span += ty2.span;
            return Ok(Type::new(Function(Box::new(ty), Box::new(ty2)), span));
        }
        Ok(ty)
    }

    /// Parse a kind of form: `* | ( K )`
    fn kind_single(&mut self) -> Result<Kind, Error> {
        if self.bump_if(&Token::LParen) {
            let k = self.kind()?;
            self.expect(Token::RParen)?;
            return Ok(k);
        }
        self.expect(Token::Asterisk)?;
        Ok(Kind::Star)
    }

    /// Parse a kind of form: `K | K -> K`
    pub fn kind(&mut self) -> Result<Kind, Error> {
        let k = self.kind_single()?;
        if self.bump_if(&Token::SingleArrow) {
            let k2 = self.kind()?;
            return Ok(Kind::Arrow(Box::new(k), Box::new(k2)));
        }
        Ok(k)
    }
}
