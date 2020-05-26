use super::*;

impl<'s> Parser<'s> {
    fn decl_datatype(&mut self) -> Result<Decl, Error> {
        let mut span = self.current.span;
        self.expect(Token::Datatype)?;
        let tyvars = self.parse_tyvar_sequence()?;
        let tyname = self.expect_lower_id()?;
        self.expect(Token::Equals)?;
        self.bump_if(&Token::Bar);
        let ty = self.type_sum()?;
        span += self.prev;
        Ok(Decl::new(DeclKind::Datatype(tyvars, tyname, ty), span))
    }

    fn decl_type(&mut self) -> Result<Decl, Error> {
        let mut span = self.current.span;
        self.expect(Token::Type)?;
        let tyvars = self.parse_tyvar_sequence()?;
        let tyname = self.expect_lower_id()?;
        self.expect(Token::Equals)?;
        let ty = self.parse_type()?;

        span += self.prev;
        Ok(Decl::new(DeclKind::Type(tyvars, tyname, ty), span))
    }

    fn decl_value(&mut self) -> Result<Decl, Error> {
        let mut span = self.current.span;
        self.expect(Token::Val)?;
        let tyvars = self.parse_tyvar_sequence()?;
        let pat = self.parse_pattern()?;
        self.expect(Token::Equals)?;
        let expr = self.parse_expr()?;
        span += self.prev;
        Ok(Decl::new(DeclKind::Value(tyvars, pat, expr), span))
    }

    fn decl_expr(&mut self) -> Result<Decl, Error> {
        let expr = self.parse_expr()?;
        let sp = expr.span;
        Ok(Decl::new(DeclKind::Expr(expr), sp))
    }
    /// Parse a simple declaration
    /// decl ::=    type
    ///             datatype
    ///             val
    ///             fun
    ///             structure
    ///             signature
    ///             infix
    ///             exp
    ///             open
    pub(crate) fn parse_decl(&mut self) -> Result<Decl, Error> {
        match self.current() {
            Token::Type => self.decl_type(),
            Token::Datatype => self.decl_datatype(),
            Token::Val => self.decl_value(),
            _ => self.decl_expr(),
        }
    }

    pub(crate) fn parse_decl_seq(&mut self) -> Result<Vec<Decl>, Error> {
        let mut seqs = vec![self.parse_decl()?];
        self.bump_if(&Token::Semicolon);
        while let Ok(d) = self.parse_decl() {
            seqs.push(d);
            self.bump_if(&Token::Semicolon);
        }
        Ok(seqs)
    }
}
