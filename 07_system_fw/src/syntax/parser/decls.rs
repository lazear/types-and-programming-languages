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
        Ok(Decl::with_id(
            DeclKind::Datatype(tyvars, tyname, ty),
            span,
            self.allocate_ast_id(),
        ))
    }

    fn decl_type(&mut self) -> Result<Decl, Error> {
        let mut span = self.current.span;
        self.expect(Token::Type)?;
        let tyvars = self.parse_tyvar_sequence()?;
        let tyname = self.expect_lower_id()?;
        self.expect(Token::Equals)?;
        let ty = self.parse_type()?;

        span += self.prev;
        Ok(Decl::with_id(
            DeclKind::Type(tyvars, tyname, ty),
            span,
            self.allocate_ast_id(),
        ))
    }

    fn decl_value(&mut self) -> Result<Decl, Error> {
        let mut span = self.current.span;
        self.expect(Token::Val)?;
        let tyvars = self.parse_tyvar_sequence()?;
        let pat = self.parse_pattern()?;
        self.expect(Token::Equals)?;
        let expr = self.parse_expr()?;
        span += self.prev;
        Ok(Decl::with_id(
            DeclKind::Value(tyvars, pat, expr),
            span,
            self.allocate_ast_id(),
        ))
    }

    fn decl_fun_arm(&mut self, ident: &str) -> Result<FnArm, Error> {
        let mut span = self.current.span;
        let id = self.expect_lower_id()?;
        if id != ident {
            return self.error(ErrorKind::FunctionIdMismatch);
        }
        let pats = self.plus(|p| p.atomic_pattern(), None)?;
        self.expect(Token::Equals)?;
        let expr = self.parse_expr()?;
        span += self.prev;
        Ok(FnArm { pats, expr, span })
    }

    fn decl_fun(&mut self) -> Result<Decl, Error> {
        let mut span = self.current.span;
        self.expect(Token::Function)?;
        let tyvars = self.parse_tyvar_sequence()?;

        // Peek the id and clone it, since decl_fun_arm will expect it to be there
        let ident = match self.current() {
            Token::LowerId(id) => id.clone(),
            _ => return self.error(ErrorKind::ExpectedIdentifier),
        };

        let arms = self.delimited(|p| p.decl_fun_arm(&ident), Token::Bar)?;
        span += self.prev;
        Ok(Decl::with_id(
            DeclKind::Function(tyvars, ident, arms),
            span,
            self.allocate_ast_id(),
        ))
    }

    fn decl_expr(&mut self) -> Result<Decl, Error> {
        let expr = self.parse_expr()?;
        let sp = expr.span;
        Ok(Decl::with_id(DeclKind::Expr(expr), sp, self.allocate_ast_id()))
    }

    /// Parse a simple declaration
    /// decl ::=    type
    ///             datatype
    ///             val
    ///             fun
    ///             exp
    pub fn parse_decl_atom(&mut self) -> Result<Decl, Error> {
        match self.current() {
            Token::Type => self.decl_type(),
            Token::Datatype => self.decl_datatype(),
            Token::Val => self.decl_value(),
            Token::Function => self.decl_fun(),
            _ => self.decl_expr(),
        }
    }

    pub(crate) fn parse_decl(&mut self) -> Result<Decl, Error> {
        let mut span = self.current.span;
        let mut d = self.parse_decl_atom()?;
        while let Token::And = self.current.data {
            self.bump();
            let d2 = self.once(|p| p.parse_decl_atom(), "expected declaration after `and`")?;
            span += self.prev;
            d = Decl::with_id(DeclKind::And(Box::new(d), Box::new(d2)), span, self.allocate_ast_id());
        }
        span += self.prev;
        Ok(d)
    }

    pub fn parse_program(&mut self) -> Result<Program, Error> {
        let mut decls = vec![self.parse_decl()?];
        self.bump_if(&Token::Semicolon);
        while let Ok(d) = self.parse_decl() {
            decls.push(d);
            self.bump_if(&Token::Semicolon);
        }
        Ok(Program { decls })
    }
}
