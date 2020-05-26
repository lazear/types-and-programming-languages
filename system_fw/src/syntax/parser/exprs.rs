use super::*;

impl<'s> Parser<'s> {
    fn record_row(&mut self) -> Result<Field, Error> {
        let mut span = self.current.span;
        let label = self.expect_lower_id()?;
        self.expect(Token::Equals)?;
        let expr = self.once(|p| p.parse_expr(), "missing expr in record row")?;
        span += self.prev;
        Ok(Field { label, expr, span })
    }

    fn record_expr(&mut self) -> Result<Expr, Error> {
        let mut span = self.current.span;
        self.expect(Token::LBrace)?;
        let fields = self.delimited(|p| p.record_row(), Token::Comma)?;
        self.expect(Token::RBrace)?;
        span += self.prev;
        Ok(Expr::new(ExprKind::Record(fields), span))
    }

    fn let_binding(&mut self) -> Result<Expr, Error> {
        let mut span = self.current.span;
        self.expect(Token::Let)?;
        // let pat = self.once(|p| p.parse_pattern(), "missing pattern in let
        // binding")?; self.expect(Token::Equals)?;
        // let t1 = self.once(|p| p.parse_expr(), "let binder required")?;
        let decls = self.parse_decl_seq()?;
        self.expect(Token::In)?;
        let t2 = self.once(|p| p.parse_expr(), "let body required")?;
        self.expect(Token::End)?;
        span += self.prev;
        Ok(Expr::new(ExprKind::Let(decls, Box::new(t2)), span))
    }

    fn case_arm(&mut self) -> Result<Arm, Error> {
        let mut span = self.current.span;
        let pat = self.once(|p| p.parse_pattern(), "missing pattern in case arm")?;
        self.expect(Token::DoubleArrow)?;
        let expr = self.once(|p| p.parse_expr(), "missing expression in case arm")?;
        self.bump_if(&Token::Comma);
        span += self.prev;
        Ok(Arm { pat, expr, span })
    }

    fn case_expr(&mut self) -> Result<Expr, Error> {
        let mut span = self.current.span;
        self.expect(Token::Case)?;
        let expr = self.once(|p| p.parse_expr(), "missing case expression")?;
        self.expect(Token::Of)?;
        self.bump_if(&Token::Bar);
        let arms = self.delimited(|p| p.case_arm(), Token::Bar)?;
        self.expect(Token::End)?;
        span += self.prev;
        Ok(Expr::new(ExprKind::Case(Box::new(expr), arms), span))
    }

    fn lambda_expr(&mut self) -> Result<Expr, Error> {
        let mut span = self.current.span;
        self.expect(Token::Lambda)?;
        let arg = self.once(
            |p| p.parse_pattern(),
            "expected pattern binding in lambda expression!",
        )?;
        self.expect(Token::DoubleArrow)?;
        let body = self.parse_expr()?;
        span += self.prev;
        Ok(Expr::new(
            ExprKind::Abs(Box::new(arg), Box::new(body)),
            span,
        ))
    }

    fn if_expr(&mut self) -> Result<Expr, Error> {
        let mut span = self.current.span;
        self.expect(Token::If)?;
        let guard = self.parse_expr()?;
        self.expect(Token::Then)?;
        let cond = self.parse_expr()?;
        self.expect(Token::Else)?;
        let alt = self.parse_expr()?;
        span += self.prev;
        Ok(Expr::new(
            ExprKind::If(Box::new(guard), Box::new(cond), Box::new(alt)),
            span,
        ))
    }

    /// atexp ::=   constant
    ///             id
    ///             { [label = exp] }
    ///             ()
    ///             ( exp, ... expN )
    ///             ( exp )
    ///             let decl in exp, ... expN end
    fn atomic_expr(&mut self) -> Result<Expr, Error> {
        let mut span = self.current.span;
        match self.current.data {
            Token::LowerId(_) => self
                .expect_lower_id()
                .map(|e| Expr::new(ExprKind::Var(e), span)),
            Token::UpperId(_) => self
                .expect_upper_id()
                .map(|e| Expr::new(ExprKind::Constr(e), span)),
            Token::LBrace => self.record_expr(),
            Token::Let => self.let_binding(),
            Token::Int(n) => {
                self.bump();
                Ok(Expr::new(ExprKind::Int(n), span))
            }
            Token::Unit => {
                self.bump();
                Ok(Expr::new(ExprKind::Unit, span))
            }
            Token::LParen => {
                self.expect(Token::LParen)?;
                let mut exprs = self.delimited(|p| p.parse_expr(), Token::Comma)?;
                let e = match exprs.len() {
                    1 => exprs.pop().unwrap(),
                    _ => Expr::new(ExprKind::Tuple(exprs), span),
                };
                self.expect(Token::RParen)?;
                span += self.prev;
                Ok(e)
            }
            _ => self.error(ErrorKind::ExpectedExpr),
        }
    }

    fn projection_expr(&mut self) -> Result<Expr, Error> {
        let mut span = self.current.span;
        let mut expr = self.atomic_expr()?;
        while self.bump_if(&Token::Dot) {
            span += self.prev;
            let p = self.once(|p| p.atomic_expr(), "expected expr after Dot")?;
            expr = Expr::new(ExprKind::Projection(Box::new(expr), Box::new(p)), span);
        }
        Ok(expr)
    }

    /// appexp ::=      atexp
    ///                 appexp atexp
    fn application_expr(&mut self) -> Result<Expr, Error> {
        let mut span = self.current.span;
        let mut expr = self.projection_expr()?;
        loop {
            if let Token::LowerId(s) = &self.current() {
                if self.infix.get(&s).is_some() {
                    break;
                }
            }

            if let Ok(e) = self.projection_expr() {
                span += self.prev;
                expr = Expr::new(ExprKind::App(Box::new(expr), Box::new(e)), span);
            } else if let Token::TypeAppSigil = self.current() {
                self.bump();
                let ty = self.type_atom()?;
                span += self.prev;
                expr = Expr::new(ExprKind::TyApp(Box::new(expr), Box::new(ty)), span);
            } else {
                break;
            }
        }
        Ok(expr)
    }

    /// exp ::=     appexp
    ///             exp path exp
    // fn infix_expr(&mut self) -> Result<Expr, Error> {
    //     let mut span = self.current.span;
    //     let mut expr = self.application_expr()?;
    //     while let Token::LowerId(s) = &self.current() {
    //         if self.infix.get(s).is_some() {
    //             let p = self.expect_lower_id()?;
    //             let e = self.application_expr()?;
    //             span += self.prev;
    //             expr = Expr::new(ExprKind::Infix(p, Box::new(expr), Box::new(e)), span)
    //         } else {
    //             break;
    //         }
    //     }
    //     Ok(expr)
    // }

    /// exp ::=     if exp then exp2 else exp3
    ///             case exp of casearm end
    ///             fn x
    ///             infix
    pub fn parse_expr(&mut self) -> Result<Expr, Error> {
        match self.current() {
            Token::Case => self.case_expr(),
            Token::If => self.if_expr(),
            Token::Lambda => self.lambda_expr(),
            _ => self.application_expr(),
        }
    }
}
