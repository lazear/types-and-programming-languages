use super::*;

use PatKind::*;

impl<'s> Parser<'s> {
    fn tuple_pattern(&mut self) -> Result<Pattern, Error> {
        let mut span = self.current.span;
        self.expect(Token::LParen)?;
        let mut v = self.star(|p| p.parse_pattern(), Some(&Token::Comma));
        self.expect(Token::RParen)?;
        span += self.prev;
        match v.len() {
            0 => Ok(Pattern::new(Unit, span)),
            1 => Ok(v.pop().unwrap()),
            _ => Ok(Pattern::new(Product(v), span)),
        }
    }

    fn record_pattern(&mut self) -> Result<Pattern, Error> {
        let mut span = self.current.span;
        self.expect(Token::LBrace)?;
        let v = self.delimited(|p| p.expect_lower_id(), Token::Comma)?;
        self.expect(Token::RBrace)?;
        span += self.prev;
        Ok(Pattern::new(Record(v), span))
    }

    /// atpat ::=   constant
    ///             id
    ///             wildcard
    ///             ( pat )
    ///             ( pat, ... patN )
    ///             { [patrow] }
    pub(crate) fn atomic_pattern(&mut self) -> Result<Pattern, Error> {
        let span = self.current.span;
        match self.current.data {
            Token::Wildcard => {
                self.bump();
                Ok(Pattern::new(Any, span))
            }
            Token::LowerId(_) => self.expect_lower_id().map(|s| Pattern::new(Variable(s), span)),
            Token::UpperId(_) => self.expect_upper_id().map(|s| Pattern::new(Constructor(s), span)),
            Token::Int(n) => {
                self.bump();
                Ok(Pattern::new(Literal(n), span))
            }
            Token::Unit => {
                self.bump();
                Ok(Pattern::new(PatKind::Unit, span))
            }
            Token::LParen => self.tuple_pattern(),
            Token::LBrace => self.record_pattern(),
            _ => self.error(ErrorKind::ExpectedPattern),
        }
    }

    /// app_pat ::=     atpat
    ///                 app_pat atpat
    fn application_pattern(&mut self) -> Result<Pattern, Error> {
        let mut span = self.current.span;
        let pat = self.atomic_pattern()?;
        if let PatKind::Constructor(_) = pat.kind {
            match self.atomic_pattern() {
                Ok(arg) => {
                    span += self.prev;
                    return Ok(Pattern::new(Application(Box::new(pat), Box::new(arg)), span));
                }
                _ => return Ok(pat),
            }
        }
        // while let Ok(e) = self.atomic_pattern() {
        //     span += self.prev;
        //     pat = Pattern::new(Application(Box::new(pat), Box::new(e)), span);
        // }

        Ok(pat)
    }

    pub fn parse_pattern(&mut self) -> Result<Pattern, Error> {
        let mut span = self.current.span;
        let pat = self.application_pattern()?;
        if self.bump_if(&Token::Colon) {
            let ty = self.once(|p| p.parse_type(), "expected type annotation after `pat :`")?;
            span += self.prev;
            return Ok(Pattern::new(Ascribe(Box::new(pat), Box::new(ty)), span));
        }
        Ok(pat)
    }
}
