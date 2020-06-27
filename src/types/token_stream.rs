use crate::types::{
    diagnostic::{Error, ErrorType, TokenAlternative},
    result::Result,
    span::{Span, Spanned},
    store::StrId,
    token::{SToken, Token, TokenType},
};

macro_rules! next_t {
    ($slf:expr, $pat:pat, $ty:expr) => {
        match $slf.next()? {
            t @ Spanned { val: $pat, .. } => Ok(t),
            t => $slf.error(
                t.span,
                ErrorType::UnexpectedToken(TokenAlternative::List(&[$ty]), t.ty()),
            ),
        }
    };
}

pub struct TokenStream {
    tokens: Box<[SToken]>,
    pos: usize,
    hi: u32,
}

impl TokenStream {
    pub fn new(tokens: Box<[SToken]>, hi: u32) -> Self {
        TokenStream { tokens, hi, pos: 0 }
    }

    fn unexpected_eof<T>(&self) -> Result<T> {
        let lo = self.hi - 1;
        self.error(Span::new(lo, self.hi), ErrorType::UnexpectedEndOfInput)
    }

    pub fn peek(&mut self, n: usize) -> Result<SToken> {
        match self.try_peek(n) {
            Some(t) => Ok(t),
            _ => self.unexpected_eof(),
        }
    }

    pub fn try_peek(&self, n: usize) -> Option<SToken> {
        self.tokens.get(self.pos + n).copied()
    }

    pub fn next(&mut self) -> Result<SToken> {
        match self.try_next() {
            Some(t) => Ok(t),
            _ => self.unexpected_eof(),
        }
    }

    pub fn try_next(&mut self) -> Option<SToken> {
        let res = self.try_peek(0);
        if res.is_some() {
            self.pos += 1;
        }
        res
    }

    pub fn skip(&mut self, n: usize) {
        self.pos += n;
    }

    pub fn next_ident(&mut self) -> Result<(SToken, StrId)> {
        let t = self.next()?;
        match t.val {
            Token::Ident(id) => Ok((t, id)),
            _ => self.error(
                t.span,
                ErrorType::UnexpectedToken(
                    TokenAlternative::List(&[TokenType::Ident]),
                    t.ty(),
                ),
            ),
        }
    }

    pub fn next_assign(&mut self) -> Result<SToken> {
        next_t!(self, Token::Equals, TokenType::Equals)
    }

    pub fn next_then(&mut self) -> Result<SToken> {
        next_t!(self, Token::Then, TokenType::Then)
    }

    pub fn next_else(&mut self) -> Result<SToken> {
        next_t!(self, Token::Else, TokenType::Else)
    }

    pub fn next_left_brace(&mut self) -> Result<SToken> {
        next_t!(self, Token::LeftBrace, TokenType::LeftBrace)
    }

    pub fn next_right_brace(&mut self) -> Result<SToken> {
        next_t!(self, Token::RightBrace, TokenType::RightBrace)
    }

    pub fn next_right_paren(&mut self) -> Result<SToken> {
        next_t!(self, Token::RightParen, TokenType::RightParen)
    }

    pub fn next_colon(&mut self) -> Result<SToken> {
        next_t!(self, Token::Colon, TokenType::Colon)
    }

    fn error<T>(&self, span: Span, error: ErrorType) -> Result<T> {
        Err(Error {
            span,
            error,
            context: vec![],
        })
    }

    pub fn eof(&self) -> bool {
        self.pos >= self.tokens.len()
    }
}
