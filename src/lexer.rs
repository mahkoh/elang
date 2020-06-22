use crate::{
    types::{
        diagnostic::{
            Error, ErrorContext, ErrorType,
            TokenAlternative,
        },
        num::{HexU32, Parsable},
        result::Result,
        span::{Span, Spanned},
        store::{Store, StrId},
        token::{SToken, Token, TokenType},
    },
};
use std::collections::VecDeque;
use crate::types::result::ResultUtil;

pub struct CharStream<'a> {
    src: &'a [u8],
    peek: VecDeque<(u32, u8)>,
    pos: usize,
}

impl<'a> CharStream<'a> {
    fn next(&mut self) -> Option<(u32, u8)> {
        if self.peek.len() > 0 {
            return self.peek.pop_front();
        }
        self.real_next()
    }

    fn skip(&mut self, n: usize) {
        for _ in 0..n {
            let _ = self.next();
        }
    }

    fn real_next(&mut self) -> Option<(u32, u8)> {
        let head = &self.src[self.pos..];
        if head.len() > 0 {
            self.pos += 1;
            Some(((self.pos - 1) as u32, head[0]))
        } else {
            None
        }
    }

    fn peek(&mut self, n: usize) -> Option<(u32, u8)> {
        if self.peek.len() <= n {
            for _ in 0..(n + 1 - self.peek.len()) {
                match self.real_next() {
                    Some(c) => self.peek.push_back(c),
                    _ => return None,
                }
            }
        }
        Some(self.peek[n])
    }

    fn peek_char(&mut self, n: usize) -> Option<u8> {
        self.peek(n).map(|v| v.1)
    }

    fn pos(&self) -> u32 {
        if self.peek.len() > 0 {
            self.peek[0].0
        } else {
            self.pos as u32
        }
    }

    fn text(&self) -> &[u8] {
        &self.src[self.pos() as usize..]
    }
}

pub struct Lexer<'a> {
    store: Store,
    peek: VecDeque<SToken>,
    lo: u32,
    pub chars: CharStream<'a>,
}

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

impl<'a> Lexer<'a> {
    pub fn new(lo: u32, src: &'a [u8], store: Store) -> Self {
        Lexer {
            store,
            peek: VecDeque::new(),
            lo,
            chars: CharStream {
                src,
                peek: VecDeque::new(),
                pos: 0,
            },
        }
    }

    pub fn pos(&self) -> u32 {
        self.lo + self.chars.pos()
    }

    pub fn next_pos(&mut self) -> Result<u32> {
        Ok(self.peek(0)?.span.lo)
    }

    fn skip_whitespace(&mut self) {
        while let Some(b' ') | Some(b'\t') | Some(b'\r') | Some(b'\n') =
            self.chars.peek_char(0)
        {
            self.chars.skip(1);
        }
    }

    fn skip_line(&mut self) {
        loop {
            match self.chars.next() {
                Some((_, b'\n')) | None => break,
                _ => {}
            }
        }
    }

    pub fn eof(&mut self) -> Result<bool> {
        Ok(self.try_peek(0)?.is_none())
    }

    fn unexpected_eof<T>(&self) -> Result<T> {
        let lo = self.lo + self.chars.src.len() as u32 - 1;
        self.error(Span::new(lo, lo + 1), ErrorType::UnexpectedEndOfInput)
    }

    pub fn peek(&mut self, n: usize) -> Result<SToken> {
        match self.try_peek(n)? {
            Some(t) => Ok(t),
            _ => self.unexpected_eof(),
        }
    }

    pub fn try_peek(&mut self, n: usize) -> Result<Option<SToken>> {
        if self.peek.len() <= n {
            for _ in 0..(n + 1 - self.peek.len()) {
                match self.real_next()? {
                    Some(c) => self.peek.push_back(c),
                    _ => return Ok(None),
                }
            }
        }
        Ok(Some(self.peek[n]))
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
        next_t!(self, Token::Assign, TokenType::Assign)
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

    pub fn next_comma(&mut self) -> Result<SToken> {
        next_t!(self, Token::Comma, TokenType::Comma)
    }

    pub fn next_right_paren(&mut self) -> Result<SToken> {
        next_t!(self, Token::RightParen, TokenType::RightParen)
    }

    pub fn next_colon(&mut self) -> Result<SToken> {
        next_t!(self, Token::Colon, TokenType::Colon)
    }

    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Result<SToken> {
        match self.try_next()? {
            Some(t) => Ok(t),
            _ => self.unexpected_eof(),
        }
    }

    pub fn try_next(&mut self) -> Result<Option<SToken>> {
        if self.peek.len() > 0 {
            return Ok(self.peek.pop_front());
        }
        self.real_next()
    }

    pub fn skip(&mut self, n: usize) {
        for _ in 0..n {
            let _ = self.try_next();
        }
    }

    fn real_next(&mut self) -> Result<Option<SToken>> {
        loop {
            self.skip_whitespace();
            if Some(b'/') == self.chars.peek_char(0)
                && Some(b'/') == self.chars.peek_char(1)
            {
                self.skip_line();
            } else {
                break;
            }
        }

        let (cur_pos, cur) = match self.chars.peek(0) {
            Some(c) => c,
            _ => return Ok(None),
        };

        if cur == b'"' {
            return self.string(false).map(Some);
        }

        macro_rules! ret {
            ($ty:ident) => {
                return Ok(Some(Spanned::new(
                    Span::new(self.lo + cur_pos, self.pos()),
                    $ty,
                )));
            };
        }

        let mut tkn = None;

        macro_rules! one {
            ($ty:ident) => {
                tkn = Some((Token::$ty, false));
            };
        }

        match cur {
            b'[' => one!(LeftBracket),
            b']' => one!(RightBracket),
            b'{' => one!(LeftBrace),
            b'}' => one!(RightBrace),
            b'(' => one!(LeftParen),
            b')' => one!(RightParen),
            b':' => one!(Colon),
            b',' => one!(Comma),
            b'*' => one!(Times),
            b'/' => one!(Div),
            b'?' => one!(Questionmark),
            b'@' => one!(At),
            b'%' => one!(Mod),
            _ => {}
        }

        if let Some((t, _)) = tkn {
            self.chars.skip(1);
            ret!(t);
        }

        macro_rules! two {
            ($ty:ident) => {
                tkn = Some((Token::$ty, true));
            };
        }

        if let Some(c) = self.chars.peek_char(1) {
            match (cur, c) {
                (b'\\', b'\\') => two!(Overlay),
                (b'&', b'&') => two!(AndAnd),
                (b'|', b'|') => two!(OrOr),
                (b'.', b'.') => two!(DotDot),
                (b'.', _) => one!(Dot),
                (b'=', b'=') => two!(Equals),
                (b'=', _) => one!(Assign),
                (b'!', b'=') => two!(Unequal),
                (b'!', _) => one!(Not),
                (b'<', b'=') => two!(Le),
                (b'<', _) => one!(Lt),
                (b'>', b'=') => two!(Ge),
                (b'>', _) => one!(Gt),
                (b'+', b'+') => two!(Concat),
                (b'+', _) => one!(Plus),
                (b'-', b'>') => two!(Implies),
                (b'-', _) => one!(Minus),
                _ => {}
            }
        }

        if let Some((t, two)) = tkn {
            self.chars.skip(1 + two as usize);
            ret!(t);
        }

        let mut tkn = None;

        macro_rules! is_ident_cont {
            ($c:expr) => {
                match $c {
                    b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_' => true,
                    _ => false,
                }
            };
        }

        macro_rules! keyword {
            ($txt:expr, $ty:ident) => {
                if self.chars.text().starts_with($txt) {
                    match self.chars.peek_char($txt.len()) {
                        Some(c) => {
                            if !is_ident_cont!(c) {
                                tkn = Some((Token::$ty, $txt.len()));
                            }
                        }
                        None => tkn = Some((Token::$ty, $txt.len())),
                    }
                }
            };
        }

        keyword!(b"true", True);
        keyword!(b"false", False);
        keyword!(b"null", Null);
        keyword!(b"rec", Rec);
        keyword!(b"let", Let);
        keyword!(b"in", In);
        keyword!(b"inherit", Inherit);
        keyword!(b"if", If);
        keyword!(b"then", Then);
        keyword!(b"else", Else);
        keyword!(b"or", Or);

        if let Some((t, skip)) = tkn {
            self.chars.skip(skip);
            ret!(t);
        }

        if let b'0'..=b'9' = cur {
            let (val, len) = i64::parse_bytes_init(self.chars.text());
            if let Some((next_pos, next)) = self.chars.peek(len) {
                if let b'0'..=b'9' = next {
                    return self.error(
                        Span::new(self.lo + cur_pos, self.lo + next_pos),
                        ErrorType::OutOfBoundsLiteral,
                    );
                }
                if is_ident_cont!(next) {
                    return self.error(
                        Span::new(self.lo + next_pos, self.lo + next_pos + 1),
                        ErrorType::UnexpectedIntegerSuffix(next),
                    );
                }
            }
            self.chars.skip(len);
            return Ok(Some(Spanned::new(
                Span::new(self.lo + cur_pos, self.pos()),
                Token::Integer(val),
            )));
        }

        macro_rules! is_ident_start {
            ($c:expr) => {
                match $c {
                    b'a'..=b'z' | b'A'..=b'Z' | b'_' => true,
                    _ => false,
                }
            };
        }

        if is_ident_start!(cur) {
            let mut i = 1;
            while let Some(c) = self.chars.peek_char(i) {
                if is_ident_cont!(c) {
                    i += 1;
                } else {
                    break;
                }
            }
            let ident = self.chars.text()[..i].to_vec();
            let ident = self.store.add_str(ident.into_boxed_slice().into());
            self.chars.skip(i);
            return Ok(Some(Spanned::new(
                Span::new(self.lo + cur_pos, self.pos()),
                Token::Ident(ident),
            )));
        }

        self.error(
            Span::new(self.lo + cur_pos, self.lo + cur_pos + 1),
            ErrorType::UnexpectedByte(cur),
        )
    }

    pub fn string(&mut self, is_cont: bool) -> Result<SToken> {
        self.string_(is_cont)
    }

    fn string_(&mut self, is_cont: bool) -> Result<SToken> {
        let start = if is_cont {
            let b = self.next_right_brace()?;
            self.chars.peek.clear();
            self.chars.pos = (b.span.hi - self.lo) as usize;
            self.peek.clear();
            b.span.lo
        } else {
            let pos = self.pos();
            self.chars.skip(1); // eat the "
            pos
        };

        let mut res = Vec::new();
        loop {
            let (esc_pos, cur) = match self.chars.next() {
                Some(x) => x,
                _ => return self.unexpected_eof(),
            };
            if cur == b'"' {
                break;
            }
            if cur != b'\\' {
                res.push(cur);
                continue;
            }
            let (_, cur) = match self.chars.next() {
                Some(c) => c,
                _ => return self.unexpected_eof(),
            };
            match cur {
                b'\\' => res.push(b'\\'),
                b'"' => res.push(b'"'),
                b'n' => res.push(b'\n'),
                b't' => res.push(b'\t'),
                b'u' => {
                    let ctx = ErrorContext::ParseUnicodeEscape(esc_pos);
                    let chr = self.unicode_escape().ctx(ctx)?;
                    let mut bytes = [0; 4];
                    res.extend_from_slice(chr.encode_utf8(&mut bytes).as_bytes())
                }
                b'{' => {
                    let id = self.store.add_str(res.into_boxed_slice().into());
                    let span = Span::new(start, self.pos());
                    return Ok(Spanned::new(span, Token::StringPart(id)));
                }
                _ => {
                    return self.error(
                        Span::new(self.lo + esc_pos, self.pos()),
                        ErrorType::UnknownEscapeSequence(cur),
                    );
                }
            }
        }
        let id = self.store.add_str(res.into_boxed_slice().into());
        Ok(Spanned::new(
            Span::new(start, self.pos()),
            Token::String(id),
        ))
    }

    fn unicode_escape(&mut self) -> Result<char> {
        self.next_left_brace()?;
        self.skip_whitespace();
        let before = self.pos();
        let (val, len) = HexU32::parse_bytes_init(self.chars.text());
        if len == 0 {
            return self.error(
                Span::new(self.pos(), self.pos() + 1),
                ErrorType::MissingCodePoint,
            );
        }
        self.chars.skip(len);
        let after = self.pos();
        let _ = self.next_right_brace()?;
        match std::char::from_u32(val.0) {
            Some(c) => Ok(c),
            _ => {
                return self.error(
                    Span::new(before, after),
                    ErrorType::InvalidCodePoint(val.0),
                );
            }
        }
    }

    fn error<T>(&self, span: Span, error: ErrorType) -> Result<T> {
        Err(Error {
            span,
            error,
            context: vec![],
        })
    }
}
