use crate::types::{
    diagnostic::{Error, ErrorContext, ErrorType},
    result::{Result, ResultUtil},
    span::{Span, Spanned},
    store::Store,
    token::{SToken, Token, TokenType},
    token_stream::TokenStream,
};

pub fn lex(lo: u32, src: &[u8], store: &mut Store) -> Result<TokenStream> {
    let mut lexer = Lexer::new(lo, src, store);
    lexer.lex()?;
    Ok(TokenStream::new(
        lexer.res.into_boxed_slice(),
        lo + src.len() as u32,
    ))
}

struct CharStream<'a> {
    src: &'a [u8],
    pos: usize,
}

impl<'a> CharStream<'a> {
    fn next(&mut self) -> Option<(u32, u8)> {
        let pos = self.pos;
        if pos < self.src.len() {
            self.pos += 1;
            Some((pos as u32, self.src[pos]))
        } else {
            None
        }
    }

    fn skip(&mut self, n: usize) {
        self.pos += n;
    }

    fn peek(&mut self, n: usize) -> Option<(u32, u8)> {
        let pos = self.pos + n;
        if pos < self.src.len() {
            Some((pos as u32, self.src[pos]))
        } else {
            None
        }
    }

    fn peek_char(&mut self, n: usize) -> Option<u8> {
        self.peek(n).map(|v| v.1)
    }

    fn pos(&self) -> u32 {
        self.pos as u32
    }

    fn text(&self) -> &[u8] {
        &self.src[self.pos..]
    }
}

#[derive(PartialEq)]
enum BraceType {
    Plain,
    String,
}

struct Lexer<'a, 'b> {
    store: &'b mut Store,
    lo: u32,
    chars: CharStream<'a>,
    braces: Vec<Spanned<BraceType>>,
    res: Vec<SToken>,
}

impl<'a, 'b> Lexer<'a, 'b> {
    fn new(lo: u32, src: &'a [u8], store: &'b mut Store) -> Self {
        Lexer {
            store,
            lo,
            chars: CharStream { src, pos: 0 },
            braces: vec![],
            res: vec![],
        }
    }

    fn lex(&mut self) -> Result {
        while self.lex_one()? {
            //
        }
        if let Some(t) = self.braces.pop() {
            return self.error(t.span, ErrorType::UnmatchedToken(TokenType::LeftBrace));
        }
        Ok(())
    }

    fn skip_whitespace_and_comments(&mut self) {
        let c = &mut self.chars;
        loop {
            // skip whitespace
            loop {
                match c.peek_char(0) {
                    Some(b' ') | Some(b'\t') | Some(b'\r') | Some(b'\n') => c.skip(1),
                    _ => break,
                }
            }
            // skip comment
            match (c.peek_char(0), c.peek_char(1)) {
                (Some(b'/'), Some(b'/')) => loop {
                    match c.next() {
                        Some((_, b'\n')) | None => break,
                        _ => {}
                    }
                }
                _ => break,
            }
        }
    }

    fn lex_one(&mut self) -> Result<bool> {
        self.skip_whitespace_and_comments();
        match self.chars.peek(0) {
            Some(c) => {
                self.lex_one_(c)?;
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    ///
    fn lex_one_(&mut self, (cur_pos, cur): (u32, u8)) -> Result {
        // Step 1: Strings
        if cur == b'"' {
            let span = Span::new(self.pos(), self.pos() + 1);
            self.res.push(span.span(Token::StringStart));
            self.chars.skip(1);
            return self.string(span.lo);
        }

        macro_rules! ret {
            ($ty:ident) => {
                Spanned::new(Span::new(self.lo + cur_pos, self.pos()), $ty);
            };
        }

        let mut tkn = None;

        macro_rules! one {
            ($ty:ident) => {
                tkn = Some((Token::$ty, false));
            };
        }

        // Step 2: Unambiguous single-character tokens
        match cur {
            b'[' => one!(LeftBracket),
            b']' => one!(RightBracket),
            b'{' => one!(LeftBrace),
            b'}' => one!(RightBrace),
            b'(' => one!(LeftParen),
            b')' => one!(RightParen),
            b':' => one!(Colon),
            b',' => one!(Comma),
            b'*' => one!(Star),
            b'/' => one!(Slash),
            b'?' => one!(QuestionMark),
            b'@' => one!(At),
            b'%' => one!(Percent),
            _ => {}
        }

        if let Some((t, _)) = tkn {
            self.chars.skip(1);
            let res = ret!(t);
            self.res.push(res);
            match t {
                Token::LeftBrace => self.braces.push(res.span.span(BraceType::Plain)),
                Token::RightBrace => {
                    let ty = match self.braces.pop() {
                        Some(t) => t,
                        _ => {
                            return self
                                .error(res.span, ErrorType::UnmatchedToken(t.ty()))
                        }
                    };
                    if *ty == BraceType::String {
                        self.string(res.span.lo)?;
                    }
                }
                _ => {}
            }
            return Ok(());
        }

        macro_rules! two {
            ($ty:ident) => {
                tkn = Some((Token::$ty, true));
            };
        }

        // Step 3: Two-character tokens and ambiguous single-character tokens
        match (cur, self.chars.peek_char(1)) {
            (b'\\', Some(b'\\')) => two!(BackslashBackslash),
            (b'&', Some(b'&')) => two!(AmpersandAmpersand),
            (b'|', Some(b'|')) => two!(BarBar),
            (b'.', Some(b'.')) => two!(DotDot),
            (b'.', _) => one!(Dot),
            (b'=', Some(b'=')) => two!(EqualsEquals),
            (b'=', _) => one!(Equals),
            (b'!', Some(b'=')) => two!(ExclamationMarkEquals),
            (b'!', _) => one!(ExclamationMark),
            (b'<', Some(b'=')) => two!(LeftAngleEquals),
            (b'<', _) => one!(LeftAngle),
            (b'>', Some(b'=')) => two!(RightAngleEquals),
            (b'>', _) => one!(RightAngle),
            (b'+', Some(b'+')) => two!(PlusPlus),
            (b'+', _) => one!(Plus),
            (b'-', _) => one!(Minus),
            _ => {}
        }

        if let Some((t, two)) = tkn {
            self.chars.skip(1 + two as usize);
            self.res.push(ret!(t));
            return Ok(());
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

        // Step 4: Keywords
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
        keyword!(b"int/", IntSlash);
        keyword!(b"int%", IntPercent);

        if let Some((t, skip)) = tkn {
            self.chars.skip(skip);
            self.res.push(ret!(t));
            return Ok(());
        }

        // Step 5: Numbers
        if let b'0'..=b'9' = cur {
            self.number()?;
            return Ok(());
        }

        let is_ident_start = match cur {
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => true,
            _ => false,
        };

        // Step 6: Identifiers
        if is_ident_start {
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
            self.res.push(Spanned::new(
                Span::new(self.lo + cur_pos, self.pos()),
                Token::Ident(ident),
            ));
            return Ok(());
        }

        self.error(
            Span::new(self.lo + cur_pos, self.lo + cur_pos + 1),
            ErrorType::UnexpectedByte(cur),
        )
    }

    fn string(&mut self, start: u32) -> Result {
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
                    let bspan = Span::new(span.hi - 1, span.hi);
                    self.res.push(span.span(Token::String(id)));
                    self.res.push(bspan.span(Token::LeftBrace));
                    self.braces.push(bspan.span(BraceType::String));
                    return Ok(());
                }
                _ => {
                    return self.error(
                        Span::new(self.lo + esc_pos, self.pos()),
                        ErrorType::UnknownEscapeSequence(cur),
                    );
                }
            }
        }
        let span = Span::new(start, self.pos());
        let espan = Span::new(span.hi - 1, span.hi);
        let id = self.store.add_str(res.into_boxed_slice().into());
        self.res.push(span.span(Token::String(id)));
        self.res.push(espan.span(Token::StringEnd));
        Ok(())
    }

    fn unicode_escape(&mut self) -> Result<char> {
        macro_rules! next {
            ($b:expr) => {
                match self.chars.next() {
                    Some((_, $b)) => {}
                    Some((pos, b)) => {
                        return self.error(
                            Span::new(pos, self.pos()),
                            ErrorType::UnexpectedByte(b),
                        )
                    }
                    _ => return self.unexpected_eof(),
                }
            };
        }

        next!(b'{');
        let before = self.pos();
        let mut len = 0;
        let mut val = 0u32;
        while let Some((pos, mut next)) = self.chars.peek(0) {
            next -= match next {
                b'0'..=b'9' => b'0',
                b'a'..=b'f' => b'a' - 10,
                b'A'..=b'F' => b'A' - 10,
                _ => break,
            };
            len += 1;
            self.chars.skip(1);
            val = match val
                .checked_shl(4)
                .and_then(|val| val.checked_add(next as u32))
            {
                Some(val) => val,
                _ => {
                    return self
                        .error(Span::new(before, pos), ErrorType::OutOfBoundsLiteral)
                }
            }
        }
        if len == 0 {
            return self.error(
                Span::new(self.pos(), self.pos() + 1),
                ErrorType::MissingCodePoint,
            );
        }
        let after = self.pos();
        next!(b'}');
        match std::char::from_u32(val) {
            Some(c) => Ok(c),
            _ => self.error(Span::new(before, after), ErrorType::InvalidCodePoint(val)),
        }
    }

    fn number(&mut self) -> Result {
        let mut saw_dot = false;
        let mut post_dot_places = 0;
        let mut res = vec![];
        let mut base = 10;
        let (start, first) = self.chars.peek(0).unwrap();
        if first == b'0' {
            match self.chars.peek(1) {
                Some((_, b'b')) => base = 2,
                Some((_, b'o')) => base = 8,
                Some((_, b'x')) => base = 16,
                _ => {}
            }
            if base != 10 {
                self.chars.skip(2);
            }
        }
        macro_rules! parse {
            ($($pat:pat)|+) => {
                while let Some((next_pos, next)) = self.chars.peek(0) {
                    match next {
                        b'_' => { },
                        b'.' if !saw_dot => saw_dot = true,
                        $($pat)|+ => {
                            res.push(next);
                            if saw_dot {
                                post_dot_places += 1;
                            }
                        },
                        #[allow(unreachable_patterns, overlapping_patterns)]
                        b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' => {
                            return self.error(
                                Span::new(next_pos, next_pos + 1),
                                ErrorType::UnexpectedNumberSuffix(next),
                            );
                        }
                        _ => break,
                    }
                    self.chars.skip(1);
                }
            }
        }
        match base {
            2 => parse!(b'0' | b'1'),
            8 => parse!(b'0'..=b'7'),
            10 => parse!(b'0'..=b'9'),
            16 => parse!(b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F'),
            _ => unreachable!(),
        }
        let span = Span::new(start, self.pos());
        if res.is_empty() {
            return self.error(span, ErrorType::EmptyNumberLiteral);
        }
        let value = self.store.add_str(res.into_boxed_slice().into());
        self.res
            .push(span.span(Token::Number(value, base, post_dot_places)));
        Ok(())
    }

    fn pos(&self) -> u32 {
        self.lo + self.chars.pos()
    }

    fn unexpected_eof<T>(&self) -> Result<T> {
        let lo = self.lo + self.chars.src.len() as u32 - 1;
        self.error(Span::new(lo, lo + 1), ErrorType::UnexpectedEndOfInput)
    }

    fn error<T>(&self, span: Span, error: ErrorType) -> Result<T> {
        Err(Error {
            span,
            error,
            context: vec![],
        })
    }
}
