use crate::{Elang, Error, ErrorContext, ErrorType, Span, TokenAlternative};
use std::{fmt::Write, rc::Rc};

struct Codemap {
    files: Vec<Filemap>,
}

impl Codemap {
    fn new() -> Codemap {
        Codemap { files: vec![] }
    }

    fn add_file(&mut self, name: Rc<[u8]>, src: Rc<[u8]>) -> u32 {
        let old_pos = match self.files.last() {
            Some(f) => *f.lines.last().unwrap(),
            _ => 0,
        };
        let mut cur_pos = old_pos;
        let mut lines = Vec::new();
        lines.push(cur_pos);
        {
            let mut src = &*src;
            while src.len() > 0 {
                let pos = src.iter().position(|&c| c == b'\n').unwrap() + 1;
                cur_pos += pos as u32;
                src = &src[pos..];
                lines.push(cur_pos);
            }
        }
        let map = Filemap {
            file: name,
            src,
            lines: lines.into_boxed_slice(),
        };
        self.files.push(map);
        old_pos
    }

    fn file(&self, span: Span) -> &Filemap {
        for file in &*self.files {
            if file.lines[file.lines.len() - 1] > span.lo {
                return file;
            }
        }
        &self.files[self.files.len() - 1]
    }
}

struct Filemap {
    file: Rc<[u8]>,
    src: Rc<[u8]>,
    /// Contains at index `i` the byte that starts line `i+1`.
    lines: Box<[u32]>,
}

impl Filemap {
    fn file(&self) -> &[u8] {
        &self.file
    }

    fn lines(&self, span: Span) -> LineIter {
        let start = match self.lines.binary_search_by(|&l| l.cmp(&span.lo)) {
            Ok(l) => l,
            Err(l) => l - 1,
        };
        let end = match self.lines.binary_search_by(|&l| l.cmp(&span.hi)) {
            Ok(0) => 1,
            Ok(l) => l,
            Err(l) => l,
        };
        LineIter {
            src: self,
            start,
            end,
            start_idx: span.lo - self.lines[start],
            end_idx: span.hi - self.lines[end - 1],
        }
    }
}

#[derive(Copy, Clone)]
pub struct LineIter<'a> {
    src: &'a Filemap,
    start: usize,
    end: usize,
    start_idx: u32,
    end_idx: u32,
}

#[allow(clippy::len_without_is_empty)]
impl<'a> LineIter<'a> {
    pub fn len(&self) -> usize {
        self.end - self.start
    }

    pub fn start(&self) -> usize {
        self.start + 1
    }

    pub fn last(&self) -> usize {
        self.end
    }

    pub fn start_idx(&self) -> u32 {
        self.start_idx
    }

    pub fn last_idx(&self) -> u32 {
        self.end_idx - 1
    }
}

impl<'a> Iterator for LineIter<'a> {
    type Item = (u32, &'a [u8]);

    fn next(&mut self) -> Option<(u32, &'a [u8])> {
        if self.start == self.end {
            None
        } else {
            let base = self.src.lines[0];
            let lo = self.src.lines[self.start] - base;
            let hi = self.src.lines[self.start + 1] - base;
            let line = &self.src.src[lo as usize..hi as usize];
            self.start += 1;
            Some((self.start as u32, line))
        }
    }
}

pub struct Diagnostic {
    codemap: Codemap,
}

impl Diagnostic {
    pub fn new() -> Self {
        Self {
            codemap: Codemap::new(),
        }
    }

    pub fn add_src(&mut self, name: Rc<[u8]>, src: Rc<[u8]>) -> u32 {
        self.codemap.add_file(name, src)
    }

    fn common(&self, span: Span, prefix: &str, f: &str) {
        macro_rules! w { ($($tt:tt)*) => { eprint!($($tt)*) } }
        macro_rules! wl { ($($tt:tt)*) => { eprintln!($($tt)*) } }

        if span == Span::built_in() {
            w!("<built in> ");
            w!("{}{}", prefix, f);
            wl!("");
            return;
        }

        let file = self.codemap.file(span);
        let mut lines = file.lines(span);
        wl!(
            "{}:{}:{}: {}:{} {}{}",
            &String::from_utf8_lossy(file.file()),
            lines.start(),
            lines.start_idx() + 1,
            LineIter::last(&lines),
            lines.last_idx() + 1,
            prefix,
            f,
        );
        let len = lines.len();
        for (_, src) in &mut lines {
            w!("{} {}", ">>>", &String::from_utf8_lossy(src));
        }
        if len == 1 {
            w!("{} ", ">>>");
            for _ in 0..lines.start_idx() {
                w!(" ");
            }
            w!("{}", "^");
            for _ in lines.start_idx()..lines.last_idx() {
                w!("~");
            }
            wl!("");
        }
    }
}

impl Diagnostic {
    pub fn handle<H: FnOnce(&(dyn std::error::Error + 'static)) -> String>(
        &self,
        e: &Elang,
        msg: &Error,
        h: H,
    ) {
        let text = match msg.error {
            ErrorType::UnexpectedEndOfInput => format!("unexpected end of input"),
            ErrorType::UnexpectedToken(exp, act) => match exp {
                TokenAlternative::EndOfInput => format!(
                    "unexpected token. expected end of input, got `{}`",
                    act.as_str()
                ),
                TokenAlternative::StartOfExpression => format!(
                    "unexpected token. expected start of expression, got `{}`",
                    act.as_str()
                ),
                TokenAlternative::List(l) => {
                    let mut s = format!("unexpected token. expected ");
                    let _ = match l {
                        &[t] => write!(s, "`{}`", t.as_str()),
                        &[t1, t2] => write!(s, "`{}` or `{}`", t1.as_str(), t2.as_str()),
                        _ => {
                            for t in &l[..l.len() - 1] {
                                let _ = write!(&mut s, "`{}`, ", t.as_str());
                            }
                            write!(&mut s, " or `{}`", l.last().unwrap().as_str())
                        }
                    };
                    let _ = write!(s, ", got `{}`", act.as_str());
                    s
                }
            },
            ErrorType::OutOfBoundsLiteral => format!("out-of-bounds literal"),
            ErrorType::UnexpectedNumberSuffix(b) => format!(
                "unexpected number suffix {:?}",
                &String::from_utf8_lossy(&[b])
            ),
            ErrorType::UnexpectedByte(b) => format!("unexpected byte 0x{:02X}", b),
            ErrorType::MissingCodePoint => format!("missing code point"),
            ErrorType::InvalidCodePoint(i) => format!("invalid code point {}", i),
            ErrorType::UnknownEscapeSequence(b) => {
                format!("unknown escape sequence {:?}", b)
            }
            ErrorType::DuplicateIdentifier(id, prev) => {
                let s = e.get_interned(id);
                let txt =
                    format!("duplicate identifier `{}`", &String::from_utf8_lossy(&s));
                self.common(msg.span, "error: ", &txt);
                self.common(prev, "note: ", "previous declaration here");
                self.trace(e, msg);
                return;
            }
            ErrorType::UnexpectedExprType(expected, actual) => {
                let mut s = format!("unexpected expression type. expected ");
                let _ = match expected {
                    &[t] => write!(s, "`{}`", t.as_str()),
                    &[t1, t2] => write!(s, "`{}` or `{}`", t1.as_str(), t2.as_str()),
                    _ => {
                        for t in &expected[..expected.len() - 1] {
                            let _ = write!(&mut s, "`{}`, ", t.as_str());
                        }
                        write!(&mut s, " or `{}`", expected.last().unwrap().as_str())
                    }
                };
                let _ = write!(s, ", got `{}`", actual.as_str());
                s
            }
            ErrorType::MissingSetField(name) => {
                let s = e.get_interned(name);
                format!("missing set field `{}`", &String::from_utf8_lossy(&s))
            }
            ErrorType::MissingListField(ref n) => format!("missing list field {}", n),
            ErrorType::InfiniteRecursion(_) => format!("infinite recursion"),
            ErrorType::CannotForceExpr(ty) => {
                format!("cannot force expression of type `{}`", ty.as_str())
            }
            ErrorType::DivideByZero => format!("division by 0"),
            ErrorType::ExtraArgument(name, span) => {
                let s = e.get_interned(name);
                self.common(
                    msg.span,
                    "error: ",
                    &format!("extra argument `{}`", &String::from_utf8_lossy(&s)),
                );
                self.common(span, "note: ", "parameters declared here");
                self.trace(e, msg);
                return;
            }
            ErrorType::MissingArgument(name, span) => {
                let s = e.get_interned(name);
                self.common(
                    msg.span,
                    "error: ",
                    &format!("missing argument `{}`", &String::from_utf8_lossy(&s)),
                );
                self.common(span, "note: ", "parameter declared here");
                self.trace(e, msg);
                return;
            }
            ErrorType::MissingNewline => format!("missing newline"),
            ErrorType::SpanOverflow => format!("span overflow"),
            ErrorType::AssertionFailed => format!("assertion failed"),
            ErrorType::EmptyNumberLiteral => format!("empty number literal"),
            ErrorType::Custom(ref custom) => {
                let custom = &**custom;
                h(custom)
            }
            ErrorType::CannotStringifyNonInteger => {
                format!("cannot stringify numbers that are not integers")
            }
        };
        self.common(msg.span, "error: ", &text);
        self.trace(e, msg);
    }

    fn trace(&self, el: &Elang, msg: &Error) {
        for ctx in &msg.context {
            let s = |s| Span::new(s, s + 1);
            let e = |s| el.span(s);
            let p = |s| format!("while parsing {} starting here", s);
            let q = |s| format!("while evaluating {}", s);
            let (span, txt) = match *ctx {
                ErrorContext::ParseString(start) => (s(start), p("string")),
                ErrorContext::ParseUnicodeEscape(start) => {
                    (s(start), p("unicode escape"))
                }
                ErrorContext::ParseTest(start) => (s(start), p("test")),
                ErrorContext::ParseSelect(start) => (s(start), p("select")),
                ErrorContext::ParseParenthesized(start) => {
                    (s(start), p("parenthesized expression"))
                }
                ErrorContext::ParseFnHeader(start) => (s(start), p("function header")),
                ErrorContext::ParseFnPattern(start) => (s(start), p("function pattern")),
                ErrorContext::ParseLet(start) => (s(start), p("let")),
                ErrorContext::ParseField(start) => (s(start), p("field")),
                ErrorContext::ParseCond(start) => (s(start), p("cond")),
                ErrorContext::ParseList(start) => (s(start), p("list")),
                ErrorContext::ParseSet(start) => (s(start), p("set")),
                ErrorContext::ParseInherit(start) => (s(start), p("inherit")),
                ErrorContext::EvalResolved(eid) => (e(eid), format!("while evaluating")),
                ErrorContext::EvalArithmetic(eid) => (e(eid), q("arithmetic expression")),
                ErrorContext::EvalBool(eid) => (e(eid), q("boolean expression")),
                ErrorContext::EvalOverlay(eid) => (e(eid), q("overlay expression")),
                ErrorContext::EvalConcat(eid) => (e(eid), q("concat expression")),
                ErrorContext::EvalCond(eid) => (e(eid), q("conditional expression")),
                ErrorContext::EvalStringify(eid) => (e(eid), q("stringify expression")),
                ErrorContext::EvalApl(eid) => (e(eid), q("function application")),
                ErrorContext::EvalSelect(eid) => (e(eid), q("select expression")),
                ErrorContext::EvalOtherExprType(eid, ty) => (
                    e(eid),
                    format!("because this expression is a `{}`", ty.as_str()),
                ),
            };
            self.common(span, "note: ", &txt);
        }
    }
}
