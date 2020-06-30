use crate::{
    util::str::Utf8Lossy, Elang, Error, ErrorContext, ErrorType, Span, TokenAlternative,
};
use std::{fmt::Write, rc::Rc};

mod code_map;

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
            ErrorType::UnexpectedToken {
                expected,
                encountered,
            } => match expected {
                TokenAlternative::EndOfInput => format!(
                    "unexpected token. expected end of input, got `{}`",
                    encountered.as_str()
                ),
                TokenAlternative::StartOfExpression => format!(
                    "unexpected token. expected start of expression, got `{}`",
                    encountered.as_str()
                ),
                TokenAlternative::List { candidates } => {
                    let mut s = format!("unexpected token. expected ");
                    let _ = match candidates {
                        &[t] => write!(s, "`{}`", t.as_str()),
                        &[t1, t2] => write!(s, "`{}` or `{}`", t1.as_str(), t2.as_str()),
                        _ => {
                            for t in &candidates[..candidates.len() - 1] {
                                let _ = write!(&mut s, "`{}`, ", t.as_str());
                            }
                            write!(
                                &mut s,
                                " or `{}`",
                                candidates.last().unwrap().as_str()
                            )
                        }
                    };
                    let _ = write!(s, ", got `{}`", encountered.as_str());
                    s
                }
            },
            ErrorType::OutOfBoundsLiteral => format!("out-of-bounds literal"),
            ErrorType::InvalidDigit { digit } => {
                format!("unexpected digit {:?}", digit as char)
            }
            ErrorType::InvalidByteForTokenStart { byte } => {
                format!("expected token, found byte {:?}", byte as char)
            }
            ErrorType::MissingCodePoint => format!("missing code point"),
            ErrorType::InvalidCodePoint { code_point } => {
                format!("invalid code point U+{:X}", code_point)
            }
            ErrorType::UnknownEscapeSequence { escape_sequence } => {
                format!("unknown escape sequence {:?}", escape_sequence as char)
            }
            ErrorType::DuplicateIdentifier {
                previous_declaration,
            } => {
                let s = e.get_interned(*previous_declaration);
                let txt =
                    format!("duplicate identifier `{}`", &String::from_utf8_lossy(&s));
                self.common(msg.span, "error: ", &txt);
                self.common(
                    previous_declaration.span,
                    "note: ",
                    "previous declaration here",
                );
                self.trace(e, msg);
                return;
            }
            ErrorType::UnexpectedExprKind {
                expected,
                encountered,
            } => {
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
                let _ = write!(s, ", got `{}`", encountered.as_str());
                s
            }
            ErrorType::MissingMapField { field_name } => {
                let s = e.get_interned(field_name);
                format!("missing map field `{}`", &String::from_utf8_lossy(&s))
            }
            ErrorType::MissingListField { ref index } => {
                format!("missing list field {}", index)
            }
            ErrorType::InfiniteRecursion { .. } => format!("infinite recursion"),
            ErrorType::CannotEvaluateExpr { kind } => {
                format!("cannot force expression of type `{}`", kind.as_str())
            }
            ErrorType::DivideByZero => format!("division by 0"),
            ErrorType::MissingArgument { missing_parameter } => {
                let s = e.get_interned(*missing_parameter);
                self.common(
                    msg.span,
                    "error: ",
                    &format!("missing argument `{}`", &String::from_utf8_lossy(&s)),
                );
                self.common(missing_parameter.span, "note: ", "parameter declared here");
                self.trace(e, msg);
                return;
            }
            ErrorType::SpanOverflow => format!("span overflow"),
            ErrorType::AssertionFailed { msg } => {
                let msg = e.store.get_str(msg);
                format!("assertion failed: {}", Utf8Lossy::from_bytes(&msg))
            }
            ErrorType::EmptyNumberLiteral => format!("empty number literal"),
            ErrorType::Custom { ref error } => {
                let custom = &**error;
                h(custom)
            }
            ErrorType::CannotStringifyNonInteger => {
                format!("cannot stringify numbers that are not integers")
            }
            ErrorType::UnmatchedToken { kind } => {
                format!("unmatched token `{}`", kind.as_str())
            }
            ErrorType::UnexpectedByte {
                expected,
                encountered,
            } => {
                let mut s = format!("unexpected bytes. expected ");
                let _ = match expected {
                    &[t] => write!(s, "{:?}", t as char),
                    &[t1, t2] => write!(s, "{:?} or {:?}", t1 as char, t2 as char),
                    _ => {
                        for &t in &expected[..expected.len() - 1] {
                            let _ = write!(&mut s, "{:?}, ", t as char);
                        }
                        write!(
                            &mut s,
                            " or {:?}",
                            expected.last().copied().unwrap() as char
                        )
                    }
                };
                let _ = write!(s, ", got {:?}", encountered as char);
                s
            }
            ErrorType::Raised { msg } => {
                let msg = e.store.get_str(msg);
                format!("an error was raised: {}", Utf8Lossy::from_bytes(&msg))
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
            let q = |s| format!("while evaluating this {}", s);
            let (span, txt) = match *ctx {
                ErrorContext::ParseString { start } => (s(start), p("string")),
                ErrorContext::ParseUnicodeEscape { start } => {
                    (s(start), p("unicode escape"))
                }
                ErrorContext::ParseTest { start } => (s(start), p("test")),
                ErrorContext::ParseSelect { start } => (s(start), p("select")),
                ErrorContext::ParseParenthesized { start } => {
                    (s(start), p("parenthesized expression"))
                }
                ErrorContext::ParseFnHeader { start } => (s(start), p("function header")),
                ErrorContext::ParseFnPattern { start } => {
                    (s(start), p("function pattern"))
                }
                ErrorContext::ParseLet { start } => (s(start), p("let")),
                ErrorContext::ParseField { start } => (s(start), p("field")),
                ErrorContext::ParseCond { start } => (s(start), p("cond")),
                ErrorContext::ParseList { start } => (s(start), p("list")),
                ErrorContext::ParseMap { start } => (s(start), p("map")),
                ErrorContext::ParseInherit { start } => (s(start), p("inherit")),
                ErrorContext::EvalResolved { pointer } => {
                    (e(pointer), format!("while evaluating"))
                }
                ErrorContext::EvalArithmetic { arithmetic_expr } => {
                    (e(arithmetic_expr), q("arithmetic expression"))
                }
                ErrorContext::EvalBool { boolean_expr } => {
                    (e(boolean_expr), q("boolean expression"))
                }
                ErrorContext::EvalOverlay { overlay_expr } => {
                    (e(overlay_expr), q("overlay expression"))
                }
                ErrorContext::EvalAdd { add_expr } => (e(add_expr), q("add expression")),
                ErrorContext::EvalCond { cond_expr } => (e(cond_expr), q("condition")),
                ErrorContext::EvalStringify { stringify_expr } => {
                    (e(stringify_expr), q("string interpolation"))
                }
                ErrorContext::EvalApl { apl_expr } => {
                    (e(apl_expr), q("function application"))
                }
                ErrorContext::EvalSelect { select_expr } => {
                    (e(select_expr), q("select expression"))
                }
                ErrorContext::EvalFnPat { fn_pat_span } => {
                    (fn_pat_span, format!("while matching this function pattern"))
                }
                ErrorContext::EvalOtherExprKind {
                    other_expr,
                    other_expr_kind,
                } => (
                    e(other_expr),
                    format!(
                        "because this expression is a `{}`",
                        other_expr_kind.as_str()
                    ),
                ),
            };
            self.common(span, "note: ", &txt);
        }
    }
}
