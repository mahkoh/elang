use bstr::ByteSlice;
use console::{style, Color};
use elang::{util::codemap::{Codemap, LineIter}, Elang, Error, ErrorType, Span, TokenAlternative, ErrorContext};
use std::{cell::RefCell, fmt::Write, rc::Rc};

#[derive(Clone)]
pub struct TestDiag {
    codemap: Rc<RefCell<Codemap>>,
    e: Rc<Elang>,
    is_term: bool,
}

impl TestDiag {
    pub fn new(codemap: Rc<RefCell<Codemap>>, e: Rc<Elang>) -> Self {
        Self {
            codemap,
            e,
            is_term: atty::is(atty::Stream::Stderr),
        }
    }

    fn common(&self, span: Span, color: Color, prefix: &str, f: &str) {
        macro_rules! w { ($($tt:tt)*) => { eprint!($($tt)*) } }
        macro_rules! wl { ($($tt:tt)*) => { eprintln!($($tt)*) } }

        if span == Span::built_in() {
            w!("<built in> ");
            w!("{}{}", style(prefix).bold().fg(color), style(f).bold());
            wl!("");
            return;
        }

        let codemap = self.codemap.borrow();
        let file = codemap.file(span);
        let mut lines = file.lines(span);
        wl!(
            "{}:{}:{}: {}:{} {}{}",
            file.file().as_bstr(),
            lines.start(),
            lines.start_idx() + 1,
            LineIter::last(&lines),
            lines.last_idx() + 1,
            style(prefix).bold().fg(color),
            style(f).bold(),
        );
        let len = lines.len();
        for (_, src) in &mut lines {
            w!("{} {}", style(">>>").bold().black(), src.as_bstr());
        }
        if len == 1 {
            w!("{} ", style(">>>").bold().black());
            for _ in 0..lines.start_idx() {
                w!(" ");
            }
            w!("{}", style("^").bold().fg(color));
            for _ in lines.start_idx()..lines.last_idx() {
                w!("{}", style("~").bold().fg(color));
            }
            wl!("");
        }
    }
}

impl TestDiag {
    pub fn handle(&self, msg: &Error) {
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
                    match l {
                        &[t] => {
                            write!(s, "`{}`", t.as_str());
                        }
                        &[t1, t2] => {
                            write!(s, "`{}` or `{}`", t1.as_str(), t2.as_str());
                        }
                        _ => {
                            for t in &l[..l.len() - 1] {
                                write!(&mut s, "`{}`, ", t.as_str());
                            }
                            write!(&mut s, " or `{}`", l.last().unwrap().as_str());
                        }
                    }
                    write!(s, ", got `{}`", act.as_str());
                    s
                }
            }
            ErrorType::OutOfBoundsLiteral => format!("out-of-bounds literal"),
            ErrorType::OutOfBoundsSelector(i) => format!("out-of-bounds selector: {}", i),
            ErrorType::UnexpectedIntegerSuffix(b) => format!("unexpected integer suffix {:?}", b),
            ErrorType::UnexpectedByte(b) => format!("unexpected byte {:?}", b),
            ErrorType::MissingCodePoint => format!("missing code point"),
            ErrorType::InvalidCodePoint(i) => format!("invalid code point {}", i),
            ErrorType::UnknownEscapeSequence(b) => format!("unknown escape sequence {:?}", b),
            ErrorType::DuplicateIdentifier(id, prev) => {
                let s = self.e.get_interned(id);
                let txt = format!("duplicate identifier `{}`", s.as_bstr());
                self.common(msg.span, Color::Red, "error: ", &txt);
                self.common(prev, Color::Cyan, "note: ", "previous declaration here");
                self.trace(msg);
                return;
            },
            ErrorType::UnexpectedExpr(expected, actual) => {
                let mut s = format!("unexpected expression. expected ");
                match expected {
                    &[t] => { write!(s, "`{}`", t.as_str()); },
                    &[t1, t2] => { write!(s, "`{}` or `{}`", t1.as_str(), t2.as_str()); },
                    _ => {
                        for t in &expected[..expected.len() - 1] {
                            write!(&mut s, "`{}`, ", t.as_str());
                        }
                        write!(&mut s, " or `{}`", expected.last().unwrap().as_str());
                    }
                }
                write!(s, ", got `{}`", actual.as_str());
                s
            },
            ErrorType::MissingSetField(name) => {
                let s = self.e.get_interned(name);
                format!("missing set field `{}`", s.as_bstr())
            },
            ErrorType::MissingListField(n) => format!("missing list field {}", n),
            ErrorType::InfiniteRecursion(_) => format!("infinite recursion"),
            ErrorType::CannotForceExpr(ty) => format!("cannot force expression of type `{}`", ty.as_str()),
            ErrorType::DivideByZero => format!("division by 0"),
            ErrorType::ExtraArgument(name) => {
                let s = self.e.get_interned(name);
                format!("extra argument `{}`", s.as_bstr())
            },
            ErrorType::MissingArgument(name) =>{
                let s = self.e.get_interned(name);
                format!("missing argument `{}`", s.as_bstr())
            },
            ErrorType::MissingNewline => format!("missing newline"),
            ErrorType::SpanOverflow => format!("span overflow"),
            ErrorType::Overflow => format!("overflow"),
            _ => format!(""),
        };
        self.common(msg.span, Color::Red, "error: ", &text);
        self.trace(msg);
    }

    fn trace(&self, msg: &Error) {
        for ctx in &msg.context {
            let s = |s| Span::new(s, s + 1);
            let e = |s| self.e.span(s);
            let p = |s| format!("while parsing {} starting here", s);
            let q = |s| format!("while evaluating {}", s);
            let (span, txt) = match *ctx {
                ErrorContext::ParseString(start) => (s(start), p("string")),
                ErrorContext::ParseUnicodeEscape(start) => (s(start), p("unicode escape")),
                ErrorContext::ParseTest(start) => (s(start), p("test")),
                ErrorContext::ParseSelect(start) => (s(start), p("select")),
                ErrorContext::ParseParenthesized(start) => (s(start), p("parenthesized expression")),
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
                _ => unreachable!(),
            };
            self.common(span, Color::Cyan, "note: ", &txt);
        }
    }
}
