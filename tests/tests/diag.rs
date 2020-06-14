use bstr::ByteSlice;
use console::{style, Color};
use elang::{
    util::codemap::{Codemap, LineIter},
    Elang, Error, MsgDetails, MsgLevel, Span,
};
use std::{
    cell::RefCell,
    fmt,
    fmt::{Display, Formatter},
    rc::Rc,
};

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

    fn common<F>(&self, span: Span, color: Color, prefix: &str, f: F)
    where
        F: Fn(&mut Formatter) -> fmt::Result,
    {
        struct Cb<F: Fn(&mut Formatter) -> fmt::Result>(F);
        impl<F: Fn(&mut Formatter) -> fmt::Result> Display for Cb<F> {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                self.0(f)
            }
        }

        macro_rules! w { ($($tt:tt)*) => { eprint!($($tt)*) } }
        macro_rules! wl { ($($tt:tt)*) => { eprintln!($($tt)*) } }

        if span == Span::built_in() {
            w!("<built in> ");
            w!("{}{}", style(prefix).bold().fg(color), style(Cb(f)).bold());
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
            style(Cb(f)).bold(),
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

pub enum Tmsg {
    Std(Error),
    Custom(Span, MsgLevel, String),
}

impl From<Error> for Tmsg {
    fn from(msg: Error) -> Self {
        Tmsg::Std(msg)
    }
}

impl TestDiag {
    pub fn handle(&self, msg: Tmsg) {
        let get_color = |l| match l {
            MsgLevel::Notice => (Color::Cyan, "notice: "),
            MsgLevel::Error => (Color::Red, "error: "),
        };
        let msg = match msg {
            Tmsg::Std(m) => m,
            Tmsg::Custom(span, level, m) => {
                let (color, prefix) = get_color(level);
                self.common(span, color, prefix, |w| write!(w, "{}", m));
                return;
            }
        };
        let (color, prefix) = get_color(msg.level);
        macro_rules! err {
            ($f:expr) => {
                self.common(msg.span, color, prefix, $f)
            };
        }
        match msg.details {
            MsgDetails::DivideByZero => {
                err!(|w| { write!(w, "attempted to divide by zero") })
            }
            MsgDetails::FoundExpr(ex, found) => err!(|w| {
                write!(
                    w,
                    "expected {}, found {:?}",
                    ex,
                    found.value().borrow().debug(&self.e)
                )
            }),
            MsgDetails::FoundToken(ex, found) => {
                err!(|w| { write!(w, "expected {}, found {:?}", ex, found.val) })
            }
            MsgDetails::FoundString(ex, found) => {
                err!(|w| { write!(w, "expected {}, found {}", ex, found) })
            }
            MsgDetails::FoundChar(ex, found) => {
                err!(|w| { write!(w, "expected {}, found {:?}", ex, found) })
            }
            MsgDetails::CannotStringify(ex) => err!(|w| {
                write!(
                    w,
                    "cannot stringify this expression: {:?}",
                    ex.value().borrow().debug(&self.e)
                )
            }),
            MsgDetails::InfiniteRecursion => err!(|w| write!(w, "infinite recursion")),
            MsgDetails::OutOfBounds => err!(|w| write!(w, "out of bounds")),
            MsgDetails::OverflowingLiteral => err!(|w| write!(w, "overflowing literal")),
            MsgDetails::InvalidCodepoint => err!(|w| write!(w, "invalid codepoint")),
            MsgDetails::UnknownEscapeSequence(c) => {
                err!(|w| write!(w, "unknown escape sequence: {:?}", c))
            }
            MsgDetails::SetHasNoField(id) => err!(|w| write!(
                w,
                "set has no field `{}`",
                self.e.get_interned(id).as_bstr()
            )),
            MsgDetails::ListHasNoField(id) => {
                err!(|w| write!(w, "list has no field `{}`", id))
            }
            MsgDetails::FnPattern(_) => {
                err!(|w| write!(w, "argument does not match function pattern"))
            }
            MsgDetails::FnMissingField(id) => err!(|w| write!(
                w,
                "missing field: `{}`",
                self.e.get_interned(id).as_bstr()
            )),
            MsgDetails::DupSetField(span) => {
                err!(|w| write!(w, "duplicate set field"));
                self.common(span, Color::Cyan, "note: ", |w| {
                    write!(w, "previous declaration")
                });
            }
            MsgDetails::AssertionFailed => err!(|w| write!(w, "assertion failed")),
            MsgDetails::Overflow => err!(|w| write!(w, "overflow")),
            _ => err!(|w| write!(w, "unexpected error")),
        }
    }
}
