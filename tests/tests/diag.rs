use bstr::ByteSlice;
use console::{style, Color};
use elang::{util::codemap::{Codemap, LineIter}, Elang, Error, Span, ErrorType, TokenAlternative};
use std::{
    cell::RefCell,
    rc::Rc,
    fmt::Write,
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
            ErrorType::UnexpectedToken(exp, act) => {
                match exp {
                    TokenAlternative::EndOfInput => format!("unexpected token. expected end of input, got `{}`", act.as_str()),
                    TokenAlternative::StartOfExpression => format!("unexpected token. expected start of expression, got `{}`", act.as_str()),
                    TokenAlternative::List(l) => {
                        let mut s = format!("unexpected token. expected ");
                        match l {
                            &[t] => { write!(s, "`{}`", t.as_str()); },
                            &[t1, t2] => { write!(s, "`{}` or `{}`", t1.as_str(), t2.as_str()); },
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
            }
            ErrorType::OutOfBoundsLiteral => format!(""),
            ErrorType::OutOfBoundsSelector(_) => format!(""),
            ErrorType::UnexpectedIntegerSuffix(_) => format!(""),
            ErrorType::UnexpectedByte(_) => format!(""),
            ErrorType::MissingCodePoint => format!(""),
            ErrorType::InvalidCodePoint(_) => format!(""),
            ErrorType::UnknownEscapeSequence(_) => format!(""),
            ErrorType::DuplicateIdentifier(_, _) => format!(""),
            ErrorType::UnexpectedExpr(_, _) => format!(""),
            ErrorType::MissingSetField(_) => format!(""),
            ErrorType::MissingListField(_) => format!(""),
            ErrorType::InfiniteRecursion(_) => format!(""),
            ErrorType::CannotForceExpr(_) => format!(""),
            ErrorType::DivideByZero => format!(""),
            ErrorType::ExtraArgument(_) => format!(""),
            ErrorType::MissingArgument(_) => format!(""),
            ErrorType::MissingNewline => format!(""),
            ErrorType::SpanOverflow => format!(""),
            ErrorType::Overflow => format!(""),
            _ => format!(""),
        };
        self.common(msg.span, Color::Red, "error: ", &text);
    }
}
