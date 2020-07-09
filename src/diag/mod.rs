use crate::{
    diag::code_map::{CodeMap, Lines},
    util::str::Utf8Lossy,
    Elang, Error, ErrorType, Span, TokenAlternative,
};
use std::{
    fmt,
    fmt::{Display, Formatter, Write},
    rc::Rc,
};
use crate::types::error::ErrorContext;
use num_rational::BigRational;

mod code_map;

/// A util for generating diagnostic messages
pub struct Diagnostic {
    code_map: CodeMap,
}

struct Unprocessed {
    span: Span,
    msg: Rc<str>,
    children: Vec<Unprocessed>,
}

struct Processed {
    location: Location,
    msg: Rc<str>,
    max_hints_len: u32,
    d_lines: Vec<DLine>,
    max_line_num: u32,
    children: Vec<Processed>,
}

enum Location {
    BuiltIn,
    Invalid,
    Source {
        name: Rc<str>,
        first_line: u32,
        first_column: u32,
        last_line: u32,
        last_column: u32,
    },
}

enum DLine {
    Text {
        line: u32,
        content: Rc<str>,
    },
    Hint {
        first_column: u32,
        last_column: u32,
        text: Option<Rc<str>>,
    },
    HintStart {
        hint_idx: u32,
        first_column: u32,
    },
    HintStop {
        hint_idx: u32,
        last_column: u32,
        text: Option<Rc<str>>,
    },
}

impl Diagnostic {
    /// Creates a new diagnostic object
    pub fn new() -> Self {
        Self {
            code_map: CodeMap::new(),
        }
    }

    /// Adds a source unit
    ///
    /// `name` is the name that will be displayed next to diagnostic messages referring
    /// to this source unit.
    ///
    /// If adding this source unit would cause the total amount of source code in this
    /// diagnostic object to be larger than `u32::max_value() - 1`, `None` is returned.
    ///
    /// Otherwise the base for spans referring to this source unit is returned.
    pub fn add_src(&mut self, name: &[u8], src: &[u8]) -> Option<u32> {
        self.code_map.add_source(&name, src)
    }

    fn process(&self, unprocessed: &Unprocessed) -> Processed {
        let children = unprocessed
            .children
            .iter()
            .map(|c| self.process(c))
            .collect();
        let span = unprocessed.span;
        if unprocessed.span == Span::built_in() {
            return Processed {
                location: Location::BuiltIn,
                msg: unprocessed.msg.clone(),
                max_hints_len: 0,
                d_lines: vec![],
                max_line_num: 0,
                children,
            };
        }
        let Lines {
            name,
            start_no,
            lines,
        } = match self.code_map.get_lines(unprocessed.span) {
            Some(l) => l,
            _ => {
                return Processed {
                    location: Location::Invalid,
                    msg: unprocessed.msg.clone(),
                    max_hints_len: 0,
                    d_lines: vec![],
                    max_line_num: 0,
                    children,
                }
            }
        };
        let mut max_hints_len = 0;
        let mut active_hints = 0;
        let location = Location::Source {
            name,
            first_line: start_no,
            first_column: lines[0].mono_offset(span.lo) + 1,
            last_line: start_no + lines.len() as u32 - 1,
            last_column: lines.last().unwrap().mono_offset(span.hi),
        };
        let mut d_lines = vec![];
        for (line_off, line) in lines.iter().enumerate() {
            d_lines.push(DLine::Text {
                line: start_no + line_off as u32,
                content: line.content.clone(),
            });
            if line.lo <= span.lo && span.lo < line.hi {
                let first_column = line.mono_offset(span.lo) + 1;
                if span.hi <= line.hi {
                    let last_column = line.mono_offset(span.hi - 1) + 1;
                    d_lines.push(DLine::Hint {
                        first_column,
                        last_column,
                        text: None,
                    });
                } else {
                    active_hints += 1;
                    max_hints_len = max_hints_len.max(active_hints);
                    d_lines.push(DLine::HintStart {
                        hint_idx: 0,
                        first_column,
                    });
                }
            } else if line.lo <= span.hi && span.hi < line.hi {
                let last_column = line.mono_offset(span.hi - 1) + 1;
                active_hints -= 1;
                d_lines.push(DLine::HintStop {
                    hint_idx: 0,
                    last_column,
                    text: None,
                });
            }
        }
        Processed {
            location,
            msg: unprocessed.msg.clone(),
            max_hints_len,
            d_lines,
            max_line_num: start_no + lines.len() as u32,
            children,
        }
    }

    /// Transforms an error into an object that can be displayed
    pub fn display(&self, e: &Elang, error: &Error) -> impl Display + 'static {
        let mut children = vec![];

        let text = match error.error {
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
                children.push(Unprocessed {
                    span: previous_declaration.span,
                    msg: "note: previous declaration here"
                        .to_string()
                        .into_boxed_str()
                        .into(),
                    children: vec![],
                });

                let s = e.get_interned(*previous_declaration);
                format!("duplicate identifier `{}`", &String::from_utf8_lossy(&s))
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
                format!("missing list field {}", BigRational::from((**index).clone()))
            }
            ErrorType::InfiniteRecursion { .. } => format!("infinite recursion"),
            ErrorType::CannotEvaluateExpr { kind } => {
                format!("cannot force expression of type `{}`", kind.as_str())
            }
            ErrorType::DivideByZero => format!("division by 0"),
            ErrorType::MissingArgument { missing_parameter } => {
                children.push(Unprocessed {
                    span: missing_parameter.span,
                    msg: "note: parameter declared here"
                        .to_string()
                        .into_boxed_str()
                        .into(),
                    children: vec![],
                });
                let s = e.get_interned(*missing_parameter);
                format!("missing argument `{}`", &String::from_utf8_lossy(&s))
            }
            ErrorType::SpanOverflow => format!("span overflow"),
            ErrorType::AssertionFailed { msg } => {
                let msg = e.store.get_str(msg);
                format!("assertion failed: {}", Utf8Lossy::from_bytes(&msg))
            }
            ErrorType::EmptyNumberLiteral => format!("empty number literal"),
            ErrorType::Custom { ref error } => format!("{}", error),
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
        self.trace(e, error, &mut children);
        self.process(&Unprocessed {
            span: error.span,
            msg: format!("error: {}", text).into_boxed_str().into(),
            children,
        })
    }

    fn trace(&self, el: &Elang, msg: &Error, dst: &mut Vec<Unprocessed>) {
        for ctx in &msg.context {
            let s = |s| Span::new(s, s + 1);
            let e = |s| el.span(s);
            let p = |s| format!("while parsing {} starting here", s);
            let q = |s| format!("while evaluating this {}", s);
            let (span, txt) = match *ctx {
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
            dst.push(Unprocessed {
                span,
                msg: format!("note: {}", txt).into_boxed_str().into(),
                children: vec![],
            });
        }
    }
}

impl Display for Processed {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.format(f, 0)
    }
}

impl Processed {
    fn format_new_hints(
        f: &mut Formatter<'_>,
        hints: &Vec<bool>,
        hint_idx: u32,
        c: char,
    ) -> fmt::Result {
        for (idx, &h) in hints.iter().enumerate() {
            let idx = idx as u32;
            if idx == hint_idx {
                write!(f, "{}", c)?;
            } else if idx < hint_idx {
                match h {
                    true => write!(f, "│"),
                    _ => write!(f, " "),
                }?
            } else {
                match h {
                    true => write!(f, "┼"),
                    _ => write!(f, "─"),
                }?
            }
        }
        Ok(())
    }

    fn format_old_hints(f: &mut Formatter<'_>, hints: &Vec<bool>) -> fmt::Result {
        for &h in hints {
            match h {
                true => write!(f, "│"),
                _ => write!(f, " "),
            }?
        }
        Ok(())
    }

    fn format(&self, f: &mut Formatter<'_>, indentation_: u32) -> fmt::Result {
        let indentation = Repeat(" ", indentation_);
        write!(f, "{}", indentation)?;
        match self.location {
            Location::BuiltIn => write!(f, "<built-in>"),
            Location::Invalid => write!(f, "<invalid-span>"),
            Location::Source {
                ref name,
                first_line,
                first_column,
                last_line,
                last_column,
            } => write!(
                f,
                "{}:{}:{}: {}:{}",
                name, first_line, first_column, last_line, last_column
            ),
        }?;
        writeln!(f, ": {}", self.msg)?;
        let line_num_width = format!("{}", self.max_line_num).len() as u32;
        let mut hints: Vec<_> = (0..self.max_hints_len).map(|_| false).collect();
        for line in &self.d_lines {
            write!(f, "{}", indentation)?;
            match line {
                DLine::Text { line, content } => {
                    write!(f, "{:>width$} │", line, width = line_num_width as usize)?;
                    Self::format_old_hints(f, &hints)?;
                    writeln!(f, " {}", content)?
                }
                DLine::Hint {
                    first_column,
                    last_column,
                    text,
                } => {
                    write!(f, "{} │", Repeat(" ", line_num_width))?;
                    Self::format_old_hints(f, &hints)?;
                    write!(
                        f,
                        "{}^{}",
                        Repeat(" ", *first_column),
                        Repeat("~", *last_column - *first_column),
                    )?;
                    if let Some(ref t) = text {
                        write!(f, " {}", t)?;
                    }
                    writeln!(f)?;
                }
                DLine::HintStart {
                    hint_idx,
                    first_column,
                } => {
                    write!(f, "{} │", Repeat(" ", line_num_width))?;
                    Self::format_new_hints(f, &hints, *hint_idx, '┌')?;
                    hints[*hint_idx as usize] = true;
                    writeln!(f, "{}┘", Repeat("─", *first_column))?;
                }
                DLine::HintStop {
                    hint_idx,
                    last_column,
                    text,
                } => {
                    write!(f, "{} │", Repeat(" ", line_num_width))?;
                    Self::format_new_hints(f, &hints, *hint_idx, '└')?;
                    hints[*hint_idx as usize] = false;
                    write!(f, "{}┘", Repeat("─", *last_column))?;
                    if let Some(ref t) = text {
                        write!(f, " {}", t)?;
                    }
                    writeln!(f)?;
                }
            }
        }
        for child in &self.children {
            child.format(f, 0)?
        }
        Ok(())
    }
}

struct Repeat(&'static str, u32);

impl Display for Repeat {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for _ in 0..self.1 {
            self.0.fmt(f)?;
        }
        Ok(())
    }
}
