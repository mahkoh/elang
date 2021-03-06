use crate::{util::str::Utf8Lossy, Span};
use std::{cell::RefCell, convert::TryInto, rc::Rc};
use unicode_width::UnicodeWidthChar;

pub struct CodeMap {
    source_units: Vec<SourceUnit>,
}

struct SourceUnit {
    name: Rc<str>,
    lo: u32,
    hi: u32,
    unit: RefCell<SourceUnit_>,
}

enum SourceUnit_ {
    Unprocessed(Rc<[u8]>),
    Processed(Box<[Line]>),
}

pub struct Lines {
    pub name: Rc<str>,
    pub start_no: u32,
    pub lines: Box<[Line]>,
}

#[derive(Clone, Debug)]
pub struct Line {
    pub lo: u32,
    pub hi: u32,
    pub mono_offsets: Rc<[(u32, u32)]>,
    pub content: Rc<str>,
}

impl Line {
    pub fn mono_offset(&self, lo: u32) -> u32 {
        let mut o = lo - self.lo;
        for &(olo, mono_pos) in self.mono_offsets.iter().rev() {
            if olo <= lo {
                o = mono_pos + lo - olo;
                break;
            }
        }
        o
    }
}

impl CodeMap {
    pub fn new() -> Self {
        Self {
            source_units: vec![],
        }
    }

    pub fn add_source(&mut self, name: &[u8], src: &[u8]) -> Option<u32> {
        let src: Rc<[u8]> = if src.last().copied() != Some(b'\n') {
            let mut copy = src.to_vec();
            copy.push(b'\n');
            copy.into_boxed_slice().into()
        } else {
            src.to_vec().into_boxed_slice().into()
        };
        let lo = self.source_units.last().map(|u| u.hi).unwrap_or(0);
        let len: u32 = match src.len().try_into() {
            Ok(l) => l,
            _ => return None,
        };
        if lo as u64 + len as u64 > u32::max_value() as u64 - 1 {
            return None;
        }
        let hi = lo + len;
        self.source_units.push(SourceUnit {
            name: format!("{}", Utf8Lossy::from_bytes(name))
                .into_boxed_str()
                .into(),
            lo,
            hi,
            unit: RefCell::new(SourceUnit_::Unprocessed(src)),
        });
        Some(lo)
    }

    pub fn get_lines(&self, span: Span) -> Option<Lines> {
        for unit in &self.source_units {
            if unit.lo <= span.lo && span.hi <= unit.hi {
                return Some(unit.get_lines(span.lo, span.hi));
            }
        }
        None
    }
}

impl SourceUnit {
    fn get_lines(&self, lo: u32, hi: u32) -> Lines {
        let mut unit = self.unit.borrow_mut();
        let mut lines = unit.process(self.lo);
        let mut iter = lines.iter().enumerate();
        let start_no;
        loop {
            let (pos, line) = match iter.next() {
                Some(n) => n,
                _ => {
                    start_no = lines.len();
                    lines = &[];
                    break;
                }
            };
            if lo < line.hi {
                start_no = pos + 1;
                lines = &lines[pos..];
                break;
            }
        }
        for (pos, line) in lines.iter().enumerate() {
            if hi <= line.hi {
                lines = &lines[..pos + 1];
                break;
            }
        }
        Lines {
            name: self.name.clone(),
            start_no: start_no as u32,
            lines: lines.to_vec().into_boxed_slice(),
        }
    }
}

impl SourceUnit_ {
    fn process(&mut self, lo: u32) -> &[Line] {
        let up = match *self {
            SourceUnit_::Unprocessed(ref up) => up,
            SourceUnit_::Processed(ref l) => return l,
        };
        let mut up = &**up;
        let mut lines = vec![];
        let mut lo_line = lo;
        while up.len() > 0 {
            let len = up.iter().position(|&c| c == b'\n').unwrap();
            let hi_line = lo_line + len as u32 + 1;
            let mut line_pos = lo;
            let mut mono_pos = 0;
            let mut mono_offsets = vec![];
            let mut content = String::new();
            let line = if len > 0 && up[len - 1] == b'\r' {
                &up[..len - 1]
            } else {
                &up[..len]
            };
            for chunk in Utf8Lossy::from_bytes(line).chunks() {
                content.push_str(chunk.valid);
                for c in chunk.valid.chars() {
                    let mono_width = c.width().unwrap_or(0) as u32;
                    let char_len = c.len_utf8() as u32;
                    for i in 1..char_len {
                        mono_offsets.push((line_pos + i, mono_pos));
                    }
                    mono_pos += mono_width;
                    line_pos += char_len;
                }
                if chunk.broken.len() > 0 {
                    content.push(std::char::REPLACEMENT_CHARACTER);
                    for i in 1..chunk.broken.len() {
                        mono_offsets.push((line_pos + i as u32, mono_pos));
                    }
                    mono_pos += 1;
                    line_pos += chunk.broken.len() as u32;
                }
            }
            lines.push(Line {
                lo: lo_line,
                hi: hi_line,
                mono_offsets: mono_offsets.into_boxed_slice().into(),
                content: content.into_boxed_str().into(),
            });
            lo_line = hi_line;
            up = &up[len + 1..];
        }
        *self = SourceUnit_::Processed(lines.into_boxed_slice());
        match self {
            SourceUnit_::Processed(ref line) => line,
            _ => unreachable!(),
        }
    }
}
