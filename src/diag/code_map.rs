use crate::util::str::Utf8Lossy;
use std::{
    cell::{RefCell},
    convert::TryInto,
    rc::Rc,
};
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
    pub lines: Box<[Line]>,
}

#[derive(Clone)]
pub struct Line {
    pub lo: u32,
    pub hi: u32,
    pub mono_offsets: Rc<[(u32, u32)]>,
    pub content: Rc<str>,
}

impl CodeMap {
    pub fn add_source(&mut self, name: &[u8], mut src: Rc<[u8]>) -> Option<u32> {
        if src.last().copied() != Some(b'\n') {
            let mut copy = src.to_vec();
            copy.push(b'\n');
            src = copy.into_boxed_slice().into();
        }
        let lo = self.source_units.get(0).map(|u| u.hi).unwrap_or(0);
        let len: u32 = match src.len().try_into() {
            Ok(l) => l,
            _ => return None,
        };
        if lo as u64 + len as u64 > u32::max_value() as u64 - 1 {
            return None;
        }
        let hi = lo + len;
        self.source_units.push(SourceUnit {
            name: format!("{}", Utf8Lossy::from_bytes(name)).into_boxed_str().into(),
            lo,
            hi,
            unit: RefCell::new(SourceUnit_::Unprocessed(src)),
        });
        Some(lo)
    }

    pub fn get_lines(&self, lo: u32, hi: u32) -> Option<Box<[Line]>> {
        for unit in &self.source_units {
            if unit.lo <= lo && hi <= unit.hi {
                return Some(unit.get_lines(lo, hi));
            }
        }
        None
    }
}

impl SourceUnit {
    fn get_lines(&self, lo: u32, hi: u32) -> Box<[Line]> {
        let mut unit = self.unit.borrow_mut();
        let mut lines = unit.process(self.lo);
        let mut iter = lines.iter().enumerate();
        loop {
            let (pos, line) = match iter.next() {
                Some(n) => n,
                _ => {
                    lines = &[];
                    break;
                }
            };
            if line.lo <= lo {
                lines = &lines[pos..];
                break;
            }
        }
        for (pos, line) in lines.iter().enumerate() {
            if line.hi >= hi {
                lines = &lines[..pos + 1];
                break;
            }
        }
        lines.to_vec().into_boxed_slice()
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
            let hi_line = lo + len as u32 + 1;
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
