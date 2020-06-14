use crate::types::span::Span;
use std::{cell::Cell, rc::Rc};

/// A codemap.
///
/// = Remarks
///
/// This type is used to map spans to source code lines.
pub struct Codemap {
    files: Cell<Vec<Filemap>>,
}

impl Codemap {
    /// Creates a new, empty codemap.
    pub fn new() -> Codemap {
        Codemap {
            files: Cell::new(Vec::new()),
        }
    }

    #[allow(clippy::mut_from_ref)]
    fn inner(&self) -> &mut Vec<Filemap> {
        unsafe { &mut *self.files.as_ptr() }
    }

    /// Adds a file to the codemap.
    ///
    /// [argument, name]
    /// The name of the file.
    ///
    /// [argument, src]
    /// The contents of the file.
    pub fn add_file(&self, name: Rc<[u8]>, src: Rc<[u8]>) -> u32 {
        let files = self.inner();
        let old_pos = match files.last() {
            Some(f) => *f.lines.last().unwrap(),
            _ => 0,
        };
        let mut cur_pos = old_pos;
        let mut lines = Vec::new();
        lines.push(cur_pos);
        {
            let mut src = &*src;
            while src.len() > 0 {
                let pos = match src.iter().position(|&c| c == b'\n') {
                    Some(pos) => pos + 1,
                    _ => src.len(),
                };
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
        files.push(map);
        old_pos
    }

    /// Retrieves the file associated with a span.
    pub fn file(&self, span: Span) -> &Filemap {
        let files = self.inner();
        for file in &*files {
            if file.lines[file.lines.len() - 1] > span.lo {
                return file;
            }
        }
        &files[files.len() - 1]
    }
}

/// A file map.
///
/// = Remarks
///
/// This is where the real work happens.
pub struct Filemap {
    file: Rc<[u8]>,
    src: Rc<[u8]>,
    /// Contains at index `i` the byte that starts line `i+1`.
    lines: Box<[u32]>,
}

impl Filemap {
    /// Returns the name of the file represented by this map.
    pub fn file(&self) -> &[u8] {
        &self.file
    }

    /// Returns an iterator over the lines crossed by a span.
    pub fn lines(&self, span: Span) -> LineIter {
        let start = match self.lines.binary_search_by(|&l| l.cmp(&span.lo)) {
            Ok(l) => l,
            Err(l) => l - 1,
        };
        let end = match self.lines.binary_search_by(|&l| l.cmp(&span.hi)) {
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

/// An iterator over file lines.
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
    /// The number of lines.
    pub fn len(&self) -> usize {
        self.end - self.start
    }

    /// The first line.
    pub fn start(&self) -> usize {
        self.start + 1
    }

    /// The last line.
    pub fn last(&self) -> usize {
        self.end
    }

    /// The column of the first byte in the span.
    pub fn start_idx(&self) -> u32 {
        self.start_idx
    }

    /// The column of the last byte in the span.
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
