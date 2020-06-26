use std::{
    fmt,
    fmt::{Debug, Formatter},
    ops::{Deref, DerefMut},
};
use std::borrow::Borrow;
use std::hash::{Hash, Hasher};

/// A span in a codemap.
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct Span {
    pub(crate) lo: u32,
    pub(crate) hi: u32,
}

impl Span {
    /// Creates a new span.
    pub fn new(lo: u32, hi: u32) -> Span {
        Span { lo, hi }
    }

    /// Creates a span that is associated with a location built into the compiler.
    ///
    /// = Remarks
    ///
    /// This span will be printed as `<built-in>` in diagnostic messages.
    pub fn built_in() -> Span {
        Span::new(!0 - 1, !0)
    }

    pub fn lo(self) -> u32 {
        self.lo
    }

    pub fn hi(self) -> u32 {
        self.hi
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{}", self.lo, self.hi)
    }
}

/// An object with a span.
#[derive(Copy, Clone, Eq)]
pub struct Spanned<T> {
    pub span: Span,
    pub val: T,
}

impl<T: PartialEq> PartialEq<Spanned<T>> for Spanned<T> {
    fn eq(&self, other: &Spanned<T>) -> bool {
        self.val == other.val
    }
}

impl<T: Hash> Hash for Spanned<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.val.hash(state)
    }
}

impl<T: Debug> Debug for Spanned<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "@{:?}: {:?}", self.span, self.val)
    }
}

impl<T> Borrow<T> for Spanned<T> {
    fn borrow(&self) -> &T {
        &self.val
    }
}

impl<T> Spanned<T> {
    pub fn new(span: Span, val: T) -> Spanned<T> {
        Spanned { span, val }
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.val
    }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.val
    }
}
