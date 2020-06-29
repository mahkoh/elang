use std::{
    borrow::Borrow,
    fmt,
    fmt::{Debug, Formatter},
    hash::{Hash, Hasher},
    ops::{Deref, DerefMut},
};

/// A span
///
/// A span represents a section of source code. Each expression has an associated span that
/// represents where the expression originates. Spans are only used for diagnostic purposes.
///
/// A span should never be empty and should never cross more than one piece of source code.
///
/// As a special case, the built-in span is used for expressions that are not backed by source code.
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct Span {
    pub(crate) lo: u32,
    pub(crate) hi: u32,
}

impl Span {
    /// Creates a new span
    ///
    /// # Panics
    ///
    /// This function panics if `lo >= hi`.
    pub fn new(lo: u32, hi: u32) -> Span {
        assert!(lo < hi);
        Span { lo, hi }
    }

    /// Returns the span that represents a built-in expression
    pub fn built_in() -> Span {
        Span::new(!0 - 1, !0)
    }

    /// Returns the start of the span
    pub fn lo(self) -> u32 {
        self.lo
    }

    /// Returns the end of the span
    pub fn hi(self) -> u32 {
        self.hi
    }

    pub(crate) fn span<T>(self, t: T) -> Spanned<T> {
        Spanned::new(self, t)
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{}", self.lo, self.hi)
    }
}

/// An object with a span
///
/// This type derefs to the contained object.
///
/// This type's `Hash` and `PartialEq` implementations delegate to the contained object.
#[derive(Copy, Clone, Eq)]
pub struct Spanned<T> {
    pub(crate) span: Span,
    pub(crate) val: T,
}

impl<T> Spanned<T> {
    /// Creates a new instance
    pub fn new(span: Span, val: T) -> Spanned<T> {
        Spanned { span, val }
    }

    /// Returns the span
    pub fn span(&self) -> Span {
        self.span
    }

    /// Unwraps the object
    pub fn unwrap(self) -> T {
        self.val
    }
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
