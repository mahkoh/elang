use crate::types::store::StrId;
use std::{
    collections::{hash_map::Entry, HashMap},
    fmt,
    fmt::{Debug, Formatter},
};

/// A scope for identifier resolution.
///
/// = Remarks
///
/// Inner scopes can shadow outer scopes. Hence, whenever we substitute identifiers, we
/// have a set of bound identifiers and a set of unbound identifiers. E.g.,
///
/// ```elang
/// let
///     x = 1;
///     y = 2;
/// in
///     x + let
///         x = 3;
///     in
///         x + y
/// ```
///
/// will be resolved to
///
/// ```elang
/// 1 + let
///     x = 3;
/// in
///     x + 2
/// ```
///
/// When we set up the substitutions for the outer `let`, we add `x` and `y` to the set of
/// bound identifiers. When we encounter the inner `let`, we add `x` to the set of unbound
/// identifiers so that we don't accidentally substitute the outer `x` inside the inner
/// `let`'s body.
pub struct Scope<T: Clone> {
    names: HashMap<StrId, Vec<Option<T>>>,
}

impl<T: Clone> Default for Scope<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Clone> Scope<T> {
    /// Creates a new scope.
    pub fn new() -> Scope<T> {
        Scope {
            names: HashMap::new(),
        }
    }

    /// Pushes an identifier and its substitution onto the scope.
    ///
    /// = Remarks
    ///
    /// Every identifier can be pushed multiple times. The later values shadow the earlier
    /// values.
    pub fn bind(&mut self, name: StrId, val: T) {
        match self.names.entry(name) {
            Entry::Occupied(mut o) => o.get_mut().push(Some(val)),
            Entry::Vacant(v) => {
                v.insert(vec![Some(val)]);
            }
        }
    }

    pub fn hide(&mut self, name: StrId) {
        match self.names.entry(name) {
            Entry::Occupied(mut o) => o.get_mut().push(None),
            Entry::Vacant(v) => {
                v.insert(vec![None]);
            }
        }
    }

    /// Pops an identifier from the scope.
    pub fn pop(&mut self, name: StrId) {
        match self.names.entry(name) {
            Entry::Occupied(mut o) => {
                let u = o.get_mut();
                u.pop().unwrap();
                if u.is_empty() {
                    o.remove();
                }
            }
            _ => unreachable!(),
        }
    }

    /// Returns the value associated with an identifier at the innermost scope, if any.
    pub fn get(&self, name: StrId) -> Option<T> {
        match self.names.get(&name) {
            Some(v) => v.last().cloned().unwrap(),
            _ => None,
        }
    }
}

impl<T: Debug + Clone> Debug for Scope<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Scope {{ names: {:?} }}", self.names)
    }
}
