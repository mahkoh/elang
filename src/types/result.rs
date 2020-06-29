use crate::{types::diagnostic::ErrorContext, Error};

/// The elang result type
pub type Result<T = ()> = std::result::Result<T, Error>;

pub(crate) trait ResultUtil<T> {
    fn ctx(self, ctx: ErrorContext) -> Self;
    fn unreachable(self) -> T;
}

impl<T> ResultUtil<T> for Result<T> {
    fn ctx(self, ctx: ErrorContext) -> Self {
        self.map_err(|e| e.add_context(ctx))
    }

    fn unreachable(self) -> T {
        match self {
            Ok(t) => t,
            _ => unreachable!(),
        }
    }
}
