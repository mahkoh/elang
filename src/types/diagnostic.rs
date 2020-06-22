use crate::{
    types::{span::Span, store::StrId, token::TokenType, tree::ValueType},
    ExprId,
};

#[derive(Debug)]
pub struct Error {
    pub span: Span,
    pub error: ErrorType,
    pub context: Vec<ErrorContext>,
}

impl Error {
    pub(crate) fn add_context(mut self, ctx: ErrorContext) -> Self {
        self.context.push(ctx);
        self
    }
}

#[derive(Copy, Clone, Debug)]
#[non_exhaustive]
pub enum ErrorContext {
    ParseString(u32),
    ParseUnicodeEscape(u32),
    ParseTest(u32),
    ParseSelect(u32),
    ParseParenthesized(u32),
    ParseFnHeader(u32),
    ParseFnPattern(u32),
    ParseLet(u32),
    ParseField(u32),
    ParseCond(u32),
    ParseList(u32),
    ParseSet(u32),
    ParseInherit(u32),
    EvalResolved(ExprId),
    EvalArithmetic(ExprId),
    EvalBool(ExprId),
    EvalOverlay(ExprId),
    EvalConcat(ExprId),
    EvalCond(ExprId),
    EvalStringify(ExprId),
    EvalApl(ExprId),
    EvalSelect(ExprId),
}

#[derive(Copy, Clone, Debug)]
#[non_exhaustive]
pub enum ErrorType {
    UnexpectedEndOfInput,
    UnexpectedToken(TokenAlternative, TokenType),
    OutOfBoundsLiteral,
    OutOfBoundsSelector(i64),
    UnexpectedIntegerSuffix(u8),
    UnexpectedByte(u8),
    MissingCodePoint,
    InvalidCodePoint(u32),
    UnknownEscapeSequence(u8),
    DuplicateIdentifier(StrId, Span),
    UnexpectedExpr(&'static [ValueType], ValueType),
    MissingSetField(StrId),
    MissingListField(usize),
    InfiniteRecursion(ExprId),
    CannotForceExpr(ValueType),
    DivideByZero,
    ExtraArgument(StrId),
    MissingArgument(StrId),
    MissingNewline,
    SpanOverflow,
    Overflow,
}

#[derive(Copy, Clone, Debug)]
pub enum TokenAlternative {
    EndOfInput,
    StartOfExpression,
    List(&'static [TokenType]),
}
