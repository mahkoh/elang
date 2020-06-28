use crate::types::{span::Spanned, store::StrId};
use std::{
    fmt,
    fmt::{Debug, Formatter},
};

/// A token with a span.
pub type SToken = Spanned<Token>;

/// A token produced by the lexer.
#[derive(Copy, Clone, Eq, PartialEq)]
pub enum Token {
    StringStart,
    String(StrId),
    StringEnd,

    AmpersandAmpersand,
    At,
    BackslashBackslash,
    BarBar,
    Colon,
    Comma,
    Dot,
    DotDot,
    Else,
    Equals,
    EqualsEquals,
    ExclamationMark,
    ExclamationMarkEquals,
    False,
    Ident(StrId),
    If,
    In,
    Inherit,
    IntSlash,
    LeftAngle,
    LeftAngleEquals,
    LeftBrace,
    LeftBracket,
    LeftParen,
    Let,
    Minus,
    Null,
    Number(StrId, u16, u16),
    Or,
    Percent,
    Plus,
    PlusPlus,
    QuestionMark,
    Rec,
    RightAngle,
    RightAngleEquals,
    RightBrace,
    RightBracket,
    RightParen,
    Slash,
    Star,
    Then,
    True,
}

/// The type of a token
///
/// Identifiers, strings, and numbers carry associated data. The variants of this enum do
/// not carry that data.
///
/// This type is used for diagnostic purposes only.
#[derive(Copy, Clone, Eq, PartialEq)]
pub enum TokenType {
    /// `&&`
    AmpersandAmpersand,
    /// `@`
    At,
    /// `\\`
    BackslashBackslash,
    /// `||`
    BarBar,
    /// `:`
    Colon,
    /// `,`
    Comma,
    /// `.`
    Dot,
    /// `..`
    DotDot,
    /// `else`
    Else,
    /// `=`
    Equals,
    /// `==`
    EqualsEquals,
    /// `!`
    ExclamationMark,
    /// `!=`
    ExclamationMarkEquals,
    /// `false`
    False,
    /// An identifier
    Ident,
    /// `if`
    If,
    /// `in`
    In,
    /// `inherit`
    Inherit,
    /// `int/`
    IntSlash,
    /// `<`
    LeftAngle,
    /// `<=`
    LeftAngleEquals,
    /// `{`
    LeftBrace,
    /// `[`
    LeftBracket,
    /// `(`
    LeftParen,
    /// `let`
    Let,
    /// `-`
    Minus,
    /// `null`
    Null,
    /// A number
    Number,
    /// `or`
    Or,
    /// `%`
    Percent,
    /// `+`
    Plus,
    /// `++`
    PlusPlus,
    /// `?`
    QuestionMark,
    /// `rec`
    Rec,
    /// `>`
    RightAngle,
    /// `>=`
    RightAngleEquals,
    /// `}`
    RightBrace,
    /// `]`
    RightBracket,
    /// `)`
    RightParen,
    /// `/`
    Slash,
    /// `*`
    Star,
    /// A string
    String,
    /// `then`
    Then,
    /// `true`
    True,
}

impl TokenType {
    /// Returns the textual representation of the token type
    ///
    /// For symbol-like tokens and keywords, this is simply their representation in source
    /// code. For example, `AmpersandAmpersand => "&&"`.
    ///
    /// There are three special cases:
    ///
    /// * `Ident => "identifier"`,
    /// * `Number => "number"`,
    /// * `String => "string"`,
    pub fn as_str(self) -> &'static str {
        match self {
            TokenType::AmpersandAmpersand => "&&",
            TokenType::At => "@",
            TokenType::BackslashBackslash => r"\\",
            TokenType::BarBar => "||",
            TokenType::Colon => ":",
            TokenType::Comma => ",",
            TokenType::Dot => ".",
            TokenType::DotDot => "..",
            TokenType::Else => "else",
            TokenType::Equals => "=",
            TokenType::EqualsEquals => "==",
            TokenType::ExclamationMark => "!",
            TokenType::ExclamationMarkEquals => "!=",
            TokenType::False => "false",
            TokenType::Ident => "identifier",
            TokenType::If => "if",
            TokenType::Inherit => "inherit",
            TokenType::In => "in",
            TokenType::IntSlash => "int/",
            TokenType::LeftAngle => "<",
            TokenType::LeftAngleEquals => "<=",
            TokenType::LeftBrace => "{",
            TokenType::LeftBracket => "[",
            TokenType::LeftParen => "(",
            TokenType::Let => "let",
            TokenType::Minus => "-",
            TokenType::Null => "null",
            TokenType::Number => "number",
            TokenType::Or => "or",
            TokenType::Percent => "%",
            TokenType::Plus => "+",
            TokenType::PlusPlus => "++",
            TokenType::QuestionMark => "?",
            TokenType::Rec => "rec",
            TokenType::RightAngle => ">",
            TokenType::RightAngleEquals => ">=",
            TokenType::RightBrace => "}",
            TokenType::RightBracket => "]",
            TokenType::RightParen => ")",
            TokenType::Slash => "/",
            TokenType::Star => "*",
            TokenType::String => "string",
            TokenType::Then => "then",
            TokenType::True => "true",
        }
    }
}

impl Token {
    /// Returns whether this token starts an expression
    ///
    /// We use this to find out if an expression has just ended. If a token does not start
    /// an expression, then it combines a subsequent token with the current expression.
    /// E.g., in
    ///
    /// ```elang
    /// 1 + 1
    /// ```
    ///
    /// the `+` does not start a new expression. However, in
    ///
    /// ```elang
    /// f x + 1
    /// ```
    ///
    /// the `x` (an identifier) starts a new expression and this will be parsed as a
    /// function application: `f(x + 1)`.
    pub fn starts_expr(self) -> bool {
        match self {
            Token::StringStart => true,
            Token::Number(..) => true,
            Token::Ident(..) => true,
            Token::True => true,
            Token::False => true,
            Token::Null => true,
            Token::Rec => true,
            Token::Let => true,
            Token::If => true,
            Token::LeftBracket => true,
            Token::LeftBrace => true,
            Token::LeftParen => true,

            // Unary operators
            Token::Minus => true,
            Token::ExclamationMark => true,

            Token::String(..) => false,
            Token::StringEnd => false,
            Token::In => false,
            Token::Inherit => false,
            Token::Then => false,
            Token::Else => false,
            Token::Or => false,
            Token::RightBracket => false,
            Token::RightBrace => false,
            Token::RightParen => false,
            Token::Dot => false,
            Token::DotDot => false,
            Token::Equals => false,
            Token::Colon => false,
            Token::Comma => false,
            Token::QuestionMark => false,
            Token::At => false,
            Token::EqualsEquals => false,
            Token::ExclamationMarkEquals => false,
            Token::LeftAngle => false,
            Token::LeftAngleEquals => false,
            Token::RightAngle => false,
            Token::RightAngleEquals => false,
            Token::PlusPlus => false,
            Token::Plus => false,
            Token::Star => false,
            Token::Slash => false,
            Token::Percent => false,
            Token::BackslashBackslash => false,
            Token::AmpersandAmpersand => false,
            Token::BarBar => false,
            Token::IntSlash => false,
        }
    }

    pub fn ty(self) -> TokenType {
        match self {
            Token::AmpersandAmpersand => TokenType::AmpersandAmpersand,
            Token::At => TokenType::At,
            Token::BackslashBackslash => TokenType::BackslashBackslash,
            Token::BarBar => TokenType::BarBar,
            Token::Colon => TokenType::Colon,
            Token::Comma => TokenType::Comma,
            Token::DotDot => TokenType::DotDot,
            Token::Dot => TokenType::Dot,
            Token::Else => TokenType::Else,
            Token::EqualsEquals => TokenType::EqualsEquals,
            Token::Equals => TokenType::Equals,
            Token::ExclamationMarkEquals => TokenType::ExclamationMarkEquals,
            Token::ExclamationMark => TokenType::ExclamationMark,
            Token::False => TokenType::False,
            Token::Ident(..) => TokenType::Ident,
            Token::If => TokenType::If,
            Token::Inherit => TokenType::Inherit,
            Token::In => TokenType::In,
            Token::IntSlash => TokenType::IntSlash,
            Token::LeftAngleEquals => TokenType::LeftAngleEquals,
            Token::LeftAngle => TokenType::LeftAngle,
            Token::LeftBrace => TokenType::LeftBrace,
            Token::LeftBracket => TokenType::LeftBracket,
            Token::LeftParen => TokenType::LeftParen,
            Token::Let => TokenType::Let,
            Token::Minus => TokenType::Minus,
            Token::Null => TokenType::Null,
            Token::Number(..) => TokenType::Number,
            Token::Or => TokenType::Or,
            Token::Percent => TokenType::Percent,
            Token::PlusPlus => TokenType::PlusPlus,
            Token::Plus => TokenType::Plus,
            Token::QuestionMark => TokenType::QuestionMark,
            Token::Rec => TokenType::Rec,
            Token::RightAngleEquals => TokenType::RightAngleEquals,
            Token::RightAngle => TokenType::RightAngle,
            Token::RightBrace => TokenType::RightBrace,
            Token::RightBracket => TokenType::RightBracket,
            Token::RightParen => TokenType::RightParen,
            Token::Slash => TokenType::Slash,
            Token::Star => TokenType::Star,
            Token::StringStart => TokenType::String,
            Token::String(..) => TokenType::String,
            Token::StringEnd => TokenType::String,
            Token::Then => TokenType::Then,
            Token::True => TokenType::True,
        }
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.ty().as_str())
    }
}

impl Debug for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
