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
    /// A non-terminating part of a string that contains an expression interpolation.
    ///
    /// = Remarks
    ///
    /// This looks like this in the source code:
    ///
    /// ```txt
    /// "aaaaaaaaaaaa\{e1}bbbbbbbbb\{e2}cccccccc"
    /// \____   ____/     \__   __/     \__   __/
    ///      \ /             \ /           \ /
    ///   StringPart      StringPart      String
    /// ```
    ///
    /// When the parser gets a `StringPart` from the lexer, it parses a single expression
    /// (`e1` or `e2` above), eats a `}`, and then tells the lexer to continue parsing the
    /// string.
    StringPart(StrId),
    String(StrId),

    Number(StrId, u16, u16),
    Ident(StrId),
    True,
    False,
    Null,
    Rec,
    Let,
    In,
    Inherit,
    If,
    Then,
    Else,
    Or,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    LeftParen,
    RightParen,
    Dot,
    DotDot,
    Assign,
    Colon,
    Comma,
    Questionmark,
    At,
    Equals,
    Unequal,
    Lt,
    Le,
    Gt,
    Ge,
    Concat,
    Plus,
    Minus,
    Times,
    Div,
    Not,
    Mod,
    /// `\\`
    Overlay,
    AndAnd,
    OrOr,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum TokenType {
    AndAnd,
    Assign,
    At,
    Colon,
    Comma,
    Concat,
    Div,
    Dot,
    DotDot,
    Else,
    Equals,
    False,
    Ge,
    Gt,
    Ident,
    If,
    In,
    Inherit,
    Le,
    LeftBrace,
    LeftBracket,
    LeftParen,
    Let,
    Lt,
    Minus,
    Mod,
    Not,
    Null,
    Number,
    Or,
    OrOr,
    Overlay,
    Plus,
    Questionmark,
    Rec,
    RightBrace,
    RightBracket,
    RightParen,
    String,
    Then,
    Times,
    True,
    Unequal,
}

impl TokenType {
    pub fn as_str(self) -> &'static str {
        match self {
            TokenType::AndAnd => "&&",
            TokenType::Assign => "=",
            TokenType::At => "@",
            TokenType::Colon => ":",
            TokenType::Comma => ",",
            TokenType::Concat => "++",
            TokenType::Div => "/",
            TokenType::Dot => ".",
            TokenType::DotDot => "..",
            TokenType::Else => "else",
            TokenType::Equals => "==",
            TokenType::False => "false",
            TokenType::Ge => ">=",
            TokenType::Gt => ">",
            TokenType::Ident => "identifier",
            TokenType::If => "if",
            TokenType::Inherit => "inherit",
            TokenType::In => "in",
            TokenType::Number => "number",
            TokenType::Le => "<=",
            TokenType::LeftBrace => "{",
            TokenType::LeftBracket => "[",
            TokenType::LeftParen => "(",
            TokenType::Let => "let",
            TokenType::Lt => "<",
            TokenType::Minus => "-",
            TokenType::Mod => "%",
            TokenType::Not => "!",
            TokenType::Null => "null",
            TokenType::Or => "or",
            TokenType::OrOr => "||",
            TokenType::Overlay => r"\\",
            TokenType::Plus => "+",
            TokenType::Questionmark => "?",
            TokenType::Rec => "rec",
            TokenType::RightBrace => "}",
            TokenType::RightBracket => "]",
            TokenType::RightParen => ")",
            TokenType::String => "string",
            TokenType::Then => "then",
            TokenType::Times => "*",
            TokenType::True => "true",
            TokenType::Unequal => "!=",
        }
    }
}

impl Token {
    /// Returns whether this token starts an expression.
    ///
    /// = Remarks
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
    /// the `x` (an identifier) starts a new expressiond and this will be parsed as a
    /// function application: `f(x + 1)`.
    pub fn starts_expr(self) -> bool {
        match self {
            Token::StringPart(..) => true,
            Token::String(..) => true,
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
            Token::Not => true,

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
            Token::Assign => false,
            Token::Colon => false,
            Token::Comma => false,
            Token::Questionmark => false,
            Token::At => false,
            Token::Equals => false,
            Token::Unequal => false,
            Token::Lt => false,
            Token::Le => false,
            Token::Gt => false,
            Token::Ge => false,
            Token::Concat => false,
            Token::Plus => false,
            Token::Times => false,
            Token::Div => false,
            Token::Mod => false,
            Token::Overlay => false,
            Token::AndAnd => false,
            Token::OrOr => false,
        }
    }

    pub fn ty(self) -> TokenType {
        match self {
            Token::AndAnd => TokenType::AndAnd,
            Token::Assign => TokenType::Assign,
            Token::At => TokenType::At,
            Token::Colon => TokenType::Colon,
            Token::Comma => TokenType::Comma,
            Token::Concat => TokenType::Concat,
            Token::Div => TokenType::Div,
            Token::Dot => TokenType::Dot,
            Token::DotDot => TokenType::DotDot,
            Token::Else => TokenType::Else,
            Token::Equals => TokenType::Equals,
            Token::False => TokenType::False,
            Token::Ge => TokenType::Ge,
            Token::Gt => TokenType::Gt,
            Token::Ident(..) => TokenType::Ident,
            Token::If => TokenType::If,
            Token::Inherit => TokenType::Inherit,
            Token::In => TokenType::In,
            Token::Number(..) => TokenType::Number,
            Token::Le => TokenType::Le,
            Token::LeftBrace => TokenType::LeftBrace,
            Token::LeftBracket => TokenType::LeftBracket,
            Token::LeftParen => TokenType::LeftParen,
            Token::Let => TokenType::Let,
            Token::Lt => TokenType::Lt,
            Token::Minus => TokenType::Minus,
            Token::Mod => TokenType::Mod,
            Token::Not => TokenType::Not,
            Token::Null => TokenType::Null,
            Token::Or => TokenType::Or,
            Token::OrOr => TokenType::OrOr,
            Token::Overlay => TokenType::Overlay,
            Token::Plus => TokenType::Plus,
            Token::Questionmark => TokenType::Questionmark,
            Token::Rec => TokenType::Rec,
            Token::RightBrace => TokenType::RightBrace,
            Token::RightBracket => TokenType::RightBracket,
            Token::RightParen => TokenType::RightParen,
            Token::StringPart(..) => TokenType::String,
            Token::String(..) => TokenType::String,
            Token::Then => TokenType::Then,
            Token::Times => TokenType::Times,
            Token::True => TokenType::True,
            Token::Unequal => TokenType::Unequal,
        }
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.ty().as_str())
    }
}
