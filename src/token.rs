use std::{borrow::Cow, fmt};

use crate::into_owned::IntoOwned;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Info<'s> {
    pub lexeme: Cow<'s, str>,
    pub line: usize,
    pub offset: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'s> {
    pub typ: Type,
    pub info: Info<'s>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Literal<'s> {
    pub typ: LiteralType,
    pub info: Info<'s>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralType {
    Number(f64),
    String(String),

    /// `true`
    True,
    /// `false`
    False,
    /// `nil`
    Nil,
}

impl IntoOwned for Token<'_> {
    type Owned = Token<'static>;

    fn into_owned(self) -> Self::Owned {
        Token {
            typ: self.typ,
            info: self.info.into_owned(),
        }
    }
}

impl IntoOwned for Info<'_> {
    type Owned = Info<'static>;

    fn into_owned(self) -> Self::Owned {
        Info {
            lexeme: Cow::from(self.lexeme.into_owned()),
            ..self
        }
    }
}

impl IntoOwned for Literal<'_> {
    type Owned = Literal<'static>;

    fn into_owned(self) -> Self::Owned {
        Literal {
            typ: self.typ,
            info: self.info.into_owned(),
        }
    }
}

impl fmt::Display for LiteralType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Number(num) => write!(f, "{num}"),
            Self::String(string) => write!(f, "{string:?}"),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub enum Type {
    // single character tokens (with no ambiguity)
    /// `(`
    LeftParen,
    /// `)`
    RightParen,
    /// `{`
    LeftBrace,
    /// `}`
    RightBrace,
    /// `,`
    Comma,
    /// `.`
    Dot,
    /// `-`
    Minus,
    /// `+`
    Plus,
    /// `;`
    Semicolon,
    /// `/`
    Slash,
    /// `*`
    Star,

    // 1/2 character tokens (with ambiguity)
    /// `!`
    Bang,
    /// `!=`
    BangEqual,
    /// `=`
    Equal,
    /// `==`
    EqualEqual,
    /// `>`
    Greater,
    /// `>=`
    GreaterEqual,
    /// `<`
    Less,
    /// `<=`
    LessEqual,

    Identifier,
    Literal(LiteralType),

    /// `and`
    And,
    /// `break`
    Break,
    /// `class`
    Class,
    /// `else`
    Else,
    /// `fun`
    Fun,
    /// `for`
    For,
    /// `if`
    If,
    /// `or`
    Or,
    /// `print`
    Print,
    /// `return`
    Return,
    /// `super`
    Super,
    /// `this`
    This,
    /// `var`
    Var,
    /// `while`
    While,
}

impl Type {
    pub fn try_from_keyword(text: &str) -> Option<Self> {
        Some(match text {
            "and" => Self::And,
            "break" => Self::Break,
            "class" => Self::Class,
            "else" => Self::Else,
            "false" => Self::Literal(LiteralType::False),
            "for" => Self::For,
            "fun" => Self::Fun,
            "if" => Self::If,
            "nil" => Self::Literal(LiteralType::Nil),
            "or" => Self::Or,
            "print" => Self::Print,
            "return" => Self::Return,
            "super" => Self::Super,
            "this" => Self::This,
            "true" => Self::Literal(LiteralType::True),
            "var" => Self::Var,
            "while" => Self::While,
            _ => return None,
        })
    }
}
