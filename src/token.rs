use std::fmt;

#[derive(Debug)]
pub struct Token<'s> {
    pub typ: Type<'s>,
    pub lexeme: &'s str,
    pub line: usize,
}

pub struct Literal<'s> {
    pub typ: LiteralType,
    pub lexeme: &'s str,
    pub line: usize,
}

#[derive(Debug)]
pub enum LiteralType {
    Number(f64),
    String(String),

    /// `nil`
    Nil,
}

impl fmt::Display for LiteralType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Number(num) => write!(f, "{num}"),
            Self::String(string) => write!(f, "{string:?}"),
            Self::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum Type<'s> {
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

    Identifier(&'s str),
    Literal(LiteralType),

    /// `and`
    And,
    /// `class`
    Class,
    /// `else`
    Else,
    /// `false`
    False,
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
    /// `true`
    True,
    /// `var`
    Var,
    /// `while`
    While,
}

impl<'s> Type<'s> {
    pub fn try_from_keyword(text: &str) -> Option<Self> {
        Some(match text {
            "and" => Self::And,
            "class" => Self::Class,
            "else" => Self::Else,
            "false" => Self::False,
            "for" => Self::For,
            "fun" => Self::Fun,
            "if" => Self::If,
            "nil" => Self::Literal(LiteralType::Nil),
            "or" => Self::Or,
            "print" => Self::Print,
            "return" => Self::Return,
            "super" => Self::Super,
            "this" => Self::This,
            "true" => Self::True,
            "var" => Self::Var,
            "while" => Self::While,
            _ => return None,
        })
    }
}
