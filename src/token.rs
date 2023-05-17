use std::{borrow::Cow, fmt};

#[derive(Debug, Clone)]
pub struct Token<'s> {
    pub typ: Type<'s>,
    pub lexeme: Cow<'s, str>,
    pub line: usize,
}

#[derive(Debug, Clone)]
pub struct Literal<'s> {
    pub typ: LiteralType,
    pub lexeme: Cow<'s, str>,
    pub line: usize,
}

#[derive(Debug, Clone)]
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

impl Token<'_> {
    pub fn into_owned(self) -> Token<'static> {
        Token {
            typ: self.typ.into_owned(),
            lexeme: Cow::from(self.lexeme.into_owned()),
            line: self.line,
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

#[derive(Debug, Clone)]
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

    Identifier(Cow<'s, str>),
    Literal(LiteralType),

    /// `and`
    And,
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

impl Type<'_> {
    pub fn try_from_keyword(text: &str) -> Option<Self> {
        Some(match text {
            "and" => Self::And,
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

    pub fn into_owned(self) -> Type<'static> {
        match self {
            Type::Identifier(ident) => Type::Identifier(Cow::from(ident.into_owned())),
            Type::LeftParen => Type::LeftParen,
            Type::RightParen => Type::RightParen,
            Type::LeftBrace => Type::LeftBrace,
            Type::RightBrace => Type::RightBrace,
            Type::Comma => Type::Comma,
            Type::Dot => Type::Dot,
            Type::Minus => Type::Minus,
            Type::Plus => Type::Plus,
            Type::Semicolon => Type::Semicolon,
            Type::Slash => Type::Slash,
            Type::Star => Type::Star,
            Type::Bang => Type::Bang,
            Type::BangEqual => Type::BangEqual,
            Type::Equal => Type::Equal,
            Type::EqualEqual => Type::EqualEqual,
            Type::Greater => Type::Greater,
            Type::GreaterEqual => Type::GreaterEqual,
            Type::Less => Type::Less,
            Type::LessEqual => Type::LessEqual,
            Type::Literal(l) => Type::Literal(l),
            Type::And => Type::And,
            Type::Class => Type::Class,
            Type::Else => Type::Else,
            Type::Fun => Type::Fun,
            Type::For => Type::For,
            Type::If => Type::If,
            Type::Or => Type::Or,
            Type::Print => Type::Print,
            Type::Return => Type::Return,
            Type::Super => Type::Super,
            Type::This => Type::This,
            Type::Var => Type::Var,
            Type::While => Type::While,
        }
    }
}
