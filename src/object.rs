use std::fmt;

use crate::token;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Number(f64),
    String(String),
    Bool(bool),
    Nil,
}

impl Object {
    pub fn is_truthy(&self) -> bool {
        match self {
            Self::Bool(bool) => *bool,
            Self::Nil => false,
            _ => true,
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Number(n) => write!(f, "{n}"),
            Object::String(s) => write!(f, "{s}"),
            Object::Bool(b) => write!(f, "{b}"),
            Object::Nil => write!(f, "nil"),
        }
    }
}

impl From<token::LiteralType> for Object {
    fn from(value: token::LiteralType) -> Self {
        match value {
            token::LiteralType::Number(n) => Self::Number(n),
            token::LiteralType::String(s) => Self::String(s),
            token::LiteralType::True => Self::Bool(true),
            token::LiteralType::False => Self::Bool(false),
            token::LiteralType::Nil => Self::Nil,
        }
    }
}

impl From<f64> for Object {
    fn from(value: f64) -> Self {
        Self::Number(value)
    }
}

impl From<String> for Object {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl From<bool> for Object {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}
