pub mod callable;
pub mod clock_fn;
pub mod function;

pub use clock_fn::ClockFn;
pub use function::Function;

use std::fmt;

use crate::token;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Number(f64),
    String(String),
    Bool(bool),
    Callable(callable::CallableObject),
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
        if f.alternate() {
            match self {
                Self::Number(_) => write!(f, "number"),
                Self::String(_) => write!(f, "string"),
                Self::Bool(_) => write!(f, "boolean"),
                Self::Callable(c) => write!(f, "{c:#}"),
                Self::Nil => write!(f, "nil"),
            }
        } else {
            match self {
                Self::Number(n) => write!(f, "{n}"),
                Self::String(s) => write!(f, "{s}"),
                Self::Bool(b) => write!(f, "{b}"),
                Self::Callable(c) => write!(f, "{c}"),
                Self::Nil => write!(f, "nil"),
            }
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

impl<T> From<T> for Object
where
    T: Into<callable::CallableObject>,
{
    fn from(value: T) -> Self {
        Self::Callable(value.into())
    }
}
