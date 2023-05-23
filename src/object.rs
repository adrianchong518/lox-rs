pub mod callable;
pub mod class;
pub mod clock_fn;
pub mod function;
pub mod instance;

pub use callable::CallableObject;
pub use class::Class;
pub use class::ClassHandle;
pub use clock_fn::ClockFn;
pub use function::Function;
pub use instance::Instance;
pub use instance::InstanceHandle;

use std::fmt;

use crate::token;

#[derive(Debug, Clone, PartialEq)]
pub enum Object<'s> {
    Number(f64),
    String(String),
    Bool(bool),
    Callable(CallableObject<'s>),
    Instance(InstanceHandle<'s>),
    Nil,
}

impl Object<'_> {
    pub fn is_truthy(&self) -> bool {
        match self {
            Self::Bool(bool) => *bool,
            Self::Nil => false,
            _ => true,
        }
    }
}

impl fmt::Display for Object<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            match self {
                Self::Number(_) => write!(f, "number"),
                Self::String(_) => write!(f, "string"),
                Self::Bool(_) => write!(f, "boolean"),
                Self::Callable(c) => write!(f, "{c:#}"),
                Self::Instance(i) => write!(f, "{i:#}"),
                Self::Nil => write!(f, "nil"),
            }
        } else {
            match self {
                Self::Number(n) => write!(f, "{n}"),
                Self::String(s) => write!(f, "{s}"),
                Self::Bool(b) => write!(f, "{b}"),
                Self::Callable(c) => write!(f, "{c}"),
                Self::Instance(i) => write!(f, "{i}"),
                Self::Nil => write!(f, "nil"),
            }
        }
    }
}

impl From<token::LiteralType> for Object<'_> {
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

impl From<f64> for Object<'_> {
    fn from(value: f64) -> Self {
        Self::Number(value)
    }
}

impl From<String> for Object<'_> {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl From<bool> for Object<'_> {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl<'s, T> From<T> for Object<'s>
where
    T: Into<callable::CallableObject<'s>>,
{
    fn from(value: T) -> Self {
        Self::Callable(value.into())
    }
}
