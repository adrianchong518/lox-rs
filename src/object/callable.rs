use std::fmt;

use crate::interpreter;

#[derive(Debug, Clone, PartialEq)]
pub enum CallableObject {
    ClockFn(super::clock_fn::ClockFn),
    Function(super::function::Function),
}

pub trait Callable {
    fn arity(&self) -> usize;

    fn call(
        &self,
        interpreter: &mut interpreter::Interpreter,
        arguments: Vec<super::Object>,
        info: &crate::token::Info<'_>,
    ) -> Result<super::Object, interpreter::RuntimeErrorState>;
}

impl Callable for CallableObject {
    fn arity(&self) -> usize {
        match self {
            Self::ClockFn(clock_fn) => clock_fn.arity(),
            Self::Function(func) => func.arity(),
        }
    }

    fn call(
        &self,
        interpreter: &mut interpreter::Interpreter,
        arguments: Vec<super::Object>,
        info: &crate::token::Info<'_>,
    ) -> Result<super::Object, interpreter::RuntimeErrorState> {
        match self {
            Self::ClockFn(clock_fn) => clock_fn.call(interpreter, arguments, info),
            Self::Function(func) => func.call(interpreter, arguments, info),
        }
    }
}

impl fmt::Display for CallableObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ClockFn(clock_fn) => clock_fn.fmt(f),
            Self::Function(func) => func.fmt(f),
        }
    }
}
