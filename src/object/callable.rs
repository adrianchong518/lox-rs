use std::fmt;

use crate::interpreter;

#[derive(Debug, Clone, PartialEq)]
pub enum CallableObject<'s> {
    ClockFn(super::clock_fn::ClockFn),
    Function(super::function::Function<'s>),
    Class(super::class::ClassHandle<'s>),
}

pub trait Callable<'s> {
    fn arity(&self) -> usize;

    fn call(
        &self,
        interpreter: &mut interpreter::Interpreter<'s>,
        arguments: Vec<super::Object<'s>>,
        info: &crate::token::Info<'s>,
    ) -> Result<super::Object<'s>, interpreter::RuntimeErrorState<'s>>;
}

impl<'s> Callable<'s> for CallableObject<'s> {
    fn arity(&self) -> usize {
        match self {
            Self::ClockFn(clock_fn) => clock_fn.arity(),
            Self::Function(func) => func.arity(),
            Self::Class(class) => class.arity(),
        }
    }

    fn call(
        &self,
        interpreter: &mut interpreter::Interpreter<'s>,
        arguments: Vec<super::Object<'s>>,
        info: &crate::token::Info<'s>,
    ) -> Result<super::Object<'s>, interpreter::RuntimeErrorState<'s>> {
        match self {
            Self::ClockFn(clock_fn) => clock_fn.call(interpreter, arguments, info),
            Self::Function(func) => func.call(interpreter, arguments, info),
            Self::Class(class) => class.call(interpreter, arguments, info),
        }
    }
}

impl fmt::Display for CallableObject<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ClockFn(clock_fn) => clock_fn.fmt(f),
            Self::Function(func) => func.fmt(f),
            Self::Class(class) => class.fmt(f),
        }
    }
}
