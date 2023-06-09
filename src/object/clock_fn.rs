use std::{fmt, time};

use error_stack::{IntoReport as _, ResultExt as _};

use crate::{interpreter, into_owned::IntoOwned as _, object::callable, token};

#[derive(Debug)]
pub struct ClockFnError;

impl fmt::Display for ClockFnError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<native fn clock> error")
    }
}

impl error_stack::Context for ClockFnError {}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ClockFn;

impl<'s> super::callable::Callable<'s> for ClockFn {
    fn arity(&self) -> usize {
        0
    }

    fn call(
        &self,
        _interpreter: &mut interpreter::Interpreter<'s>,
        _arguments: Vec<super::Object<'s>>,
        info: &token::Info<'s>,
    ) -> Result<super::Object<'s>, interpreter::RuntimeErrorState<'s>> {
        Ok(time::SystemTime::now()
            .duration_since(time::UNIX_EPOCH)
            .into_report()
            .change_context(ClockFnError)
            .attach_printable("time went backwards")
            .change_context_lazy(|| interpreter::RuntimeError::new(info.clone().into_owned()))?
            .as_secs_f64()
            .into())
    }
}

impl fmt::Display for ClockFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<native fn clock>")
    }
}

impl From<ClockFn> for callable::CallableObject<'_> {
    fn from(value: ClockFn) -> Self {
        Self::ClockFn(value)
    }
}
