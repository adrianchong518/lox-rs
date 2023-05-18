use std::{collections::HashMap, fmt};

use crate::{object, token};

#[derive(Debug)]
pub struct EnvironmentError;

impl fmt::Display for EnvironmentError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "environment error")
    }
}

impl error_stack::Context for EnvironmentError {}

pub(super) struct Context {
    values: HashMap<String, object::Object>,
}

impl Context {
    fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }
}

/// Execution environment for interpreter
///
/// This differs from the book as a context / environment stack is used, instead of a parent-pointer tree
pub(super) struct Environment {
    global: Context,
    stack: Vec<Context>,
}

impl Environment {
    pub(super) fn new() -> Self {
        Environment {
            global: Context::new(),
            stack: Vec::new(),
        }
    }

    pub(super) fn push_new_context(&mut self) {
        self.stack.push(Context::new());
    }

    pub(super) fn pop_context(&mut self) {
        self.stack.pop();
    }

    fn current_context(&self) -> &Context {
        self.stack.last().unwrap_or(&self.global)
    }

    fn current_context_mut(&mut self) -> &mut Context {
        self.stack.last_mut().unwrap_or(&mut self.global)
    }

    fn walk_context(&self) -> impl Iterator<Item = &Context> {
        self.stack.iter().rev().chain(std::iter::once(&self.global))
    }

    fn walk_context_mut(&mut self) -> impl Iterator<Item = &mut Context> {
        self.stack
            .iter_mut()
            .rev()
            .chain(std::iter::once(&mut self.global))
    }

    pub(super) fn define(&mut self, info: token::Info, value: object::Object) {
        self.current_context_mut()
            .values
            .insert(info.lexeme.into_owned(), value);
    }

    pub(super) fn assign(
        &mut self,
        info: &token::Info,
        value: object::Object,
    ) -> error_stack::Result<(), EnvironmentError> {
        self.walk_context_mut()
            .find_map(|ctx| ctx.values.get_mut(&*info.lexeme))
            .map(|obj| *obj = value)
            .ok_or_else(|| {
                error_stack::report!(EnvironmentError).attach_printable(format!(
                    "cannot assign to undefined variable {:?}",
                    info.lexeme
                ))
            })
    }

    pub(super) fn get(&self, info: &token::Info) -> Option<&object::Object> {
        self.walk_context()
            .find_map(|ctx| ctx.values.get(&*info.lexeme))
    }
}
