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

pub(super) struct Environment {
    values: HashMap<String, object::Object>,
}

impl Environment {
    pub(super) fn new() -> Self {
        Environment {
            values: HashMap::new(),
        }
    }

    pub(super) fn define(&mut self, info: token::Info, value: object::Object) {
        self.values.insert(info.lexeme.into_owned(), value);
    }

    pub(super) fn assign(
        &mut self,
        info: &token::Info,
        value: object::Object,
    ) -> error_stack::Result<(), EnvironmentError> {
        self.values
            .get_mut(&*info.lexeme)
            .map(|obj| *obj = value)
            .ok_or_else(|| {
                error_stack::report!(EnvironmentError).attach_printable(format!(
                    "cannot assign to undefined variable {:?}",
                    info.lexeme
                ))
            })
    }

    pub(super) fn get(&self, info: &token::Info) -> Option<&object::Object> {
        self.values.get(&*info.lexeme)
    }
}
