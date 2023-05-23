use std::fmt;

use crate::{
    ast, env, interpreter,
    object::{callable, InstanceHandle},
    token,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Function<'s> {
    name: Option<token::Info<'s>>,
    declaration: ast::expr::Function<'s>,
    closure: Option<env::ContextRef<'s>>,
    is_initializer: bool,
}

impl<'s> Function<'s> {
    pub fn new(
        decl: &ast::expr::Function<'s>,
        closure: Option<env::ContextRef<'s>>,
        is_initializer: bool,
    ) -> Self {
        Self {
            name: None,
            declaration: decl.clone().into_owned(),
            closure,
            is_initializer,
        }
    }

    pub fn attach_name(self, name: token::Info<'s>) -> Self {
        Self {
            name: Some(name),
            ..self
        }
    }

    pub fn bind(self, instance: InstanceHandle<'s>) -> Self {
        let mut closure = env::Context::new_with_parent(self.closure);
        closure.define(instance.into());

        Self {
            closure: Some(closure.info_ref()),
            ..self
        }
    }

    pub fn is_getter(&self) -> bool {
        self.declaration.parameters.is_none()
    }
}

impl<'s> callable::Callable<'s> for Function<'s> {
    fn arity(&self) -> usize {
        self.declaration.parameters.as_ref().map_or(0, |p| p.len())
    }

    fn call(
        &self,
        interpreter: &mut crate::interpreter::Interpreter<'s>,
        arguments: Vec<super::Object<'s>>,
        _info: &crate::token::Info<'s>,
    ) -> Result<super::Object<'s>, interpreter::RuntimeErrorState<'s>> {
        let mut context = env::Context::new_with_parent(self.closure.clone());

        if !self.is_getter() {
            for arg in arguments {
                context.define(arg);
            }
        }

        let return_value = match interpreter
            .execute_block_swap_context(&self.declaration.body, context.info_ref())
        {
            Ok(_) => super::Object::Nil,
            Err(state) => match state {
                interpreter::RuntimeErrorState::Return(v, _) => v.clone(),
                s => return Err(s.into_error()),
            },
        };

        Ok(if self.is_initializer {
            // rust hack to get `this`
            self.closure
                .as_ref()
                .unwrap()
                .borrow()
                .get(0)
                .unwrap()
                .clone()
        } else {
            return_value
        })
    }
}

impl fmt::Display for Function<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(name) = &self.name {
            write!(f, "<fn {}>", name.lexeme)
        } else {
            write!(f, "<fn>")
        }
    }
}

impl<'s> From<Function<'s>> for callable::CallableObject<'s> {
    fn from(value: Function<'s>) -> Self {
        Self::Function(value)
    }
}
