use std::fmt;

use crate::{ast, env, interpreter, object::callable, token};

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    name: Option<token::Info<'static>>,
    declaration: ast::expr::Function<'static>,
    closure: Option<env::ContextRef>,
}

impl Function {
    pub fn new(decl: &ast::expr::Function<'_>, closure: Option<env::ContextRef>) -> Self {
        Self {
            name: None,
            declaration: decl.clone().into_owned(),
            closure,
        }
    }

    pub fn attach_name(self, name: token::Info<'static>) -> Self {
        Self {
            name: Some(name),
            ..self
        }
    }
}

impl super::callable::Callable for Function {
    fn arity(&self) -> usize {
        self.declaration.parameters.len()
    }

    fn call(
        &self,
        interpreter: &mut crate::interpreter::Interpreter,
        arguments: Vec<super::Object>,
        _info: &crate::token::Info<'_>,
    ) -> Result<super::Object, interpreter::RuntimeErrorState> {
        let mut context = env::Context::new_with_parent(self.closure.clone());

        for arg in arguments {
            context.define(arg);
        }

        match interpreter.execute_block_swap_context(&self.declaration.body, context.info_ref()) {
            Ok(_) => Ok(super::Object::Nil),
            Err(state) => match state {
                interpreter::RuntimeErrorState::Return(v, _) => Ok(v),
                s => Err(s.into_error()),
            },
        }
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(name) = &self.name {
            write!(f, "<fn {}>", name.lexeme)
        } else {
            write!(f, "<fn>")
        }
    }
}

impl From<Function> for callable::CallableObject {
    fn from(value: Function) -> Self {
        Self::Function(value)
    }
}
