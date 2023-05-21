use std::fmt;

use crate::{ast, env, interpreter, object::callable};

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    declaration: ast::stmt::Function<'static>,
    closure: env::ContextRef,
}

impl Function {
    pub fn new(decl: &ast::stmt::Function<'_>, closure: env::ContextRef) -> Self {
        Self {
            declaration: decl.clone().into_owned(),
            closure,
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
        let mut context = env::Context::new_with_parent(Some(self.closure.clone()));

        for (param, arg) in self.declaration.parameters.iter().zip(arguments) {
            context.define(param.clone().lexeme.into_owned(), arg);
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
        write!(f, "<fn {}>", self.declaration.info.lexeme)
    }
}

impl From<Function> for callable::CallableObject {
    fn from(value: Function) -> Self {
        Self::Function(value)
    }
}
