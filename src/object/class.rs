use std::{collections::HashMap, fmt, ops::Deref, rc::Rc};

use crate::{interpreter, object::callable, token};

#[derive(Debug, Clone, PartialEq)]
pub struct ClassHandle<'s>(Rc<Class<'s>>);

#[derive(Debug, Clone, PartialEq)]
pub struct Class<'s> {
    name: token::Info<'s>,

    /// The `Class` is a metaclass if this is `None`
    metaclass: Option<ClassHandle<'s>>,

    superclass: Option<ClassHandle<'s>>,

    methods: HashMap<String, super::Function<'s>>,
}

impl<'s> Class<'s> {
    pub const INITIALIZER_NAME: &str = "init";

    pub fn new(
        name: token::Info<'s>,
        metaclass: Option<ClassHandle<'s>>,
        superclass: Option<ClassHandle<'s>>,
        methods: HashMap<String, super::Function<'s>>,
    ) -> Self {
        Self {
            name,
            metaclass,
            superclass,
            methods,
        }
    }

    pub fn name(&self) -> &str {
        &self.name.lexeme
    }

    pub fn find_method(&self, name: &str) -> Option<super::Function<'s>> {
        self.methods.get(name).cloned().or_else(|| {
            self.superclass
                .as_ref()
                .and_then(|superclass| superclass.find_method(name))
        })
    }

    pub fn metaclass_instance(&self) -> Option<super::InstanceHandle<'s>> {
        self.metaclass
            .as_ref()
            .map(|meta| super::Instance::new(meta.clone()).into())
    }

    fn initializer(&self) -> Option<super::Function<'s>> {
        self.find_method(Self::INITIALIZER_NAME)
    }
}

impl<'s> callable::Callable<'s> for ClassHandle<'s> {
    fn arity(&self) -> usize {
        self.initializer().map_or(0, |i| i.arity())
    }

    fn call(
        &self,
        interpreter: &mut interpreter::Interpreter<'s>,
        arguments: Vec<super::Object<'s>>,
        info: &token::Info<'s>,
    ) -> Result<super::Object<'s>, interpreter::RuntimeErrorState<'s>> {
        let instance: super::InstanceHandle = super::Instance::new(self.clone()).into();
        if let Some(initializer) = self.initializer() {
            initializer
                .bind(instance.clone())
                .call(interpreter, arguments, info)?;
        }

        Ok(instance.into())
    }
}

impl<'s> Deref for ClassHandle<'s> {
    type Target = Class<'s>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl fmt::Display for Class<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<class {}>", self.name.lexeme)
    }
}

impl fmt::Display for ClassHandle<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<'s> From<Class<'s>> for ClassHandle<'s> {
    fn from(value: Class<'s>) -> Self {
        Self(Rc::new(value))
    }
}

impl<'s> From<Class<'s>> for callable::CallableObject<'s> {
    fn from(value: Class<'s>) -> Self {
        Self::Class(value.into())
    }
}

impl<'s> From<ClassHandle<'s>> for callable::CallableObject<'s> {
    fn from(value: ClassHandle<'s>) -> Self {
        Self::Class(value)
    }
}
