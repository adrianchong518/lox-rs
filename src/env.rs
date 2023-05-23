use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};

use crate::{object, resolver};

#[derive(Debug)]
pub struct EnvironmentError;

impl fmt::Display for EnvironmentError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "environment error")
    }
}

impl error_stack::Context for EnvironmentError {}

#[derive(Debug, Clone, PartialEq)]
pub struct Context<'s> {
    values: Vec<object::Object<'s>>,
    parent: Option<ContextRef<'s>>,
}

pub type ContextRef<'s> = Rc<RefCell<Context<'s>>>;

impl<'s> Context<'s> {
    pub fn new() -> Self {
        Self {
            values: Vec::new(),
            parent: None,
        }
    }

    pub fn new_with_parent(parent: Option<ContextRef<'s>>) -> Self {
        Self {
            values: Vec::new(),
            parent,
        }
    }

    pub fn info_ref(self) -> ContextRef<'s> {
        Rc::new(RefCell::new(self))
    }

    pub fn define(&mut self, value: object::Object<'s>) {
        self.values.push(value);
    }

    pub fn assign(
        &mut self,
        slot: usize,
        value: object::Object<'s>,
    ) -> error_stack::Result<(), EnvironmentError> {
        let Some(var) = self.values.get_mut(slot) else {
            return Err(error_stack::report!(EnvironmentError)
                .attach_printable(format!("slot {slot} out of bounds")));
        };

        *var = value;
        Ok(())
    }

    pub fn get(&self, slot: usize) -> Option<object::Object<'s>> {
        self.values.get(slot).cloned()
    }
}

impl Default for Context<'_> {
    fn default() -> Self {
        Self::new()
    }
}

/// Execution environment for interpreter
#[derive(Debug)]
pub struct Environment<'s> {
    global: HashMap<String, object::Object<'s>>,
    context: Option<ContextRef<'s>>,
}

impl<'s> Environment<'s> {
    pub fn new() -> Self {
        let mut env = Environment {
            global: HashMap::new(),
            context: None,
        };

        env.define_global("clock".to_string(), object::clock_fn::ClockFn.into());

        env
    }

    pub fn push_context(&mut self, context: ContextRef<'s>) {
        context.borrow_mut().parent = self.context.take();
        self.context = Some(context);
    }

    pub fn pop_context(&mut self) -> Option<ContextRef<'s>> {
        let old = self.context.take()?;
        self.context = old.borrow_mut().parent.take();

        Some(old)
    }

    pub fn context(&self) -> Option<&ContextRef<'s>> {
        self.context.as_ref()
    }

    pub fn swap_context(&mut self, new: Option<ContextRef<'s>>) -> Option<ContextRef<'s>> {
        match (&mut self.context, new) {
            (old, mut new @ Some(_)) => {
                std::mem::swap(old, &mut new);
                new
            }
            (old, None) => old.take(),
        }
    }

    fn ancestor(&self, distance: usize) -> Option<ContextRef<'s>> {
        let mut ancestor = self.context.as_ref()?.clone();
        for _ in 0..distance {
            let next = ancestor.borrow().parent.as_ref()?.clone();
            ancestor = next;
        }
        Some(ancestor)
    }

    pub fn define(&mut self, name: String, value: object::Object<'s>) {
        if let Some(ctx) = &self.context {
            ctx.borrow_mut().define(value);
        } else {
            self.define_global(name, value);
        }
    }

    pub fn define_global(&mut self, name: String, value: object::Object<'s>) {
        self.global.insert(name, value);
    }

    pub fn assign_at(
        &mut self,
        location: &resolver::VariableLocation,
        value: object::Object<'s>,
    ) -> error_stack::Result<(), EnvironmentError> {
        self.ancestor(location.distance)
            .expect("resolved distance is valid")
            .borrow_mut()
            .assign(location.slot, value)
    }

    pub fn assign_global(
        &mut self,
        name: &str,
        value: object::Object<'s>,
    ) -> error_stack::Result<(), EnvironmentError> {
        self.global
            .get_mut(name)
            .map(|var| *var = value)
            .ok_or_else(|| {
                error_stack::report!(EnvironmentError)
                    .attach_printable(format!("cannot assign to undeclared variable `{name}`"))
            })
    }

    pub fn get_at(&self, location: &resolver::VariableLocation) -> Option<object::Object<'s>> {
        self.ancestor(location.distance)?
            .borrow()
            .get(location.slot)
    }

    pub fn get_global(&self, name: &str) -> Option<object::Object<'s>> {
        self.global.get(name).cloned()
    }
}
