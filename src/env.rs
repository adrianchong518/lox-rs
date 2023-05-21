use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};

use crate::object;

#[derive(Debug)]
pub struct EnvironmentError;

impl fmt::Display for EnvironmentError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "environment error")
    }
}

impl error_stack::Context for EnvironmentError {}

#[derive(Debug, Clone, PartialEq)]
pub struct Context {
    values: HashMap<String, object::Object>,
    parent: Option<ContextRef>,
}

pub type ContextRef = Rc<RefCell<Context>>;

impl Context {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            parent: None,
        }
    }

    pub fn new_with_parent(parent: Option<ContextRef>) -> Self {
        Self {
            values: HashMap::new(),
            parent,
        }
    }

    pub fn info_ref(self) -> ContextRef {
        Rc::new(RefCell::new(self))
    }

    pub fn define(&mut self, name: String, value: object::Object) {
        self.values.insert(name, value);
    }

    pub fn assign(
        &mut self,
        name: &str,
        value: object::Object,
    ) -> error_stack::Result<(), EnvironmentError> {
        if let Some(var) = self.values.get_mut(name) {
            *var = value;
            Ok(())
        } else {
            self.parent.as_mut().map_or_else(
                || {
                    Err(
                        error_stack::report!(EnvironmentError).attach_printable(format!(
                            "cannot assign to undefined variable {:?}",
                            name
                        )),
                    )
                },
                |p| p.borrow_mut().assign(name, value),
            )
        }
    }

    pub fn get(&self, name: &str) -> Option<object::Object> {
        self.values
            .get(name)
            .cloned()
            .or_else(|| self.parent.as_ref().and_then(|p| p.borrow_mut().get(name)))
    }
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

/// Execution environment for interpreter
#[derive(Debug)]
pub struct Environment {
    global: ContextRef,
    context: Option<ContextRef>,
}

impl Environment {
    pub fn new() -> Self {
        let mut env = Environment {
            global: Rc::new(RefCell::new(Context::new())),
            context: None,
        };

        env.define("clock".to_string(), object::clock_fn::ClockFn.into());

        env
    }

    pub fn push_context(&mut self, context: ContextRef) {
        context.borrow_mut().parent = Some(self.current_context().clone());
        self.context = Some(context);
    }

    pub fn pop_context(&mut self) -> Option<ContextRef> {
        let old = self.context.take()?;
        self.context = old.borrow_mut().parent.take();

        Some(old)
    }

    pub fn swap_context(&mut self, new: Option<ContextRef>) -> Option<ContextRef> {
        match (&mut self.context, new) {
            (Some(old), Some(mut new)) => {
                std::mem::swap(old, &mut new);
                Some(new)
            }
            (None, Some(new)) => {
                self.push_context(new);
                None
            }
            (old, None) => old.take(),
        }
    }

    pub fn current_context(&self) -> &ContextRef {
        self.context.as_ref().unwrap_or(&self.global)
    }

    fn ancestor(&self, distance: usize) -> Option<ContextRef> {
        let mut ancestor = self.context.as_ref()?.clone();
        for _ in 0..distance {
            let next = ancestor.borrow().parent.as_ref()?.clone();
            ancestor = next;
        }
        Some(ancestor)
    }

    pub fn define(&mut self, name: String, value: object::Object) {
        self.current_context().borrow_mut().define(name, value)
    }

    pub fn assign(
        &mut self,
        name: &str,
        value: object::Object,
    ) -> error_stack::Result<(), EnvironmentError> {
        self.current_context().borrow_mut().assign(name, value)
    }

    pub fn assign_at(
        &mut self,
        name: &str,
        value: object::Object,
        distance: usize,
    ) -> error_stack::Result<(), EnvironmentError> {
        self.ancestor(distance)
            .expect("resolved distance is valid")
            .borrow_mut()
            .assign(name, value)
    }

    pub fn assign_global(
        &mut self,
        name: &str,
        value: object::Object,
    ) -> error_stack::Result<(), EnvironmentError> {
        self.global.borrow_mut().assign(name, value)
    }

    pub fn get(&self, name: &str) -> Option<object::Object> {
        self.current_context().borrow().get(name)
    }

    pub fn get_global(&self, name: &str) -> Option<object::Object> {
        self.global.borrow().get(name)
    }

    pub fn get_at(&self, name: &str, distance: usize) -> Option<object::Object> {
        self.ancestor(distance)?.borrow().get(name)
    }
}
