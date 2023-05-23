use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};

#[derive(Debug, Clone, PartialEq)]
pub struct InstanceHandle<'s>(Rc<RefCell<Instance<'s>>>);

#[derive(Debug, Clone, PartialEq)]
pub struct Instance<'s> {
    class: super::ClassHandle<'s>,
    fields: HashMap<String, super::Object<'s>>,
}

impl<'s> Instance<'s> {
    pub fn new(class: super::ClassHandle<'s>) -> Self {
        Self {
            class,
            fields: HashMap::new(),
        }
    }
}

impl<'s> InstanceHandle<'s> {
    pub fn get(&self, name: &str) -> Option<super::Object<'s>> {
        self.0.borrow().fields.get(name).cloned().or_else(|| {
            self.0
                .borrow()
                .class
                .find_method(name)
                .map(|f| f.bind(self.clone()).into())
        })
    }

    pub fn set(&self, name: String, value: super::Object<'s>) {
        self.0.borrow_mut().fields.insert(name, value);
    }
}

impl fmt::Display for InstanceHandle<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.borrow().fmt(f)
    }
}

impl fmt::Display for Instance<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<class {} instance>", self.class.name())
    }
}

impl<'s> From<Instance<'s>> for InstanceHandle<'s> {
    fn from(value: Instance<'s>) -> Self {
        Self(Rc::new(RefCell::new(value)))
    }
}

impl<'s> From<InstanceHandle<'s>> for super::Object<'s> {
    fn from(value: InstanceHandle<'s>) -> Self {
        Self::Instance(value)
    }
}

impl<'s> From<Instance<'s>> for super::Object<'s> {
    fn from(value: Instance<'s>) -> Self {
        Self::Instance(value.into())
    }
}
