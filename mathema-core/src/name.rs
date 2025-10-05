use std::{fmt::Display, ops::Deref, rc::Rc};

#[repr(transparent)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name(Rc<str>);

impl Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Deref for Name {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Name {
    pub fn new(str: &str) -> Self {
        Name(Rc::from(str))
    }
}
