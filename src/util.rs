// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

use std::hash::{Hash, Hasher};
use std::ops::Deref;

#[derive(Clone, Debug)]
pub enum MaybeConstantString {
    Static(&'static str),
    Dynamic(Box<str>),
}

impl PartialEq for MaybeConstantString {
    fn eq(&self, other: &MaybeConstantString) -> bool {
        PartialEq::eq(&**self, &**other)
    }
}

impl Eq for MaybeConstantString {}

impl Hash for MaybeConstantString {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Deref::deref(self).hash(state);
    }
}

impl Deref for MaybeConstantString {
    type Target = str;
    
    fn deref(&self) -> &str {
        match self {
            MaybeConstantString::Static(value) => value,
            MaybeConstantString::Dynamic(value) => &*value,
        }
    }
}
