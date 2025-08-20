use std::collections::HashSet;

use crate::{grammar::NameAndType, ir::new_id};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Auto(HashSet<UnknownTypeId>),
    // AutoFloat(HashSet<UnknownTypeId>),
    // AutoInt(HashSet<UnknownTypeId>),
    Void,
    Boolean,
    Integer { bits: u32, signed: bool },
    F32,
    F64,
    Ptr(Box<Type>),
    Struct(Vec<NameAndType>),
    Ident(String),
    GenericInstance { base: Box<Type>, args: Vec<Type> },
}

impl Default for Type {
    fn default() -> Self {
        Self::new_auto()
    }
}

pub type UnknownTypeId = u64;

impl Type {
    pub fn deref(self) -> Option<Self> {
        if let Self::Ptr(t) = self {
            return Some(*t);
        }

        None
    }

    pub fn new_auto() -> Self {
        let mut set = HashSet::new();
        set.insert(new_id());
        Self::Auto(set)
    }
}

pub fn bin_op_coerce(a: &Type, b: &Type) -> Option<Type> {
    if *a == *b {
        return Some(a.clone());
    }

    match (a, b) {
        (Type::Auto(a_ids), Type::Auto(b_ids)) => {
            let mut ids = a_ids.clone();
            ids.extend(b_ids);
            return Some(Type::Auto(ids));
        }
        _ => {}
    }

    None
}
