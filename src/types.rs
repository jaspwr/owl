use std::collections::HashSet;

use crate::{
    error::{CompErr, Range},
    grammar::NameAndType,
    ir::new_id,
};

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

pub fn bin_op_coerce(
    a: &Type,
    b: &Type,
    ctx: &mut crate::ir::GenContext,
    range: Range,
) -> Result<Type, CompErr> {
    if *a == *b {
        return Ok(a.clone());
    }

    match (a, b) {
        (Type::Auto(a_ids), Type::Auto(b_ids)) => {
            let mut ids = a_ids.clone();
            ids.extend(b_ids);
            return Ok(Type::Auto(ids));
        }
        (Type::Auto(a_ids), b) => {
            for id in a_ids {
                auto_with_known(ctx, &range, b, id)?;
            }
            return Ok(b.clone());
        }
        (a, Type::Auto(b_ids)) => {
            for id in b_ids {
                auto_with_known(ctx, &range, a, id)?;
            }
            return Ok(a.clone());
        }
        _ => {}
    }

    return CompErr::new_general(
        format!("Could not operate on types {:?} and {:?}", a, b),
        range,
    );
}

fn auto_with_known(
    ctx: &mut crate::ir::GenContext,
    range: &Range,
    b: &Type,
    id: &u64,
) -> Result<(), CompErr> {
    assert!(!matches!(b, Type::Auto(_)));

    if let Some(t) = ctx.auto_map.get(id) {
        if t != b {
            return CompErr::new_general(
                format!(
                    "Type could not be inferred. Is used as both {:?} and {:?}",
                    t, b
                ),
                range.clone(),
            );
        }
    }
    ctx.auto_map.insert(*id, b.clone());
    Ok(())
}
