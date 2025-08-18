use crate::grammar::NameAndType;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum Type {
    #[default]
    Auto,
    AutoFloat,
    AutoInt,
    Void,
    Boolean,
    F64,
    Ptr(Box<Type>),
    Struct(Vec<NameAndType>),
    Ident(String),
    GenericInstance {
        base: Box<Type>,
        args: Vec<Type>
    }
}

impl Type {
    pub fn deref(self) -> Option<Self> {
        if let Self::Ptr(t) = self {
            return Some(*t);
        }

        None
    }
}
