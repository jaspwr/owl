use std::sync::atomic::AtomicU64;

use crate::{grammar::BinaryOperation, ir::IrSnippet, types::Type};

#[derive(Debug, Clone)]
pub struct Inst {
    pub assigns: Option<Value>,
    pub inner: InstInner,
}

pub type InstLoc = usize;

#[derive(Debug, Clone)]
pub enum InstInner {
    Function {
        name: String,
        body: IrSnippet,
    },
    BinOp {
        op: BinaryOperation,
        lhs: Value,
        rhs: Value,
    },
    Call {
        fn_name: String,
        args: Vec<Value>,
    },
    Label(VregId),
    Jnz(VregId, Value),
    Jz(VregId, Value),
    Jmp(VregId),
    Ret(Value),
    Load(Value),
    Store(Value, Value),
    Copy(Value),
    Lea(Value),
    Alloca,
}

pub type VregId = u64;
pub static ID_COUNTER: AtomicU64 = AtomicU64::new(0);
pub fn new_id() -> VregId {
    ID_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
}

#[derive(Debug, Clone)]
pub struct Value {
    pub type_: Type,
    pub inner: ValueInner,
}

#[derive(Debug, Clone, Copy)]
pub enum ValueInner {
    Vreg(VregId),
    Immediate([u8; 16]),
    ImmediateInt(i64),
    ImmediateFloat(f32),
    ImmediateDouble(f64),
    ImmediateBool(bool),
    StringLiteral(usize),
}

pub fn new_vreg(type_: Type) -> Value {
    Value {
        type_,
        inner: ValueInner::Vreg(new_id()),
    }
}

impl Value {
    pub fn as_vreg(&self) -> Option<VregId> {
        if let ValueInner::Vreg(vreg) = self.inner {
            return Some(vreg);
        }

        None
    }

    pub fn int_immediate(arg: i64) -> Self {
        Self {
            type_: Type::new_auto(),
            inner: ValueInner::ImmediateInt(arg),
        }
    }

    pub fn float_immediate(arg: f64) -> Self {
        Self {
            type_: Type::new_auto(),
            inner: ValueInner::ImmediateDouble(arg),
        }
    }

    pub(crate) fn bool_immediate(arg: bool) -> Value {
        Self {
            type_: Type::Boolean,
            inner: ValueInner::ImmediateBool(arg),
        }
    }

    pub fn string_literal(string_id: usize) -> Value {
        Value {
            type_: Type::Ptr(Box::new(Type::Integer {
                bits: 8,
                signed: false,
            })),
            inner: ValueInner::StringLiteral(string_id),
        }
    }
}
