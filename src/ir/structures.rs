use std::sync::atomic::AtomicU64;

use crate::{grammar::BinaryOperation, types::Type};

#[derive(Debug, Clone)]
pub struct Inst {
    pub assigns: Option<Value>,
    pub inner: InstInner,
}

pub type InstLoc = usize;

#[derive(Debug, Clone)]
pub enum InstInner {
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
    Immediate([u8; 16]),
    Vreg(VregId),
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
}

