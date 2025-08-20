use std::collections::HashMap;

use crate::{
    ir::{InstInner, InstLoc, IrSnippet},
    reg_alloc::{self, PoolId, RegType},
};

pub fn exec(ir: IrSnippet) {
    let allocs = reg_alloc::alloc(&ir);
    println!("{:?}", allocs);

    let mut float_regs = [0_f32; 32];
    let mut double_regs = [0_f64; 32];

    let mut long_regs = [0_u64; 32];

    let access = |pool: PoolId, index: usize| match pool {
        PoolId {
            reg_type: RegType::Float,
            bits: 32,
        } => &mut float_regs[index] as *mut _ as *mut (),
        PoolId {
            reg_type: RegType::Float,
            bits: 64,
        } => &mut double_regs[index] as *mut _ as *mut (),
        PoolId {
            reg_type: RegType::Int,
            bits: 64,
        } => &mut long_regs[index] as *mut _ as *mut (),
        _ => panic!(),
    };

    let mut label_map = HashMap::new();
    // let mut vreg_map = HashMap::new();

    for (i, inst) in ir.insts.iter().enumerate() {
        if let InstInner::Label(l) = inst.inner {
            label_map.insert(l, i);
        }
    }

    let mut pc: InstLoc = 0;

    while pc < ir.insts.len() {
        let inst = &ir.insts[pc];

        // match &inst.inner {
        //     InstInner::BinOp { op, lhs, rhs } => {}
        //     InstInner::Call { fn_name, args } => todo!(),
        //     InstInner::Label(_) => {}
        //     InstInner::Jnz(_, value) => todo!(),
        //     InstInner::Jz(_, value) => todo!(),
        //     InstInner::Jmp(l) => pc = *label_map.get(&l).unwrap(),
        //     InstInner::Load(_) => todo!(),
        //     InstInner::Store(_, value) => todo!(),
        //     InstInner::Copy(value) => todo!(),
        //     InstInner::Alloca => todo!(),
        //     InstInner::Lea(value) => todo!(),
        //     InstInner::Function { name, body } => todo!(),
        // }

        pc += 1;
    }
}
