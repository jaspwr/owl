use crate::{
    ir::{GenContext, IrSnippet, Value},
    types::Type,
};

pub fn backpatch_types(ctx: &mut GenContext, ir: &mut IrSnippet) {
    for inst in ir.insts.iter_mut() {
        if let Some(Value { type_, .. }) = &mut inst.assigns {
            patch_type(ctx, type_);
        }

        match &mut inst.inner {
            super::InstInner::Function { body, .. } => {
                backpatch_types(ctx, body);
            }
            super::InstInner::BinOp { lhs, rhs, .. } => {
                patch_type(ctx, &mut lhs.type_);
                patch_type(ctx, &mut rhs.type_);
            }
            super::InstInner::Load(value) => {
                patch_type(ctx, &mut value.type_);
            }
            super::InstInner::Store(value, value1) => {
                patch_type(ctx, &mut value.type_);
                patch_type(ctx, &mut value1.type_);
            }
            super::InstInner::Call { args, .. } => {
                for value in args {
                    patch_type(ctx, &mut value.type_);
                }
            }
            super::InstInner::Ret(value) => {
                patch_type(ctx, &mut value.type_);
            }
            _ => {}
        }
    }
}

fn patch_type(ctx: &mut GenContext, type_: &mut Type) {
    if let Type::Auto(ids) = type_.clone() {
        for id in &ids {
            let new_type = ctx.auto_map.get(&id).cloned();
            if let Some(new_type) = new_type {
                // Propagate to others
                for id in ids {
                    assert!(!matches!(new_type, Type::Auto(_)));
                    ctx.auto_map.insert(id, new_type.clone());
                }

                // Set our self
                *type_ = new_type;

                return;
            }
        }
    }

    if let Type::Ptr(t) = type_ {
        patch_type(ctx, &mut *t);
    }
}
