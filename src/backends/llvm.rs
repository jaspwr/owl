use crate::data::Data;
use crate::grammar::BinaryOperation;
use crate::ir::{new_id, InstInner, IrSnippet, Value, VregId};
use crate::types::Type;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
};
use inkwell::types::{AnyType, BasicMetadataTypeEnum, BasicType};
use inkwell::values::{BasicValue, BasicValueEnum, InstructionOpcode};
use inkwell::{AddressSpace, OptimizationLevel};

use std::collections::HashMap;
use std::path::Path;

use std::error::Error;

/// Convenience type alias for the `sum` function.
///
/// Calling this is innately `unsafe` because there's no guarantee it doesn't
/// do `unsafe` operations internally.
type SumFunc = unsafe extern "C" fn(u64, u64, u64) -> u64;

pub fn codegen(ir: IrSnippet, data: Data, path: impl AsRef<Path>) {
    let opt = OptimizationLevel::None;
    let reloc = RelocMode::Default;
    let model = CodeModel::Default;

    Target::initialize_all(&InitializationConfig::default());

    let triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&triple).unwrap();
    // let target = Target::from_name("x86-64").unwrap();
    let target_machine = target
        .create_target_machine(&triple, "x86-64", "+avx2", opt, reloc, model)
        .unwrap();

    let context = Context::create();
    let module = context.create_module("sum");

    module.set_data_layout(&target_machine.get_target_data().get_data_layout());

    let builder = context.create_builder();

    let i8_type = context.i8_type();
    let i32_type = context.i32_type();
    let i64_type = context.i64_type();
    let void_type = context.void_type();

    let fn_type = i32_type.fn_type(&[], false);
    let function = module.add_function("main", fn_type, Some(inkwell::module::Linkage::External));
    let basic_block = context.append_basic_block(function, "entry");

    let ptr_type = context.ptr_type(AddressSpace::default());

    let fn_type = i32_type.fn_type(&[ptr_type.into()], true);
    let printf = module.add_function("printf", fn_type, Some(inkwell::module::Linkage::External));

    builder.position_at_end(basic_block);

    let mut basic_blocks: HashMap<VregId, BasicBlock> = HashMap::new();

    for inst in &ir.insts {
        if let InstInner::Label(l) = inst.inner {
            basic_blocks.insert(l, context.append_basic_block(function, &format!("l{}", l)));
        }
    }

    let mut values: HashMap<VregId, BasicValueEnum> = HashMap::new();

    let string_literals = data
        .string_literals
        .into_iter()
        .enumerate()
        .map(|(i, s)| {
            builder
                .build_global_string_ptr(&format!("{}\0", s), &format!("s{}", i))
                .unwrap()
                .as_basic_value_enum()
        })
        .collect::<Vec<_>>();

    macro_rules! access {
        ($v:expr) => {
            match $v.inner {
                crate::ir::ValueInner::Vreg(vreg) => values.get(&vreg).unwrap(),
                crate::ir::ValueInner::Immediate(_) => todo!(),
                crate::ir::ValueInner::ImmediateInt(int) => {
                    let v = context
                        .i32_type()
                        .const_int(int as u64, false)
                        .as_basic_value_enum();
                    values.insert(999 + int as VregId, v);
                    values.get(&(999 + int as VregId)).unwrap()
                }
                crate::ir::ValueInner::ImmediateFloat(_) => todo!(),
                crate::ir::ValueInner::ImmediateDouble(_) => todo!(),
                crate::ir::ValueInner::StringLiteral(id) => &string_literals[id],
            }
        };
    }

    let get_type = |t: Type| match t {
        Type::Auto(hash_set) => todo!(),
        Type::Void => todo!(),
        Type::F32 => context.f32_type().as_basic_type_enum(),
        Type::F64 => context.f64_type().as_basic_type_enum(),
        Type::Boolean => context.bool_type().as_basic_type_enum(),
        Type::Integer {
            bits: 1,
            signed: true,
        } => context.bool_type().as_basic_type_enum(),
        Type::Integer {
            bits: 16,
            signed: true,
        } => context.i16_type().as_basic_type_enum(),
        Type::Integer {
            bits: 32,
            signed: true,
        } => context.i32_type().as_basic_type_enum(),
        Type::Integer {
            bits: 64,
            signed: true,
        } => context.i64_type().as_basic_type_enum(),
        Type::Integer {
            bits: 128,
            signed: true,
        } => context.i128_type().as_basic_type_enum(),
        Type::Integer { bits, signed } => context.i32_type().as_basic_type_enum(),
        Type::Ptr(_) => context.ptr_type(AddressSpace::default()).as_basic_type_enum(),
        Type::Struct(name_and_types) => todo!(),
        Type::Ident(_) => todo!(),
        Type::GenericInstance { base, args } => todo!(),
    };

    for inst in ir.insts {
        let t = inst.assigns.as_ref().map(|a| a.type_.clone());
        let vreg = inst.assigns.and_then(|m| m.as_vreg());

        let vreg_s = vreg.as_ref().map(|m| format!("r{}", m));

        match inst.inner {
            InstInner::Function { name, body } => {}
            InstInner::Alloca => {
                let v = builder.build_alloca(get_type(t.unwrap().deref().unwrap()), &vreg_s.unwrap()).unwrap();

                values.insert(vreg.unwrap(), v.as_basic_value_enum());
            }
            InstInner::BinOp { op, lhs, rhs } => {
                let x = access!(lhs).clone();
                let y = access!(rhs);
                match op {
                    BinaryOperation::Add => {
                        let v = builder
                            .build_int_add(x.into_int_value(), y.into_int_value(), &vreg_s.unwrap())
                            .unwrap();
                        values.insert(vreg.unwrap(), v.as_basic_value_enum());
                    }
                    BinaryOperation::Sub => {
                        let v = builder
                            .build_int_sub(x.into_int_value(), y.into_int_value(), &vreg_s.unwrap())
                            .unwrap();
                        values.insert(vreg.unwrap(), v.as_basic_value_enum());
                    }
                    BinaryOperation::Mul => {
                        let v = builder
                            .build_int_mul(x.into_int_value(), y.into_int_value(), &vreg_s.unwrap())
                            .unwrap();
                        values.insert(vreg.unwrap(), v.as_basic_value_enum());
                    }
                    BinaryOperation::Div => {
                        let v = builder
                            .build_int_sub(x.into_int_value(), y.into_int_value(), &vreg_s.unwrap())
                            .unwrap();
                        values.insert(vreg.unwrap(), v.as_basic_value_enum());
                    }
                    BinaryOperation::Mod => {
                        let v = builder
                            .build_int_signed_rem(x.into_int_value(), y.into_int_value(), &vreg_s.unwrap())
                            .unwrap();
                        values.insert(vreg.unwrap(), v.as_basic_value_enum());
                    }
                    BinaryOperation::Xor => {
                        let v = builder
                            .build_xor(x.into_int_value(), y.into_int_value(), &vreg_s.unwrap())
                            .unwrap();
                        values.insert(vreg.unwrap(), v.as_basic_value_enum());
                    }
                    BinaryOperation::And => {
                        let v = builder
                            .build_and(x.into_int_value(), y.into_int_value(), &vreg_s.unwrap())
                            .unwrap();
                        values.insert(vreg.unwrap(), v.as_basic_value_enum());
                    }
                    BinaryOperation::Eq
                    | BinaryOperation::Neq
                    | BinaryOperation::Gte
                    | BinaryOperation::Gt
                    | BinaryOperation::Lt
                    | BinaryOperation::Lte => {
                        let pred = match op {
                            BinaryOperation::Eq => inkwell::IntPredicate::EQ,
                            BinaryOperation::Neq => inkwell::IntPredicate::NE,
                            BinaryOperation::Gt => inkwell::IntPredicate::SGT,
                            BinaryOperation::Lt => inkwell::IntPredicate::SLT,
                            BinaryOperation::Lte => inkwell::IntPredicate::SLE,
                            BinaryOperation::Gte => inkwell::IntPredicate::SGE,
                            _ => unreachable!(),
                        };

                        let v = builder
                            .build_int_compare(
                                pred,
                                x.into_int_value(),
                                y.into_int_value(),
                                &vreg_s.unwrap(),
                            )
                            .unwrap();
                        values.insert(vreg.unwrap(), v.as_basic_value_enum());
                    }
                    _ => todo!("{:?}", op),
                }
            }
            InstInner::Load(value) => {
                let v = builder
                    .build_load(
                        i32_type,
                        access!(value).into_pointer_value(),
                        &vreg_s.unwrap(),
                    )
                    .unwrap();
                values.insert(vreg.unwrap(), v.as_basic_value_enum());
            }
            InstInner::Store(loc, value) => {
                builder
                    .build_store(
                        access!(loc).into_pointer_value(),
                        access!(value).into_int_value(),
                    )
                    .unwrap();
            }
            InstInner::Call { fn_name, args } => {
                let args = args
                    .into_iter()
                    .map(|arg| (*access!(arg)).into())
                    .collect::<Vec<_>>();
                builder
                    .build_call(printf, args.as_slice(), &vreg_s.unwrap())
                    .unwrap();
            }
            InstInner::Ret(value) => {
                builder.build_return(Some(access!(value))).unwrap();
            }
            InstInner::Label(l) => {
                let lab = basic_blocks.get(&l).unwrap();

                if !builder
                    .get_insert_block()
                    .unwrap()
                    .get_last_instruction()
                    .map(|i| i.is_terminator())
                    .unwrap_or(false)
                {
                    builder.build_unconditional_branch(*lab).unwrap();
                }

                builder.position_at_end(*lab);
            }
            InstInner::Jmp(loc) => {
                let bb = basic_blocks.get(&loc).unwrap().clone();
                builder.build_unconditional_branch(bb).unwrap();
            }
            InstInner::Jz(loc, value) => {
                let bb = basic_blocks.get(&loc).unwrap().clone();
                let new_bb =
                    context.append_basic_block(function, &format!("fallthrough{}", new_id()));
                builder
                    .build_conditional_branch(access!(value).into_int_value(), new_bb, bb)
                    .unwrap();
                builder.position_at_end(new_bb);
            }
            InstInner::Cast(value) => {
                // let v = builder.build_cast(
                //     InstructionOpcode::Add,
                //     access!(value).into_int_value(),
                //     get_type(value.type_),
                //     &vreg_s.unwrap(),
                // ).unwrap();
                // values.insert(vreg.unwrap(), v.as_basic_value_enum());
            }
        }
    }

    // let x = function.get_nth_param(0).unwrap().into_int_value();
    // let y = function.get_nth_param(1).unwrap().into_int_value();
    // let z = function.get_nth_param(2).unwrap().into_int_value();
    //
    // let sum = builder.build_int_add(x, y, "main").unwrap();
    // let x = context.i32_type().const_int(42, false);
    // let y = context.i32_type().const_int(42, false);
    // let sum = builder.build_int_add(x, y, "b").unwrap();
    //
    // let s = builder
    //     .build_global_string_ptr("Hello, Owl!\n\0", "asdhkj")
    //     .unwrap();
    //
    // builder
    //     .build_call(printf, &[s.as_pointer_value().into()], "main")
    //     .unwrap();
    //
    // let bleh = context.i32_type().const_int(42, false);
    //
    // builder.build_return(Some(&sum)).unwrap();
    // builder.build_return(Some(&bleh)).unwrap();

    module.print_to_stderr();

    target_machine
        .write_to_file(&module, FileType::Object, path.as_ref())
        .unwrap();
}
