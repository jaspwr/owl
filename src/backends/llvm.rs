use crate::data::Data;
use crate::ir::{IrSnippet, Value, VregId};

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
};
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::values::{BasicValue, BasicValueEnum};
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
                crate::ir::ValueInner::ImmediateBool(_) => todo!(),
                crate::ir::ValueInner::StringLiteral(id) => &string_literals[id],
            }
        };
    }

    for inst in ir.insts {
        let vreg = inst.assigns.and_then(|m| m.as_vreg());

        let vreg_s = vreg.as_ref().map(|m| format!("r{}", m));

        match inst.inner {
            crate::ir::InstInner::Alloca => {
                let v = builder.build_alloca(i64_type, &vreg_s.unwrap()).unwrap();

                values.insert(vreg.unwrap(), v.as_basic_value_enum());
            }
            crate::ir::InstInner::BinOp { op, lhs, rhs } => {
                let x = access!(lhs).clone();
                let y = access!(rhs);
                let v = builder.build_int_add(x.into_int_value(), y.into_int_value(), &vreg_s.unwrap()).unwrap();
                values.insert(vreg.unwrap(), v.as_basic_value_enum());
            }
            crate::ir::InstInner::Load(value) => {
                // let v = builder.build_load(access!(value), &vreg_s.unwrap()).unwrap();
                // values.insert(vreg.unwrap(), v.as_basic_value_enum());
            }
            crate::ir::InstInner::Call { fn_name, args } => {
                let args = args
                    .into_iter()
                    .map(|arg| (*access!(arg)).into())
                    .collect::<Vec<_>>();
                builder
                    .build_call(printf, args.as_slice(), &vreg_s.unwrap())
                    .unwrap();
            }
            crate::ir::InstInner::Ret(value) => {
                // let bleh = context.i32_type().const_int(0, false);
                builder.build_return(Some(access!(value))).unwrap();
            }
            _ => {}
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

    target_machine
        .write_to_file(&module, FileType::Object, path.as_ref())
        .unwrap();

    module.print_to_stderr();
}
