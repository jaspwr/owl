use crate::ir::{InstInner, IrSnippet, Value};

pub fn write_readable<T: std::io::Write>(to: &mut T, ir: &IrSnippet) -> std::io::Result<()> {
    for inst in &ir.insts {
        if let InstInner::Function { name, body } = &inst.inner {
            writeln!(to, "fn {} {{", name)?;
            write_readable(to, &body)?;
            writeln!(to, "}}")?;
            continue;
        }

        if !matches!(inst.inner, crate::ir::InstInner::Label(_)) {
            write!(to, "    ")?;
        }

        if let Some(assigns) = &inst.assigns {
            write!(to, "%{}:{:?} = ", assigns.as_vreg().unwrap(), assigns.type_)?;
        }

        match inst.inner.clone() {
            crate::ir::InstInner::BinOp { op, lhs, rhs } => {
                write!(to, "{:?} ", op)?;
                write_value(to, lhs)?;
                write_value(to, rhs)?;
            }
            crate::ir::InstInner::Call { fn_name, args } => {
                write!(to, "Call @{}(", fn_name)?;
                for arg in args {
                    write_value(to, arg)?;
                }
                write!(to, ")")?;
            }
            crate::ir::InstInner::Label(lab) => write!(to, "  lab{}:", lab)?,
            crate::ir::InstInner::Jnz(lab, value) => {
                write!(to, "Jnz ")?;
                write_value(to, value)?;
                write!(to, "lab{}:", lab)?;
            }
            crate::ir::InstInner::Jz(lab, value) => {
                write!(to, "Jz ")?;
                write_value(to, value)?;
                write!(to, "lab{}", lab)?;
            }
            crate::ir::InstInner::Jmp(lab) => {
                write!(to, "Jmp lab{}", lab)?;
            }
            crate::ir::InstInner::Load(vreg) => {
                write!(to, "Load ")?;
                write_value(to, vreg)?;
            }
            crate::ir::InstInner::Store(vreg, value) => {
                write!(to, "Store ")?;
                write_value(to, vreg)?;
                write_value(to, value)?;
            }
            crate::ir::InstInner::Copy(value) => {
                write!(to, "Copy ")?;
                write_value(to, value)?;
            }
            crate::ir::InstInner::Alloca => {
                write!(to, "Alloca")?;
            }
            crate::ir::InstInner::Lea(value) => {
                write!(to, "Lea ")?;
                write_value(to, value)?;
            }
            crate::ir::InstInner::Ret(value) => {
                write!(to, "Ret ")?;
                write_value(to, value)?;
            }
            crate::ir::InstInner::Function { .. } => {
                unreachable!()
            }
        }

        let _ = writeln!(to);
    }

    Ok(())
}

fn write_value(to: &mut impl std::io::Write, val: Value) -> std::io::Result<()> {
    match val.inner {
        super::ValueInner::Immediate(v) => write!(to, "0x{:x}", i128::from_le_bytes(v))?,
        super::ValueInner::Vreg(reg) => write!(to, "%{}", reg)?,
        super::ValueInner::ImmediateInt(v) => write!(to, "{}", v)?,
        super::ValueInner::ImmediateFloat(v) => write!(to, "{}_f32", v)?,
        super::ValueInner::ImmediateDouble(v) => write!(to, "{}_f64", v)?,
        super::ValueInner::ImmediateBool(v) => write!(to, "{}", v)?,
        super::ValueInner::StringLiteral(v) => write!(to, "\"{}\"", v)?,
    }

    write!(to, ":{:?} ", val.type_)?;

    Ok(())
}
