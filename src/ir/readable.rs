use crate::ir::{IrSnippet, Value};

pub fn write_readable(mut to: impl std::io::Write, ir: &IrSnippet) -> std::io::Result<()> {
    for inst in &ir.insts {
        if !matches!(inst.inner, crate::ir::InstInner::Label(_)) {
            write!(to, "    ")?;
        }

        if let Some(assigns) = &inst.assigns {
            write!(to, "%{}:{:?} = ", assigns.as_vreg().unwrap(), assigns.type_)?;
        }

        match inst.inner.clone() {
            crate::ir::InstInner::BinOp { op, lhs, rhs } => {
                write!(to, "{:?} ", op)?;
                write_value(&mut to, lhs)?;
                write_value(&mut to, rhs)?;
            }
            crate::ir::InstInner::Call { fn_name, args } => {
                write!(to, "Call @{}(", fn_name)?;
                for arg in args {
                    write_value(&mut to, arg)?;
                }
                write!(to, ")")?;
            },
            crate::ir::InstInner::Label(lab) => write!(to, "lab{}:", lab)?,
            crate::ir::InstInner::Jnz(lab, value) => {
                write!(to, "Jnz ")?;
                write_value(&mut to, value)?;
                write!(to, "lab{}:", lab)?;
            },
            crate::ir::InstInner::Jz(lab, value) => {
                write!(to, "Jz ")?;
                write_value(&mut to, value)?;
                write!(to, "lab{}", lab)?;
            },
            crate::ir::InstInner::Jmp(lab) => {
                write!(to, "Jmp lab{}", lab)?;
            },
            crate::ir::InstInner::Load(vreg) => {
                write!(to, "Load ")?;
                write_value(&mut to, vreg)?;
            }
            crate::ir::InstInner::Store(vreg, value) => {
                write!(to, "Store ")?;
                write_value(&mut to, vreg)?;
                write_value(&mut to, value)?;
            }
            crate::ir::InstInner::Copy(value) => {
                write!(to, "Copy ")?;
                write_value(&mut to, value)?;
            }
            crate::ir::InstInner::Alloca => {
                write!(to, "Alloca")?;
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
    }

    write!(to, ":{:?} ", val.type_)?;

    Ok(())
}
