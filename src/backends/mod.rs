use crate::{data::Data, ir::IrSnippet};

mod llvm;
mod interpreter;

pub fn codegen(ir: IrSnippet, data: Data, path: impl AsRef<std::path::Path>) {
    // interpreter::exec(ir)
    llvm::codegen(ir, data, path);
}
