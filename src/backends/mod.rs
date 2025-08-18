use crate::ir::IrSnippet;

mod llvm;
mod interpreter;

pub fn codegen(ir: IrSnippet, output: impl AsRef<std::path::Path>) {
    interpreter::exec(ir)
    // llvm::codegen(ir);
}
