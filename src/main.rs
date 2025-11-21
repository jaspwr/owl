use std::env;

use crate::ir::backpatch_types;

pub mod backends;
pub mod data;
pub mod error;
pub mod grammar;
pub mod ir;
pub mod reg_alloc;
pub mod tokenizer;
pub mod types;
pub mod utils;

fn main() {
    if env::args().len() == 2 {
        let filename = env::args().nth(1).unwrap();
        let src = std::fs::read_to_string(filename).unwrap();

        let tokens = tokenizer::tokenize(&src);
        println!("{:?}", tokens);

        let pr = grammar::parse(&tokens);

        if let Err(e) = pr {
            e.to_comp_err().print(&src);
            return;
        }

        let pr = pr.unwrap();

        println!("{:?}", pr.ast);

        let mut ctx = ir::GenContext::new();
        let snippet = ir::generate(pr.ast, &mut ctx, false);

        if let Err(e) = snippet {
            e.print(&src);
            return;
        }

        let mut snippet = snippet.unwrap();

        backpatch_types(&mut ctx, &mut snippet);
        backpatch_types(&mut ctx, &mut snippet);

        let data = data::Data {
            string_literals: pr.string_literals,
        };

        snippet.debug_print();

        let obj = "bleh.o";

        backends::codegen(snippet, data, obj);

        let mut proc = std::process::Command::new("gcc").arg(obj).spawn().unwrap();

        let _ = proc.wait().unwrap();

        std::fs::remove_file(obj).unwrap();
    }
}
