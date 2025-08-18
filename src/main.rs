use std::env;

pub mod grammar;
pub mod tokenizer;
pub mod ir;
pub mod utils;
pub mod error;
pub mod types;
pub mod backends;

fn main() {
    if env::args().len() == 2 {
        let filename = env::args().nth(1).unwrap();
        let src = std::fs::read_to_string(filename).unwrap();

        let tokens = tokenizer::tokenize(&src);
        println!("{:?}", tokens);

        let ast = grammar::parse(&tokens);

        if let Err(e) = ast {
            e.to_comp_err().print(&src);
            return;
        }

        let ast = ast.unwrap();

        println!("{:?}", ast);

        let snippet = ir::generate(ast, &mut ir::GenContext::new());

        if let Err(e) = snippet {
            e.print(&src);
            return;
        }

        let snippet = snippet.unwrap();

        snippet.debug_print();

        // backends::codegen(snippet, "bleh.asm");
    }
}
