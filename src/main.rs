mod parser;

use std::{fs::File, path::PathBuf};

use bpaf::Bpaf;

use crate::parser::get_imports;

#[derive(Clone, Debug, Bpaf)]
#[bpaf(options, version)]
/// Accept speed and distance, print them
struct Arguments {
    /// The start file for the program
    start_file: PathBuf,
}

fn main() {
    let opts = arguments().run();
    println!("Options: {:?}", opts);

    let mut file = std::fs::read_to_string(opts.start_file).unwrap();
    let imports = get_imports(&file)
        .into_iter()
        .filter(|imports| imports[0] != "std")
        .collect::<Vec<_>>()
        .clone();
    println!("Imports: {:?}", imports);
}
