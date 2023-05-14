use std::{env, path};

fn run_prompt() {
    println!("Running prompt...");
}

fn run_file(filepath: impl AsRef<path::Path>) {
    println!("Running file {}", filepath.as_ref().display());
}

fn run(source_code: &str) {}

fn main() {
    let args = env::args().collect::<Vec<_>>();

    match args[..] {
        // Run the REPL prompt if no file is provided
        [_] => run_prompt(),

        // Run the provided file
        [_, ref filepath] => run_file(filepath),

        // Malformed usage
        _ => eprintln!("Usage: lox-rs [script]"),
    }
}
