mod scanner;

use std::{env, fmt, fs, io, path};

use error_stack::{IntoReport as _, ResultExt as _};

#[derive(Debug)]
struct LoxError;

impl fmt::Display for LoxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Lox Interpreter")
    }
}

impl error_stack::Context for LoxError {}

fn main() -> error_stack::Result<(), LoxError> {
    let args = env::args().collect::<Vec<_>>();

    match args[..] {
        // Run the REPL prompt if no file is provided
        [_] => run_prompt()?,

        // Run the provided file
        [_, ref filepath] => run_file(filepath)?,

        // Malformed usage
        _ => eprintln!("Usage: lox-rs [script]"),
    }

    Ok(())
}

fn run_prompt() -> error_stack::Result<(), LoxError> {
    use io::Write as _;

    println!("Running prompt...");

    let mut line = String::new();

    loop {
        print!("> ");
        io::stdout()
            .flush()
            .into_report()
            .change_context(LoxError)
            .attach_printable("unable to flush stdout")?;

        line.clear();
        match io::stdin()
            .read_line(&mut line)
            .into_report()
            .change_context(LoxError)
            .attach_printable("unable to read line from stdin")?
        {
            0 => break,
            _ => run(&line)?,
        }
    }

    Ok(())
}

fn run_file(filepath: impl AsRef<path::Path>) -> error_stack::Result<(), LoxError> {
    let filepath = filepath.as_ref();
    println!("Running file {}...", filepath.display());
    let source = fs::read_to_string(filepath)
        .into_report()
        .change_context(LoxError)
        .attach_printable_lazy(|| format!("unable to read file {filepath:?}"))?;

    run(&source)
}

fn run(source: &str) -> error_stack::Result<(), LoxError> {
    for token in scanner::Scanner::new(source) {
        println!("{token:?}");
    }

    Ok(())
}
