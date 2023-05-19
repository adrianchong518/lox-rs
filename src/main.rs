mod ast;
mod interpreter;
mod object;
mod parser;
mod scanner;
mod token;

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

    eprintln!("Running prompt...");
    let mut interpreter = interpreter::Interpreter::new();

    let mut line = String::new();

    loop {
        print!("> ");
        io::stdout()
            .flush()
            .into_report()
            .change_context(LoxError)
            .attach_printable("unable to flush stdout")?;

        line.clear();
        let result = match io::stdin()
            .read_line(&mut line)
            .into_report()
            .change_context(LoxError)
            .attach_printable("unable to read line from stdin")?
        {
            0 => break,
            _ => run(&mut interpreter, &line, true),
        };

        if let Err(report) = result {
            eprintln!("{report:?}");
        }
    }

    Ok(())
}

fn run_file(filepath: impl AsRef<path::Path>) -> error_stack::Result<(), LoxError> {
    let filepath = filepath.as_ref();
    eprintln!("Running file {}...", filepath.display());
    let mut interpreter = interpreter::Interpreter::new();

    let source = fs::read_to_string(filepath)
        .into_report()
        .change_context(LoxError)
        .attach_printable_lazy(|| format!("unable to read file {filepath:?}"))?;

    run(&mut interpreter, &source, false)
}

fn run(
    interpreter: &mut interpreter::Interpreter,
    source: &str,
    allow_expr: bool,
) -> error_stack::Result<(), LoxError> {
    let (tokens, syntax_error) = scanner::scan(source);
    for token in &tokens {
        eprintln!("{token:?}");
    }

    let (found_expr, mut statements, parse_error) = parser::parse(tokens, allow_expr);
    for statement in &statements {
        eprintln!("{statement}")
    }

    match (
        syntax_error.map(|r| r.change_context(LoxError)),
        parse_error.map(|r| r.change_context(LoxError)),
    ) {
        (Some(mut s), Some(p)) => {
            s.extend_one(p);
            return Err(s);
        }
        (Some(s), None) => return Err(s),
        (None, Some(p)) => return Err(p),
        (None, None) => {}
    }

    if found_expr {
        let Some(ast::Stmt::Expression(last)) = statements.pop() else {
            return Err(error_stack::report!(LoxError)
                .attach_printable("last statement is not an expression, despite internal flag"));
        };

        interpreter
            .interpret(&statements)
            .change_context(LoxError)?;

        let value = interpreter
            .evaluate(&last.expression)
            .change_context(LoxError)?;
        println!("= {value}");
    } else {
        interpreter
            .interpret(&statements)
            .change_context(LoxError)?;
    }

    Ok(())
}
