use clap::{Parser as CLAParser, Subcommand};
use nu_ansi_term::Color;
use reedline::{FileBackedHistory, Reedline, Signal};
use std::fs;
use std::path::PathBuf;
use tap::{
    diagnostics::Reporter, interpreter::Interpreter, lexer::Lexer, parser::Parser, prompt::Prompt,
};

#[derive(CLAParser)]
#[command(name = "tap")]
#[command(about = "Tap Programming Language", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,

    /// Source file to execute
    file: Option<PathBuf>,

    /// Arguments to pass to the program
    #[arg(trailing_var_arg = true)]
    args: Vec<String>,
}

#[derive(Subcommand)]
enum Commands {
    /// Start an interactive REPL
    Repl,

    /// Run a Tap source file
    Run {
        /// Path to the source file
        file: PathBuf,

        /// Arguments to pass to the program
        #[arg(trailing_var_arg = true)]
        args: Vec<String>,
    },
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Some(Commands::Repl) => run_repl(),
        Some(Commands::Run { file, args }) => {
            if let Err(e) = run_file(&file, args) {
                eprintln!("{}", Color::Red.paint(format!("Error: {}", e)));
                std::process::exit(1);
            }
        }
        None => {
            if let Some(file) = cli.file {
                // Execute file directly
                if let Err(e) = run_file(&file, cli.args) {
                    eprintln!("{}", Color::Red.paint(format!("Error: {}", e)));
                    std::process::exit(1);
                }
            } else {
                // No file provided, start REPL
                run_repl();
            }
        }
    }
}

fn run_repl() {
    println!("{}", Color::Cyan.bold().paint("Tap REPL v0.1.0"));
    println!(
        "Type {} for help, {} to exit\n",
        Color::Yellow.paint(".help"),
        Color::Yellow.paint(".exit")
    );

    let history = Box::new(
        FileBackedHistory::with_file(100, "tap_history.txt".into())
            .expect("Failed to create history file"),
    );

    let mut line_editor = Reedline::create().with_history(history);

    let mut interpreter = Interpreter::new();
    let mut line_num = 1;

    loop {
        let sig = line_editor.read_line(&Prompt);

        match sig {
            Ok(Signal::Success(buffer)) => {
                let input = buffer.trim();

                // Handle REPL commands
                if input.starts_with('.') {
                    match input {
                        ".help" => print_help(),
                        ".exit" | ".quit" => {
                            println!("{}", Color::Green.paint("Goodbye!"));
                            break;
                        }
                        ".clear" => {
                            print!("\x1B[2J\x1B[1;1H");
                            continue;
                        }
                        ".reset" => {
                            interpreter = Interpreter::new();
                            println!("{}", Color::Green.paint("Interpreter state reset"));
                            continue;
                        }
                        _ => {
                            eprintln!(
                                "{}",
                                Color::Red.paint(format!("Unknown command: {}", input))
                            );
                            continue;
                        }
                    }
                    continue;
                }

                if input.is_empty() {
                    continue;
                }

                // Execute the input
                match execute_repl_line(&mut interpreter, input, line_num) {
                    Ok(Some(result)) => {
                        println!(
                            "{} {}",
                            Color::Green.paint("=>"),
                            Color::White.bold().paint(result)
                        );
                    }
                    Ok(None) => {}
                    Err(e) => {
                        eprintln!("{}", Color::Red.paint(e));
                    }
                }

                line_num += 1;
            }
            Ok(Signal::CtrlD) | Ok(Signal::CtrlC) => {
                println!("\n{}", Color::Green.paint("Goodbye!"));
                break;
            }
            Err(err) => {
                eprintln!("{}", Color::Red.paint(format!("Error: {}", err)));
                break;
            }
        }
    }
}

fn execute_repl_line(
    interpreter: &mut Interpreter,
    input: &str,
    line_num: usize,
) -> Result<Option<String>, String> {
    let mut reporter = Reporter::new();

    // Lex
    let lexer = Lexer::new(input, &mut reporter);
    let tokens = match lexer.tokenize() {
        Ok(tokens) => tokens,
        Err(e) => {
            return Err(format!("Lexer error: {:?}", e));
        }
    };

    if reporter.has_errors() {
        return Err(format!(
            "Lexer errors:\n{}",
            reporter.format_diagnostics(input)
        ));
    }

    // Parse
    let mut parser = Parser::new(&tokens, &mut reporter);
    let program = match parser.parse_program() {
        Ok(prog) => prog,
        Err(e) => {
            if reporter.has_errors() {
                return Err(format!(
                    "Parse errors:\n{}",
                    reporter.format_diagnostics(input)
                ));
            }
            return Err(format!("Parse error: {}", e.message));
        }
    };

    if reporter.has_errors() {
        return Err(format!(
            "Parse errors:\n{}",
            reporter.format_diagnostics(input)
        ));
    }

    // Optionally, type check if `--type-check` command line was specified
    todo!("Type check");

    // Interpret
    match interpreter.interpret(&program) {
        Ok(Some(value)) => {
            // let display = interpreter_value_to_string(&value);
            let display = interpreter.value_to_display_string(&value);
            Ok(Some(display))
        }
        Ok(None) => Ok(None),
        Err(e) => Err(format!("Runtime error at line {}: {}", line_num, e)),
    }
}

fn run_file(path: &PathBuf, args: Vec<String>) -> Result<(), String> {
    let source = fs::read_to_string(path)
        .map_err(|e| format!("Failed to read file '{}': {}", path.display(), e))?;

    let mut reporter = Reporter::new();

    // Lex
    let lexer = Lexer::new(&source, &mut reporter);
    let tokens = match lexer.tokenize() {
        Ok(tokens) => tokens,
        Err(e) => {
            return Err(format!("Lexer error: {:?}", e));
        }
    };

    if reporter.has_errors() {
        return Err(format!(
            "Lexer errors:\n{}",
            reporter.format_diagnostics(&source)
        ));
    }

    // Parse
    let mut parser = Parser::new(&tokens, &mut reporter);
    let program = match parser.parse_program() {
        Ok(prog) => prog,
        Err(e) => {
            if reporter.has_errors() {
                return Err(format!(
                    "Parse errors:\n{}",
                    reporter.format_diagnostics(&source)
                ));
            }
            return Err(format!("Parse error: {}", e.message));
        }
    };

    if reporter.has_errors() {
        return Err(format!(
            "Parse errors:\n{}",
            reporter.format_diagnostics(&source)
        ));
    }

    // Prepare arguments
    let mut full_args = vec![path.to_string_lossy().to_string()];
    full_args.extend(args);

    // Interpret
    let mut interpreter = Interpreter::new_with_args(full_args);

    match interpreter.interpret(&program) {
        Ok(_) => Ok(()),
        Err(e) => Err(format!("Runtime error: {}", e)),
    }
}

// fn interpreter_value_to_string(value: &tap::interpreter::Value) -> String {
//     use tap::interpreter::Value;

//     match value {
//         Value::Integer(i) => i.to_string(),
//         Value::Float(f) => f.to_string(),
//         Value::String(s) => format!("\"{}\"", s),
//         Value::Boolean(b) => b.to_string(),
//         Value::Unit => "()".to_string(),
//         Value::List(items) => {
//             let items_str: Vec<String> = items.iter().map(interpreter_value_to_string).collect();
//             format!("[{}]", items_str.join(", "))
//         }
//         Value::Record(fields) => {
//             let fields_str: Vec<String> = fields
//                 .iter()
//                 .map(|(k, v)| format!("{}: {}", k, interpreter_value_to_string(v)))
//                 .collect();
//             format!("{{{}}}", fields_str.join(", "))
//         }
//         Value::Function { name, .. } => {
//             format!(
//                 "<function {}>",
//                 name.as_ref().unwrap_or(&"anonymous".to_string())
//             )
//         }
//         Value::File { path, .. } => format!("<file '{}'>", path),
//         Value::Args { .. } => "<args>".to_string(),
//         Value::Variant { name, data } => {
//             if let Some(d) = data {
//                 format!("{}({})", name, interpreter_value_to_string(d))
//             } else {
//                 name.clone()
//             }
//         }
//         _ => format!("{:?}", value),
//     }
// }

fn print_help() {
    println!("{}", Color::Cyan.bold().paint("Tap REPL Commands:"));
    println!(
        "  {}  - Show this help message",
        Color::Yellow.paint(".help")
    );
    println!("  {}  - Exit the REPL", Color::Yellow.paint(".exit"));
    println!("  {}  - Clear the screen", Color::Yellow.paint(".clear"));
    println!(
        "  {}  - Reset interpreter state",
        Color::Yellow.paint(".reset")
    );
    println!();
    println!("{}", Color::Cyan.bold().paint("Tips:"));
    println!(
        "  - Press {} or {} to exit",
        Color::Yellow.paint("Ctrl+D"),
        Color::Yellow.paint("Ctrl+C")
    );
    println!(
        "  - Use {} and {} to navigate history",
        Color::Yellow.paint("↑"),
        Color::Yellow.paint("↓")
    );
    println!(
        "  - History is saved to {}",
        Color::Green.paint("tap_history.txt")
    );
}
