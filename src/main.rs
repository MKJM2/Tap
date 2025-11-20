use tap::*;

use environment::Environment;
use interpreter::{Interpreter, Value};
use lexer::Lexer;
use parser::Parser;
use rustyline::Editor;
use rustyline::error::ReadlineError;
use std::cell::RefCell;
use std::env;
use std::fs;
use std::rc::Rc;

const VERSION: &str = "0.1.0";
const AUTHOR: &str = "Michał Kurek";

// ASCII logo banner
fn print_banner() {
    println!(
        r#"████████╗ █████╗ ██████╗
╚══██╔══╝██╔══██╗██╔══██╗
   ██║   ███████║██████╔╝
   ██║   ██╔══██║██╔═══╝
   ██║   ██║  ██║██║
   ╚═╝   ╚═╝  ╚═╝╚═╝

Tap Language REPL
Version: {version}
Author:  {author}
Type 'exit' to quit.
"#,
        version = VERSION,
        author = AUTHOR,
    );
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() > 2 {
        println!("Usage: tap [script]");
        return;
    }

    color_eyre::install().unwrap_or(eprintln!(
        "Failed to install color_eyre for enhanced error reporting."
    ));

    if args.len() == 2 {
        run_file(&args[1]);
    } else {
        run_prompt();
    }
}

fn run_file(path: &str) {
    let source = fs::read_to_string(path).expect("Failed to read file");
    run(&source);
}

fn run_prompt() {
    print_banner(); // ← print the logo once here

    let mut rl = Editor::<()>::new().unwrap();
    let interpreter = Interpreter::new();
    let env = Rc::new(RefCell::new(Environment::new()));

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                if line.trim() == "exit" {
                    break;
                }
                run_with_interpreter(&line, &interpreter, Rc::clone(&env));
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
}

fn run(source: &str) {
    let interpreter = Interpreter::new();
    let env = Rc::new(RefCell::new(Environment::new()));
    run_with_interpreter(source, &interpreter, env);
}

fn run_with_interpreter(source: &str, interpreter: &Interpreter, env: Rc<RefCell<Environment>>) {
    let tokens = match Lexer::new(source).tokenize() {
        Ok(tokens) => tokens,
        Err(err) => {
            eprintln!("Lexer Error: {}", err);
            return;
        }
    };

    let mut parser = Parser::new(&tokens, source);
    let program = match parser.parse_program() {
        Ok(program) => program,
        Err(err) => {
            eprintln!("Parser Error: {:?}", err);
            return;
        }
    };

    match interpreter.interpret(&program, env) {
        Ok(Some(value)) => {
            if value != Value::Null {
                println!("{}", value);
            }
        }
        Ok(None) => { /* no value to print */ }
        Err(err) => {
            eprintln!("Runtime Error: {}", err);
        }
    }
}
