use tap_lang::*;

use environment::Environment;
use interpreter::{Interpreter, Value};
use lexer::Lexer;
use parser::Parser;
use rustyline::Editor;
use rustyline::error::ReadlineError;
use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() > 2 {
        println!("Usage: tap-lang [script]");
        return;
    }

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
    let mut rl = Editor::<()>::new().unwrap();
    let interpreter = Interpreter::new();
    let mut env = Environment::new();

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                if line.trim() == "exit" {
                    break;
                }
                run_with_interpreter(&line, &interpreter, &mut env);
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
    let mut env = Environment::new();
    run_with_interpreter(source, &interpreter, &mut env);
}

fn run_with_interpreter(source: &str, interpreter: &Interpreter, env: &mut Environment) {
    let tokens = match Lexer::new(source).tokenize() {
        Ok(tokens) => tokens,
        Err(err) => {
            eprintln!("Lexer Error: {}", err);
            return;
        }
    };

    let mut parser = Parser::new(&tokens);
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
        Ok(None) => {
            // Do nothing if no value is returned (e.g., a statement that doesn't produce a value)
        }
        Err(err) => {
            eprintln!("Runtime Error: {:?}", err);
        }
    }
}