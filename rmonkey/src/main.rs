use std::{env, fs};

use lexer::Lexer;
use parser::Parser;

mod ast;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod repl;
mod token;

fn main() -> anyhow::Result<()> {
    // Check if there's some argument
    let args: Vec<String> = env::args().skip(1).collect();

    match &args[..] {
        [] => run_repl(),
        [path] => run_file(path),
        _ => {
            print_usage();
            Ok(())
        }
    }
}

fn run_repl() -> anyhow::Result<()> {
    let user = std::env::var("USER").unwrap_or_else(|_| "anonymous".to_string());
    println!(
        "Hello {}! This is the Monkey programming language (in Rust)",
        user
    );
    println!("Feel free to type in commands");
    repl::start(std::io::stdin(), std::io::stdout())?;
    Ok(())
}

fn run_file(path: &str) -> anyhow::Result<()> {
    let contents = fs::read_to_string(path)?;
    // Actually run the code
    let lexer = Lexer::new(contents);
    let mut parser = Parser::new(lexer);
    let program = parser.parse();
    if !parser.errors.is_empty() {
        repl::print_parser_errors(&mut std::io::stdout(), &parser);
        anyhow::bail!("found parse errors");
    }

    let _ = evaluator::eval_in_new_env(&program);
    Ok(())
}

fn print_usage() {
    println!("usage: rmonkey [file]")
}
