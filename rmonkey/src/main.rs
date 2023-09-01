use std::io;

mod ast;
mod lexer;
mod object;
mod parser;
mod evaluator;
mod repl;
mod token;

fn main() -> io::Result<()> {
    let user = std::env::var("USER").unwrap_or_else(|_| "anonymous".to_string());
    println!(
        "Hello {}! This is the Monkey programming language (in Rust)",
        user
    );
    println!("Feel free to type in commands");
    repl::start(std::io::stdin(), std::io::stdout())?;
    Ok(())
}
