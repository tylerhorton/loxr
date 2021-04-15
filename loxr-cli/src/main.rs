use anyhow::{anyhow, Result};
use loxr_lib::{lexer, parser};
use std::{env, fs, io, io::prelude::*};

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() > 2 {
        Err(anyhow!("Usage: loxr [script]"))
    } else if args.len() == 2 {
        run_file(&args[1])
    } else {
        run_prompt()
    }
}

fn run_file(filepath: &str) -> Result<()> {
    let contents = fs::read_to_string(filepath)?;
    run(contents)
}

fn run_prompt() -> Result<()> {
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let mut line = String::new();
        io::stdin().read_line(&mut line)?;

        // If there was an error in the line, print it out and continue
        if let Err(e) = run(line) {
            println!("{}", e);
        }
    }
}

fn run(source: String) -> Result<()> {
    println!("Lexing...\n");

    let (tokens, lex_errors) = lexer::lex(source.as_str());

    println!("{:?}\n", tokens);
    if lex_errors.len() > 0 {
        println!("{:?}\n", lex_errors);
    }

    println!("Parsing...\n");

    let (expr, parse_errors) = parser::parse(tokens.as_slice());

    println!("{:?}\n", expr);
    if parse_errors.len() > 0 {
        println!("{:?}", parse_errors);
    }

    Ok(())
}
