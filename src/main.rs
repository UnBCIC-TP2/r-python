use cli::cli::cli;
use repl::repl::execute_inline_command;
use std::{io, process};
mod cli;
mod interpreter;
mod ir;
mod parser;
mod repl;
mod tc;
use crate::repl::repl::repl;

fn main() -> io::Result<()> {
    // Get command-line arguments
    let args: Vec<String> = std::env::args().collect();

    if args.len() > 1 {
        match args[1].as_str() {
            "--help" | "-h" => handle_help_option(),
            "-c" => handle_command_option(&args)?,
            "--exec" => handle_exec_option(&args)?,
            _ => eprintln!("Invalid option: {}", args[1]),
        };
    } else {
        repl(None)?;
    }

    Ok(())
}

fn handle_help_option() {
    eprintln!("R-Python Help:");
    eprintln!("-c\t\tExecute inline command");
    eprintln!("--help\t-h\tPrint help about r-python");
    eprintln!("--exec file\tExecute r-python file");
    eprintln!("\t--exec Options:");
    eprintln!("\t\t-i\tOpen interative shell after execution");
}

fn handle_command_option(args: &[String]) -> io::Result<()> {
    let command = args[2..].join(" ");
    execute_inline_command(&command)
}

fn handle_exec_option(args: &[String]) -> io::Result<()> {
    if args.len() >= 3 && args[2].ends_with(".rpy") {
        if args[3..].contains(&"-i".to_owned()) {
            match cli(&args[2]) {
                Ok(env) => repl(Some(env))?,
                Err(e) => {
                    eprintln!("Error: {}", e);
                    process::exit(1);
                }
            };
        } else {
            cli(&args[2])?;
        }
    } else {
        eprintln!("Usage: {} {} <file_path>", args[0], args[1]);
        process::exit(1);
    };
    Ok(())
}
