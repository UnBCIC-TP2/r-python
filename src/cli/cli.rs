use std::collections::HashMap;
use std::env;
use std::fs;
use std::process;

use crate::interpreter::interpreter::execute;
use crate::interpreter::interpreter::EnvValue;
use crate::io;
use crate::ir::ast::Type;
use crate::parser::parser::*;
use crate::tc::type_checker::check_stmt;
use crate::tc::type_checker::ControlType;

use crate::interpreter::interpreter::ControlFlow;
use crate::ir::ast::{Environment, Expression, Statement};
pub fn print_env(env: &Environment<EnvValue>) {
    for (key, value) in env.stack.iter() {
        println!("{:?} = {:?}", key, value);
    }
}

pub fn cli(file_path: &String) -> io::Result<(Environment<EnvValue>, Environment<Type>)> {
    let file_content = fs::read_to_string(file_path).unwrap_or_else(|err| {
        eprintln!("Error reading file {}: {}", file_path, err);
        process::exit(1);
    });

    let (remaining_input, parsed_statements) = match parse(&file_content) {
        Ok((remaining, statements)) if remaining.trim().is_empty() => (remaining, statements),
        Ok((remaining, _)) => {
            eprintln!("Error: Unparsed input remains: {:?}", remaining);
            process::exit(1);
        }
        Err(err) => {
            eprintln!("Error parsing file content: {}", err);
            process::exit(1);
        }
    };

    let mut env: Environment<EnvValue> = Environment::new();
    let mut env_type: Environment<Type> = Environment::new();
    println!("Environment before execution:");
    print_env(&env);

    for stmt in parsed_statements {
        match check_stmt(stmt.clone(), &env_type) {
            Ok(ControlType::Continue(new_env)) => env_type = new_env,
            Ok(ControlType::Return(_)) => {
                eprintln!("Error during execution");
                process::exit(1);
            }
            Err(err) => {
                eprintln!("Error during execution: {:?}", err);
                process::exit(1);
            }
        }
        match execute(stmt, &env) {
            Ok(ControlFlow::Continue(new_env)) => {
                env = new_env;
            }
            Ok(ControlFlow::Return(value)) => {
                println!("Execution returned: {:?}", value);
                break;
            }
            Err(err) => {
                eprintln!("Error during execution: {:?}", err);
                process::exit(1);
            }
        }
    }

    println!("\nEnvironment after execution:");
    print_env(&env);
    Ok((env, env_type))
}
