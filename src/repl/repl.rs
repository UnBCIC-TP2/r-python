use crate::interpreter::interpreter::eval;
use crate::interpreter::interpreter::execute;
use crate::interpreter::interpreter::execute_block;

use crate::interpreter::interpreter::ControlFlow;
use crate::interpreter::interpreter::EnvValue;
use crate::tc::type_checker::check_stmt;
use crate::tc::type_checker::ControlType;
use crate::ir::ast::Statement;
use crate::ir::ast::Environment;
use crate::ir::ast::Expression;
use crate::ir::ast::Type;
use crate::parser::parser::*;
use std::io::{self, Write};
use std::process::Command;

pub fn repl(env: Option<Environment<EnvValue>>, env_type: Option<Environment<Type>>) -> io::Result<()> {
    // Print welcome message
    println!("R-Python REPL");
    println!("Type !help' for more commands or '!exit' to quit'\n");
    let mut current_env = env.unwrap_or(Environment::new());
    let mut current_env_type = env_type.unwrap_or(Environment::new());
    loop {
        // Display prompt
        print!("R-Python >>> ");
        io::stdout().flush()?;

        // Read input
        let mut input = String::new();
        // Break condition
        loop {
            io::stdin().read_line(&mut input)?;
            if input.trim().ends_with(";") {
                input = input.trim().to_string();
                input.pop(); // Remove the semicolon
                break;
            }
        }

        // Handle inline commands (prefixed with '!')
        if input.starts_with('!') {
            // Handle exit condition
            if input == "!exit" {
                break;
            } else if input == "!reset" {
                current_env = Environment::new();
                continue;
            } else {
                handle_inline_command(&input, &mut current_env)?;
                continue;
            }
        }

        // If just enter or spaces, continue the loop
        if input.is_empty() {
            continue;
        }

        // Reset the output
        let mut output: Result<String, String> = Ok(String::new());

        // Parsing of expressions
        match expression(&input) {
            Ok(("", _expr)) => {
                // Evaluate the expression
                output = repl_parse_expression(&input, &current_env);
            }
            _ => {
                // Try to parse statements in the input
                match repl_parse_statements(&input, current_env.clone(), current_env_type.clone()) {
                    Ok((new_env, new_env_type)) => {
                        current_env = new_env;
                        current_env_type = new_env_type;
                    }
                    Err(e) => output = Err(e),
                };
            }
        }

        // Print the output
        match output {
            Ok(result) => {
                if !result.is_empty() {
                    println!("{}", result);
                }
            }
            Err(e) => println!("Syntax Error: {}", e),
        }
    }
    Ok(())
}

/// Handles REPL commands (prefixed with '!')
fn handle_inline_command(input: &str, env: &mut Environment<EnvValue>) -> io::Result<()> {
    let command = input.trim_start_matches('!').trim();
    match command {
        "clear" => {
            if cfg!(target_os = "windows") {
                Command::new("cmd").arg("/c").arg("cls").status()?;
            } else {
                Command::new("clear").status()?;
            }
        }
        "help" => {
            println!("\nAvailable commands:");
            println!("  !help  - Shows available commands");
            println!("  !exit - Quits the program");
            println!("  !clear - Clears the terminal screen");
            println!("  !reset - Resets the context\n");
        }
        _ => {
            println!("Unknown command: {}", command);
            println!("Type '!help' for a list of available commands.");
        }
    }
    Ok(())
}

fn repl_parse_expression(input: &str, current_env: &Environment<EnvValue>) -> Result<String, String> {
    // Parse the input as an expression
    match expression(input) {
        
        Ok(("", expr)) => {
            // Evaluate the expression
            match eval(expr, &current_env.clone()) {
                Ok(evaluated_expression) => Ok(format_env_value(&evaluated_expression)),
                Err(e) => Err(format!("Evaluation Error: {:?}", e)),
            }
        }
        Ok((_, _)) => Err(format!("Parsing Expression Error")),
        Err(_) => Err(format!("Parsing Expression Error")),
    }
}

fn repl_parse_statements(input: &str, mut current_env: Environment<EnvValue>, mut current_env_type: Environment<Type>) -> Result<(Environment<EnvValue>, Environment<Type>), String> {
    // Parse the input as a statement
    match parse(input) {
        Ok((remaining, statements)) => {
            if !remaining.is_empty() {
                return Err(format!("Warning: Unparsed input remains: {:?}\n", remaining));
            }

            let stmt = Statement::Block(statements.clone());
            match check_stmt(stmt, &current_env_type.clone()){
                Ok(ControlType::Continue(new_env)) => current_env_type = new_env,
                Ok(ControlType::Return(_)) => {
                    return Err(format!("Execution Error: Return value not in a Function"))
                }
                Err(e) => return Err(format!("Execution Error: {:?}", e)),
            }

            match execute_block(statements, &current_env.clone()) {
                Ok(ControlFlow::Continue(new_env)) => current_env = new_env,
                Ok(ControlFlow::Return(_)) => {
                    return Err(format!("Execution Error: Return value not in a Function"))
                }
                Err(e) => return Err(format!("Execution Error: {:?}", e)),
            }
            println!("Contexto de valores atual: {:?}", current_env);
            println!("Contexto de tipos atual: {:?}", current_env_type);
            Ok((current_env.clone(), current_env_type.clone()))
        }
        Err(e) => Err(format!("Statement Parse Error: {}", e)),
    }
}



// For "-c" inline commands
pub fn execute_inline_command(command: &str) -> io::Result<()> {
    let env = Environment::new();
    
    // First try to parse as an expression
    let output = match expression(command) {
        Ok(("", expr)) => evaluate_expression(expr, &env),
        _ => parse_and_execute_statements(command, &env),
    };

    handle_execution_output(output)
}

fn evaluate_expression(expr: Expression, env: &Environment<EnvValue>) -> Result<String, String> {
    eval(expr, env)
        .map(|value| format_env_value(&value))
        .map_err(|e| format!("Evaluation Error: {:?}", e))
}

fn parse_and_execute_statements(command: &str, env: &Environment<EnvValue>) -> Result<String, String> {
    match parse(command) {
        Ok(("", statements)) => {
            execute_block(statements, env)
                .map(|control_flow| match control_flow {
                    ControlFlow::Return(value) => format_env_value(&value),
                    _ => String::new(),
                })
                .map_err(|e| format!("Execution Error: {:?}", e))
        }
        Ok((remaining, _)) => Err(format!("Unparsed input: {:?}", remaining)),
        Err(_) => Err("Parse Error: Invalid syntax".to_string()),
    }
}

fn handle_execution_output(output: Result<String, String>) -> io::Result<()> {
    match output {
        Ok(msg) if !msg.is_empty() => {
            println!("{}", msg);
            Ok(())
        }
        Ok(_) => Ok(()),
        Err(e) => {
            eprintln!("{}", e);
            Ok(())
        }
    }
}


// For formatting values
fn format_env_value(value: &EnvValue) -> String {
    let EnvValue::Exp(expr) = value else {todo!()};
    format!("{}", expr)
}

mod tests {

    use super::*;
    use crate::ir::ast::Expression::*;
    use crate::ir::ast::Type::*;

    #[test]
    fn test_simple_repl_parse_expression1() {
        let input = "10 + 10";
        let env = Environment::new();
        let output = repl_parse_expression(input, &env);
        match output {
            Ok(result) => assert_eq!("20", result),
            Err(e) => panic!("Error: {}", e),
        }
    }

    #[test]
    fn test_simple_repl_parse_expression2() {
        let input = "(10+100) * (500/100)";
        let env = Environment::new();
        let output = repl_parse_expression(input, &env);
        match output {
            Ok(result) => assert_eq!("550", result),
            Err(e) => panic!("Error: {}", e),
        }
    }

    #[test]
    fn test_simple_repl_parse_expression3() {
        let input = "10-100";
        let env = Environment::new();
        let output = repl_parse_expression(input, &env);
        match output {
            Ok(result) => assert_eq!("-90", result),
            Err(e) => panic!("Error: {}", e),
        }
    }

    #[test]
    fn test_simple_repl_parse_expression4() {
        let input = "90 -                                                           (100)";
        let env = Environment::new();
        let output = repl_parse_expression(input, &env);
        match output {
            Ok(result) => assert_eq!("-10", result),
            Err(e) => panic!("Error: {}", e),
        }
    }

    #[test]
    fn test_simple_repl_parse_expression5() {
        let input = "(-90)*(-20) - (100)";
        let env = Environment::new();
        let output = repl_parse_expression(input, &env);
        match output {
            Ok(result) => assert_eq!("1700", result),
            Err(e) => panic!("Error: {}", e),
        }
    }

    #[test]
    fn test_simple_repl_parse_expression6() {
        let input = "- 90*(- 20) - (200)";
        let env = Environment::new();
        let output = repl_parse_expression(input, &env);
        match output {
            Ok(result) => assert_eq!("1600", result),
            Err(e) => panic!("Error: {}", e),
        }
    }

    #[test]
    fn test_sad_path_repl_parse_expression1() {
        let input = "a + b";
        let env = Environment::new();
        let output = repl_parse_expression(input, &env);
        match output {
            Ok(_) => panic!("Error was expected"),
            Err(e) => assert_eq!("Evaluation Error: Variable a not found", e),
        }
    }

    #[test]
    fn test_sad_path_repl_parse_expression2() {
        let input = "exit";
        let env = Environment::new();
        let output = repl_parse_expression(input, &env);
        match output {
            Ok(_) => panic!("Error was expected"),
            Err(e) => assert_eq!("Evaluation Error: Variable exit not found", e),
        }
    }

    #[test]
    fn test_happy_path_repl_parse_expression1() {
        let input = "a + b";
        let mut env = Environment::new();
        env.insert_variable(String::from("a"), (Some(EnvValue::Exp(CReal(10.5))), TReal));
        env.insert_variable(String::from("b"), (Some(EnvValue::Exp(CReal(20.1))), TReal));
        let output = repl_parse_expression(input, &env);
        match output {
            Ok(result) => assert_eq!("30.6", result),
            Err(e) => panic!("Error: {}", e),
        }
    }

    #[test]
    fn test_happy_path_repl_parse_expression2() {
        let input = "a + b";
        let mut env = Environment::new();
        env.insert_variable(String::from("a"), (Some(EnvValue::Exp(CInt(10))), TInteger));
        env.insert_variable(String::from("b"), (Some(EnvValue::Exp(CInt(20))), TInteger));
        let output = repl_parse_expression(input, &env);
        match output {
            Ok(result) => assert_eq!("30", result),
            Err(e) => panic!("Error: {}", e),
        }
    }

    #[test]
    fn test_happy_path_repl_parse_expression3() {
        let input = "a * 500";
        let mut env = Environment::new();
        env.insert_variable(String::from("a"), (Some(EnvValue::Exp(CReal(10.0))), TReal));
        let output = repl_parse_expression(input, &env);
        match output {
            Ok(result) => assert_eq!("5000", result),
            Err(e) => panic!("Error: {}", e),
        }
    }

    #[test]
    fn test_typechecker_sad_path_repl_parse_expression() {
        let input = "a + b";
        let mut env = Environment::new();
        env.insert_variable(String::from("a"), (Some(EnvValue::Exp(CTrue)), TBool));
        env.insert_variable(String::from("b"), (Some(EnvValue::Exp(CInt(20))), TInteger));
        let output = repl_parse_expression(input, &env);
        match output {
            Ok(_) => panic!("Error was expected"),
            Err(e) => assert_eq!(
                "Evaluation Error: addition '(+)' is only defined for numbers (integers and real).",
                e
            ),
        }
    }

    #[test]
    fn test_negative_first_argument() {
        let input = "-10-100";
        let env = Environment::new();
        let output = repl_parse_expression(input, &env);
        match output {
            Ok(result) => assert_eq!("-110", result),
            Err(e) => panic!("Error: {}", e),
        }
    }

    #[test]
    fn test_repl_parse_assigment1() {
        let input = "a = 10";
        let env = Environment::new();
        let env_output = repl_parse_statements(input, env);
        match env_output {
            Ok(new_env) => assert_eq!(
                new_env.get_variable("a"),
                Some(&(Some(EnvValue::Exp(CInt(10))), TInteger))
            ),
            Err(_) => panic!("New enviroment was expected"),
        }
    }

    #[test]
    fn test_repl_parse_assigment2() {
        let input = "a = 10 + 30";
        let env = Environment::new();
        let env_output = repl_parse_statements(input, env);
        match env_output {
            Ok(new_env) => assert_eq!(
                new_env.get_variable("a"),
                Some(&(Some(EnvValue::Exp(CInt(40))), TInteger))
            ),
            Err(_) => panic!("New enviroment was expected"),
        }
    }

    #[test]
    fn test_repl_parse_assigment4() {
        let input = "a = 10 > 10";
        let env = Environment::new();
        let mut env_expected = Environment::new();
        env_expected.insert_variable(
            String::from("a"),
            ((Some(EnvValue::Exp(Expression::CFalse))), TBool),
        );
        let env_output = repl_parse_statements(input, env);
        match env_output {
            Ok(new_env) => assert_eq!(new_env, env_expected),
            Err(_) => panic!("New enviroment was expected"),
        }
    }

    #[test]
    fn test_repl_parse_assigment5() {
        let input = "a = 10 == 10";
        let env = Environment::new();
        let mut env_expected = Environment::new();
        env_expected.insert_variable(
            String::from("a"),
            ((Some(EnvValue::Exp(Expression::CTrue))), TBool),
        );
        let env_output = repl_parse_statements(input, env);
        match env_output {
            Ok(new_env) => assert_eq!(new_env, env_expected),
            Err(_) => panic!("New enviroment was expected"),
        }
    }

    #[test]
    fn test_complex_repl_parse_assigment1() {
        // R-Python >> a = 10
        // R-Python >> b = a

        let input = "a = 10";
        let mut env = Environment::new();
        let mut env_expected = Environment::new();
        env_expected.insert_variable(
            String::from("a"),
            ((Some(EnvValue::Exp(Expression::CInt(10)))), TInteger),
        );
        match repl_parse_statements(input, env) {
            Ok(new_env) => {
                assert_eq!(new_env, env_expected);
                env = new_env;
            }
            Err(_) => panic!("New enviroment was expected"),
        }

        let input = "b = a";
        env_expected.insert_variable(
            String::from("b"),
            ((Some(EnvValue::Exp(Expression::CInt(10)))), TInteger),
        );
        let result = repl_parse_statements(input, env);

        match result {
            Ok(new_env) => assert_eq!(new_env, env_expected),
            Err(_) => panic!("New enviroment was expected"),
        }
    }
}