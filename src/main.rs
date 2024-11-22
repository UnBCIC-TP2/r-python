//use crate::ir::ast::Expression;
//use crate::ir::ast::Statement;
//use crate::interpreter::interpreter::eval;
use crate::interpreter::interpreter::execute;
use crate::parser::parser::parse;
use std::collections::HashMap;

pub mod interpreter;
pub mod ir;
pub mod parser;
pub mod tc;


fn run_test(name: &str, program: &str) {
    println!("\n=== Running test: {} ===", name);
    println!("Program:\n{}\n", program);

    match parse(program) {
        Ok((remaining, statements)) => {
            println!("Parsed AST: {:#?}\n", statements);

            if !remaining.is_empty() {
                println!("Warning: Unparsed input remains: {:?}\n", remaining);
                return;
            }

            let mut current_env = HashMap::new();
            for stmt in statements {
                match execute(&stmt, current_env) {
                    Ok(new_env) => {
                        println!("Environment after execution: {:?}", new_env);
                        current_env = new_env;
                    }
                    Err(e) => {
                        println!("Execution error: {}", e);
                        return;
                    }
                }
            }
            println!("\nFinal environment: {:?}", current_env);
        }
        Err(e) => println!("Parse error: {:?}", e),
    }
}

//raw string literal (r#""#) to avoid escaping newlines
fn main() {
    // Test 1: Basic if-else
    let test1 = r#"x = 10
if x > 5:
    y = 1
else:
    y = 2"#;
    run_test("Basic if-else", test1);

    // Test 2: Arithmetic operations
    let test2 = r#"x = 5
y = 3
z = (x * y) + (10 - 4)"#;
    run_test("Arithmetic operations", test2);

    // Test 3: Nested if statements
    let test3 = r#"x = 10
if x > 5:
    if x > 8:
        y = 1
    else:
        y = 2
else:
    y = 3"#;
    run_test("Nested if statements", test3);

    // Test 4: Multiple assignments
    let test4 = r#"x = 42
y = x
z = y + 10"#;
    run_test("Multiple assignments", test4);

    // Test 5: Complex arithmetic
    let test5 = r#"a = 5
b = 3
c = (a * b) + (10 / 2)
d = c - (b * 2)"#;
    run_test("Complex arithmetic", test5);
}
