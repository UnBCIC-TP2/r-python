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
                match execute(stmt, current_env) {
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
    // Basic Operations Tests
    let test1 = r#"x = 10
if x > 5:
    y = 1
else:
    y = 2"#;
    run_test("1. Basic if-else", test1);

    // Arithmetic and Parentheses Tests
    let test2 = r#"x = 5
y = 3
z = (x * y) + (10 - 4)
w = z / (y + 1)"#;
    run_test("2. Arithmetic operations", test2);

    // Nested Control Flow Tests
    let test3 = r#"x = 10
if x > 5:
    if x > 8:
        y = 1
        z = y + x
    else:
        y = 2
        z = y * x
else:
    y = 3
    z = y - x"#;
    run_test("3. Nested if statements with multiple operations", test3);

    // Variable Reference Tests
    let test4 = r#"x = 42
y = x
z = y + 10
w = z
final = w * 2"#;
    run_test("4. Multiple assignments and references", test4);

    // Complex Expressions Tests
    let test5 = r#"a = 5
b = 3
c = (a * b) + (10 / 2)
d = c - (b * 2)
e = (d + a) * (b - 1)"#;
    run_test("5. Complex arithmetic expressions", test5);

    // Comparison Chain Tests
    let test6 = r#"x = 10
y = 5
if x > y:
    if y > 3:
        if x > 8:
            z = 1
        else:
            z = 2
    else:
        z = 3
else:
    z = 4"#;
    run_test("6. Multiple nested comparisons", test6);

    // Mixed Operations Tests
    let test7 = r#"a = 15
b = 3
if a > 10:
    c = a + b
    d = c * 2
    if d > 30:
        e = d - 10
    else:
        e = d + 5
else:
    c = a - b
    d = c / 2
    e = d * 3"#;
    run_test("7. Mixed arithmetic and control flow", test7);
}
