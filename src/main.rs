//use crate::ir::ast::Expression;
//use crate::ir::ast::Statement;
//use crate::interpreter::interpreter::eval;

/*
use crate::interpreter::interpreter::{execute, ControlFlow};
use crate::ir::ast::{Statement, Type};
use crate::parser::parser::parse;
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;*/

pub mod interpreter;
pub mod ir;
pub mod parser;
pub mod tc;

fn main() {
    println!("Hello, world!");
}
/*
fn run_test(name: &str, program: &str) -> String {
    let mut output = String::new();
    output.push_str(&format!("\n=== Running test: {} ===\n", name));
    output.push_str(&format!("Program:\n{}\n\n", program));

    match parse(program) {
        Ok((remaining, statements)) => {
            output.push_str(&format!("Parsed AST: {:#?}\n\n", statements));

            if !remaining.is_empty() {
                output.push_str(&format!(
                    "Warning: Unparsed input remains: {:?}\n\n",
                    remaining
                ));
                return output;
            }

            let mut current_env = HashMap::new();

            // Initialize variables
            for stmt in &statements {
                match stmt {
                    Statement::Assignment(name, _, _) => {
                        if !current_env.contains_key(name) {
                            current_env.insert(name.clone(), (None, Type::TInteger));
                        }
                    }
                    Statement::FuncDef(func) => {
                        if !current_env.contains_key(name) {
                            current_env.insert(name.clone(), (None, func.kind.clone()));
                        }
                    }
                    Statement::IfThenElse(_, then_block, else_block) => {
                        // Handle variables in if blocks
                        if let Statement::Block(stmts) = &**then_block {
                            for s in stmts {
                                if let Statement::Assignment(name, _, _) = s {
                                    if !current_env.contains_key(name) {
                                        current_env.insert(name.clone(), (None, Type::TInteger));
                                    }
                                }
                            }
                        }
                        if let Some(else_stmt) = else_block {
                            if let Statement::Block(stmts) = &**else_stmt {
                                for s in stmts {
                                    if let Statement::Assignment(name, _, _) = s {
                                        if !current_env.contains_key(name) {
                                            current_env
                                                .insert(name.clone(), (None, Type::TInteger));
                                        }
                                    }
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }

            // Execute statements
            for stmt in statements {
                match execute(stmt, &current_env, false) {
                    Ok(ControlFlow::Continue(new_env)) => {
                        current_env = new_env;
                    }
                    Ok(ControlFlow::Return(value)) => {
                        output.push_str(&format!("Return value: {:?}\n", value));
                        return output;
                    }
                    Err(e) => {
                        output.push_str(&format!("Execution error: {}\n", e));
                        return output;
                    }
                }
            }
            output.push_str(&format!("\nFinal environment: {:?}\n", current_env));
        }
        Err(e) => output.push_str(&format!("Parse error: {:?}\n", e)),
    }
    output
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

    let test8 = r#"def add(a: TInteger, b: TInteger) -> TInteger:
    return a + b

x = 5
y = 3
result = add(x, y)"#;
    run_test("8. Basic function definition and call", test8);

    // Recursive Function Test
    let test9 = r#"def fibonacci(n: TInteger) -> TInteger:
    if n < 0:
        return 0
    if n <= 2:
        return n - 1
    return fibonacci(n - 1) + fibonacci(n - 2)

fib = fibonacci(10)"#;
    run_test("9. Recursive function", test9);

    // Function with Multiple Returns Test
    let test10 = r#"def max(a: TInteger, b: TInteger) -> TInteger:
    if a > b:
        return a
    else:
        return b

result = max(15, 10)"#;
    run_test("10. Function with multiple return paths", test10);

    // Test 11: Left recursion and operator precedence
    let test11 = r#"a = 1 + 2 + 3
b = 1 - 2 - 3
c = 2 * 3 * 4
d = 12 / 4 / 2
e = 1 + 2 * 3
f = (1 + 2) * 3
g = 2 * 3 + 4 * 5
h = (2 + 3) * (4 + 5)"#;
    run_test("11. Left recursion and operator precedence", test11);

    // Test 12: Complex expression chains
    let test12 = r#"x = 1
y = x + 2 + 3 * 4 + 5
z = (x + y) * 2 - 3"#;
    run_test("12. Complex expression chains", test12);

    // Test 13: Mixed operator precedence
    let test13 = r#"a = 10
b = 2
c = 3
result = a + b * c - (a / b) + c * (a - b)"#;
    run_test("13. Mixed operator precedence", test13);

    // Test 14: Deeply nested expressions
    let test14 = r#"x = 2
y = 3
z = ((x + y) * (x - y)) / (x + 1) + y * (x + y)"#;
    run_test("14. Deeply nested expressions", test14);

    // Test 15: Basic negative numbers
    let test15 = r#"a = -5
b = -10
c = -15
result = a + b + c"#;
    run_test("15. Basic negative numbers", test15);

    // Test 16: Mixed positive and negative operations
    let test16 = r#"x = 10
y = -3
z = x + y * -2
w = -x + (-y * 2)"#;
    run_test("16. Mixed positive and negative operations", test16);

    // Test 17: Negative numbers in complex expressions
    let test17 = r#"a = -2
b = 3
c = (-a * b) + (-4 * -5)
d = (a + -b) * (-2 + b)"#;
    run_test("17. Complex expressions with negatives", test17);

    // Test 18: Negative numbers in function calls
    let test18 = r#"def subtract(a: TInteger, b: TInteger) -> TInteger:
    return a - b

result1 = subtract(-10, -3)
result2 = subtract(5, -3)
result3 = subtract(-5, 3)"#;
    run_test("18. Function calls with negative numbers", test18);

    // Test 19: Boolean Operations
    let test19 = r#"a = True
b = False
c = True and False
d = True or False
e = not False
f = not (True and False) or True"#;
    run_test("19. Boolean Operations", test19);

    // Test 20: Real Numbers
    let test20 = r#"x = 3.14
y = -2.5
z = x + y
w = x * 2.0
v = 10.5 / 2.1"#;
    run_test("20. Real Number Operations", test20);

    // Test 21: String Operations
    let test21 = r#"name = "Hello"
greeting = "World"
message = "Test ""#;
    run_test("21. String Operations", test21);

    // Test 22: Mixed Boolean Operations
    let test22 = r#"x = 10
y = 5
result1 = x > y and True
result2 = (x < y) or True
result3 = not (x <= y)
result4 = (x >= y) and (not False)"#;
    run_test("22. Mixed Boolean Operations", test22);

    let mut all_results = String::new();

    all_results.push_str(&run_test("1. Basic if-else", test1));
    all_results.push_str(&run_test("2. Arithmetic operations", test2));
    all_results.push_str(&run_test(
        "3. Nested if statements with multiple operations",
        test3,
    ));
    all_results.push_str(&run_test("4. Multiple assignments and references", test4));
    all_results.push_str(&run_test("5. Complex arithmetic expressions", test5));
    all_results.push_str(&run_test("6. Multiple nested comparisons", test6));
    all_results.push_str(&run_test("7. Mixed arithmetic and control flow", test7));
    all_results.push_str(&run_test("8. Basic function definition and call", test8));
    all_results.push_str(&run_test("9. Recursive function", test9));
    all_results.push_str(&run_test("10. Function with multiple return paths", test10));
    all_results.push_str(&run_test(
        "11. Left recursion and operator precedence",
        test11,
    ));
    all_results.push_str(&run_test("12. Complex expression chains", test12));
    all_results.push_str(&run_test("13. Mixed operator precedence", test13));
    all_results.push_str(&run_test("14. Deeply nested expressions", test14));
    all_results.push_str(&run_test("15. Basic negative numbers", test15));
    all_results.push_str(&run_test(
        "16. Mixed positive and negative operations",
        test16,
    ));
    all_results.push_str(&run_test("17. Complex expressions with negatives", test17));
    all_results.push_str(&run_test(
        "18. Function calls with negative numbers",
        test18,
    ));
    all_results.push_str(&run_test("19. Boolean Operations", test19));
    all_results.push_str(&run_test("20. Real Number Operations", test20));
    all_results.push_str(&run_test("21. String Operations", test21));
    all_results.push_str(&run_test("22. Mixed Boolean Operations", test22));

    // Write results to file
    let mut file = File::create("test_results.txt").expect("Failed to create file");
    file.write_all(all_results.as_bytes())
        .expect("Failed to write to file");

    // Also print to console
    print!("{}", all_results);
}

/*
fn run_test(name: &str, program: &str) -> String {
    let mut output = String::new();
    output.push_str(&format!("\n=== Running test: {} ===\n", name));
    output.push_str(&format!("Program:\n{}\n\n", program));

    match parse(program) {
        Ok((remaining, statements)) => {
            output.push_str(&format!("Parsed AST: {:#?}\n\n", statements));

            if !remaining.is_empty() {
                output.push_str(&format!(
                    "Warning: Unparsed input remains: {:?}\n\n",
                    remaining
                ));
                return output;
            }

            let mut current_env = Environment::<EnvValue>::new();

            // Initialize variables
            for stmt in &statements {
                match stmt {
                    Statement::Assignment(name, expr, _) => {
                        if current_env.search_frame(name.clone()).is_none() {
                            current_env.insert_variable(
                                name.clone(),
                                EnvValue::Exp(expr.as_ref().clone()),
                            );
                        }
                    }
                    Statement::FuncDef(func) => {
                        current_env
                            .insert_variable(func.name.clone(), EnvValue::Func(func.clone()));
                    }
                    Statement::IfThenElse(_, then_block, else_block) => {
                        // Handle variables in if blocks
                        if let Statement::Block(stmts) = &**then_block {
                            for s in stmts {
                                if let Statement::Assignment(name, _, _) = s {
                                    if !current_env.search_frame(name.clone()).is_none() {
                                        current_env.insert_variable(
                                            name.clone(),
                                            EnvValue::Exp(Expression::CInt(0)),
                                        );
                                    }
                                }
                            }
                        }
                        if let Some(else_stmt) = else_block {
                            if let Statement::Block(stmts) = &**else_stmt {
                                for s in stmts {
                                    if let Statement::Assignment(name, _, _) = s {
                                        if !current_env.search_frame(name.clone()).is_none() {
                                            current_env.insert_variable(
                                                name.clone(),
                                                EnvValue::Exp(Expression::CInt(0)),
                                            );
                                        }
                                    }
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }

            // Execute statements
            for stmt in statements {
                match execute(stmt, &current_env) {
                    Ok(ControlFlow::Continue(new_env)) => {
                        current_env = new_env;
                    }
                    Ok(ControlFlow::Return(value)) => {
                        output.push_str(&format!("Return value: {:?}\n", value));
                        return output;
                    }
                    Err(e) => {
                        output.push_str(&format!("Execution error: {}\n", e));
                        return output;
                    }
                }
            }
            output.push_str(&format!("\nFinal environment: {:?}\n", current_env));
        }
        Err(e) => output.push_str(&format!("Parse error: {:?}\n", e)),
    }
    output
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

    let test8 = r#"def add(a: TInteger, b: TInteger) -> TInteger:
    return a + b

x = 5
y = 3
result = add(x, y)"#;
    run_test("8. Basic function definition and call", test8);

    // Recursive Function Test
    let test9 = r#"def fibonacci(n: TInteger) -> TInteger:
    if n < 0:
        return 0
    if n <= 2:
        return n - 1
    return fibonacci(n - 1) + fibonacci(n - 2)

fib = fibonacci(10)"#;
    run_test("9. Recursive function", test9);

    // Function with Multiple Returns Test
    let test10 = r#"def max(a: TInteger, b: TInteger) -> TInteger:
    if a > b:
        return a
    else:
        return b

result = max(15, 10)"#;
    run_test("10. Function with multiple return paths", test10);

    // Test 11: Left recursion and operator precedence
    let test11 = r#"a = 1 + 2 + 3
b = 1 - 2 - 3
c = 2 * 3 * 4
d = 12 / 4 / 2
e = 1 + 2 * 3
f = (1 + 2) * 3
g = 2 * 3 + 4 * 5
h = (2 + 3) * (4 + 5)"#;
    run_test("11. Left recursion and operator precedence", test11);

    // Test 12: Complex expression chains
    let test12 = r#"x = 1
y = x + 2 + 3 * 4 + 5
z = (x + y) * 2 - 3"#;
    run_test("12. Complex expression chains", test12);

    // Test 13: Mixed operator precedence
    let test13 = r#"a = 10
b = 2
c = 3
result = a + b * c - (a / b) + c * (a - b)"#;
    run_test("13. Mixed operator precedence", test13);

    // Test 14: Deeply nested expressions
    let test14 = r#"x = 2
y = 3
z = ((x + y) * (x - y)) / (x + 1) + y * (x + y)"#;
    run_test("14. Deeply nested expressions", test14);

    // Test 15: Basic negative numbers
    let test15 = r#"a = -5
b = -10
c = -15
result = a + b + c"#;
    run_test("15. Basic negative numbers", test15);

    // Test 16: Mixed positive and negative operations
    let test16 = r#"x = 10
y = -3
z = x + y * -2
w = -x + (-y * 2)"#;
    run_test("16. Mixed positive and negative operations", test16);

    // Test 17: Negative numbers in complex expressions
    let test17 = r#"a = -2
b = 3
c = (-a * b) + (-4 * -5)
d = (a + -b) * (-2 + b)"#;
    run_test("17. Complex expressions with negatives", test17);

    // Test 18: Negative numbers in function calls
    let test18 = r#"def subtract(a: TInteger, b: TInteger) -> TInteger:
    return a - b

result1 = subtract(-10, -3)
result2 = subtract(5, -3)
result3 = subtract(-5, 3)"#;
    run_test("18. Function calls with negative numbers", test18);

    // Test 19: Boolean Operations
    let test19 = r#"a = True
b = False
c = True and False
d = True or False
e = not False
f = not (True and False) or True"#;
    run_test("19. Boolean Operations", test19);

    // Test 20: Real Numbers
    let test20 = r#"x = 3.14
y = -2.5
z = x + y
w = x * 2.0
v = 10.5 / 2.1"#;
    run_test("20. Real Number Operations", test20);

    // Test 21: String Operations
    let test21 = r#"name = "Hello"
greeting = "World"
message = "Test ""#;
    run_test("21. String Operations", test21);

    // Test 22: Mixed Boolean Operations
    let test22 = r#"x = 10
y = 5
result1 = x > y and True
result2 = (x < y) or True
result3 = not (x <= y)
result4 = (x >= y) and (not False)"#;
    run_test("22. Mixed Boolean Operations", test22);

    let mut all_results = String::new();

    all_results.push_str(&run_test("1. Basic if-else", test1));
    all_results.push_str(&run_test("2. Arithmetic operations", test2));
    all_results.push_str(&run_test(
        "3. Nested if statements with multiple operations",
        test3,
    ));
    all_results.push_str(&run_test("4. Multiple assignments and references", test4));
    all_results.push_str(&run_test("5. Complex arithmetic expressions", test5));
    all_results.push_str(&run_test("6. Multiple nested comparisons", test6));
    all_results.push_str(&run_test("7. Mixed arithmetic and control flow", test7));
    all_results.push_str(&run_test("8. Basic function definition and call", test8));
    all_results.push_str(&run_test("9. Recursive function", test9));
    all_results.push_str(&run_test("10. Function with multiple return paths", test10));
    all_results.push_str(&run_test(
        "11. Left recursion and operator precedence",
        test11,
    ));
    all_results.push_str(&run_test("12. Complex expression chains", test12));
    all_results.push_str(&run_test("13. Mixed operator precedence", test13));
    all_results.push_str(&run_test("14. Deeply nested expressions", test14));
    all_results.push_str(&run_test("15. Basic negative numbers", test15));
    all_results.push_str(&run_test(
        "16. Mixed positive and negative operations",
        test16,
    ));
    all_results.push_str(&run_test("17. Complex expressions with negatives", test17));
    all_results.push_str(&run_test(
        "18. Function calls with negative numbers",
        test18,
    ));
    all_results.push_str(&run_test("19. Boolean Operations", test19));
    all_results.push_str(&run_test("20. Real Number Operations", test20));
    all_results.push_str(&run_test("21. String Operations", test21));
    all_results.push_str(&run_test("22. Mixed Boolean Operations", test22));

    // Write results to file
    let mut file = File::create("test_results.txt").expect("Failed to create file");
    file.write_all(all_results.as_bytes())
        .expect("Failed to write to file");

    // Also print to console
    print!("{}", all_results);
}
 */ */
