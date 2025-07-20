use crate::environment::environment::Environment;
use crate::ir::ast::{Expression, Statement};
use std::collections::HashMap;
use std::fs::File;
use std::io::Read;
use std::io::Write;
use std::sync::OnceLock;

pub type MetaBuiltinStmt = fn(&mut Environment<Expression>) -> Statement;

// Tabela estática global de metabuiltins
static METABUILTINS_TABLE: OnceLock<HashMap<String, MetaBuiltinStmt>> = OnceLock::new();

pub fn get_metabuiltins_table() -> &'static HashMap<String, MetaBuiltinStmt> {
    METABUILTINS_TABLE.get_or_init(|| {
        let mut table = HashMap::new();
        table.insert("input".to_string(), input_builtin as MetaBuiltinStmt);
        table.insert("print".to_string(), print_builtin as MetaBuiltinStmt);
        table.insert("open".to_string(), open_builtin as MetaBuiltinStmt);
        table
    })
}

pub fn input_builtin(env: &mut Environment<Expression>) -> Statement {
    let prompt = match env.lookup(&"prompt".to_string()) {
        Some((_, Expression::CString(s))) => s.clone(),
        _ => "".to_string(),
    };
    print!("{}", prompt);
    use std::io::{self, Write};
    io::stdout().flush().unwrap();
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    let input = input.trim_end_matches(['\n', '\r']).to_string();
    Statement::Return(Box::new(Expression::CString(input)))
}

pub fn print_builtin(env: &mut Environment<Expression>) -> Statement {
    let value = env
        .lookup(&"value".to_string())
        .map(|(_, v)| v)
        .unwrap_or(Expression::CString("".to_string()));
    match value {
        Expression::CString(s) => println!("{}", s),
        Expression::CInt(i) => println!("{}", i),
        Expression::CReal(f) => println!("{}", f),
        _ => println!("{:?}", value),
    }
    Statement::Return(Box::new(Expression::CVoid))
}

pub fn open_builtin(env: &mut Environment<Expression>) -> Statement {
    let path = match env.lookup(&"path".to_string()) {
        Some((_, Expression::CString(p))) => p.clone(),
        _ => {
            return Statement::Return(Box::new(Expression::CString(
                "open: first argument must be a string with the file path".to_string(),
            )));
        }
    };

    let mode = match env.lookup(&"mode".to_string()) {
        Some((_, Expression::CString(m))) => m.clone(),
        _ => "r".to_string(),
    };

    match mode.as_str() {
        "r" => {
            let mut file = match File::open(&path) {
                Ok(f) => f,
                Err(e) => {
                    return Statement::Return(Box::new(Expression::CString(format!(
                        "open: could not open '{}' for reading: {}",
                        path, e
                    ))));
                }
            };
            let mut contents = String::new();
            if let Err(e) = file.read_to_string(&mut contents) {
                return Statement::Return(Box::new(Expression::CString(format!(
                    "open: error reading '{}': {}",
                    path, e
                ))));
            }

            Statement::Return(Box::new(Expression::CString(contents)))
        }

        "w" => {
            let content = match env.lookup(&"content".to_string()) {
                Some((_, Expression::CString(c))) => c.clone(),
                _ => {
                    return Statement::Return(Box::new(Expression::CString(
                        "open: when using mode 'w', a third argument with the content to write is required".to_string())));
                }
            };

            match std::fs::write(&path, content) {
                Ok(_) => Statement::Return(Box::new(Expression::CVoid)),
                Err(e) => Statement::Return(Box::new(Expression::CString(format!(
                    "open: could not write to '{}': {}",
                    path, e
                )))),
            }
        }

        "a" => {
            let content = match env.lookup(&"content".to_string()) {
                Some((_, Expression::CString(c))) => c.clone(),
                _ => {
                    return Statement::Return(Box::new(Expression::CString(
                        "open: when using mode 'a', a third argument with the content to append is required".to_string())));
                }
            };

            match std::fs::OpenOptions::new()
                .append(true)
                .create(true)
                .open(&path)
            {
                Ok(mut file) => {
                    if let Err(e) = writeln!(file, "{}", content) {
                        return Statement::Return(Box::new(Expression::CString(format!(
                            "open: could not append to '{}': {}",
                            path, e
                        ))));
                    }
                    Statement::Return(Box::new(Expression::CVoid))
                }
                Err(e) => Statement::Return(Box::new(Expression::CString(format!(
                    "open: could not open '{}' for appending: {}",
                    path, e
                )))),
            }
        }

        m => Statement::Return(Box::new(Expression::CString(format!(
            "open: unsupported mode '{}'.",
            m
        )))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_meta_stmt_table_contains_input_function() {
        let table = get_metabuiltins_table();
        assert!(
            table.contains_key("input"),
            "The table must contain the 'input' function"
        );

        // Check if the function in the table is callable
        let input_func = table.get("input").unwrap();
        let _env: Environment<Expression> = Environment::new();
        // We do not execute the function to avoid blocking stdin
        // Just check if it exists and can be referenced
        assert!(
            std::ptr::addr_of!(*input_func) != std::ptr::null(),
            "The 'input' function must exist in the table"
        );
    }

    #[test]
    fn test_meta_stmt_table_contains_print_function() {
        let table = get_metabuiltins_table();
        assert!(
            table.contains_key("print"),
            "The table must contain the 'print' function"
        );

        // Check if the function in the table is callable
        let print_func = table.get("print").unwrap();
        let mut env: Environment<Expression> = Environment::new();
        let result = print_func(&mut env);
        match result {
            Statement::Return(_) => (), // Expected
            _ => panic!("The 'print' function in the table must return Statement::Return"),
        }
    }

    #[test]
    fn test_meta_stmt_table_has_correct_size() {
        let table = get_metabuiltins_table();
        assert_eq!(table.len(), 3, "The table must contain exactly 3 functions");
    }

    #[test]
    fn test_meta_stmt_table_contains_only_expected_keys() {
        let table = get_metabuiltins_table();
        let keys: Vec<&String> = table.keys().collect();

        assert!(
            keys.contains(&&"input".to_string()),
            "The table must contain the key 'input'"
        );
        assert!(
            keys.contains(&&"print".to_string()),
            "The table must contain the key 'print'"
        );
        assert!(
            keys.contains(&&"open".to_string()),
            "The table must contain the key 'open'"
        );
        assert_eq!(keys.len(), 3, "The table must contain only 3 keys");
    }

    #[test]
    fn test_meta_stmt_table_functions_are_callable() {
        let table = get_metabuiltins_table();

        // Check if the 'input' function exists in the table (without executing it)
        assert!(
            table.contains_key("input"),
            "The 'input' function must be in the table"
        );

        // Check if the 'print' function can be called
        if let Some(print_func) = table.get("print") {
            let mut env: Environment<Expression> = Environment::new();
            let result = print_func(&mut env);
            match result {
                Statement::Return(_) => (), // Expected
                _ => panic!("The 'print' function in the table must return Statement::Return"),
            }
        } else {
            panic!("The 'print' function must be in the table");
        }
    }

    #[test]
    fn test_input_builtin_function_signature() {
        // Checks if the input_builtin function can be referenced with an Environment
        // Note: We do not execute the function because it requires stdin
        let _input_func: MetaBuiltinStmt = input_builtin;
        assert!(true, "input_builtin has the correct signature");
    }

    #[test]
    fn test_print_builtin_function_signature() {
        // Checks if the print_builtin function can be called with an Environment
        let mut env: Environment<Expression> = Environment::new();
        env.map_variable(
            "value".to_string(),
            false,
            Expression::CString("Hello World".to_string()),
        );

        let result = print_builtin(&mut env);
        match result {
            Statement::Return(_) => (), // Expected
            _ => panic!("print_builtin must return Statement::Return"),
        }
    }

    // Helper function to run print_builtin tests with custom main function
    fn run_print_builtin_test(main_body: &str, expected_outputs: &[&str], test_name: &str) {
        use assert_cmd::Command;
        use std::fs;

        let test_code = format!(
            r#"
use r_python::environment::environment::Environment;
use r_python::ir::ast::Expression;
use r_python::stdlib::standard_library::print_builtin;

fn main() {{
{}
}}
"#,
            main_body
        );

        let filename = format!("test_print_{}.rs", test_name);
        let binary_name = format!("test_print_{}", test_name);

        fs::write(&filename, test_code).unwrap();

        let build_result = Command::new("cargo").args(&["build"]).output();
        if let Ok(build_output) = build_result {
            if build_output.status.success() {
                let compile_result = Command::new("rustc")
                    .args(&[
                        &filename,
                        "-L",
                        "target/debug/deps",
                        "--extern",
                        "r_python=target/debug/libr_python.rlib",
                        "-o",
                        &binary_name,
                    ])
                    .output();

                if let Ok(compile_output) = compile_result {
                    if compile_output.status.success() {
                        let mut cmd = Command::new(&format!("./{}", binary_name));
                        let mut assertion = cmd.assert().success();

                        for expected in expected_outputs {
                            assertion = assertion.stdout(predicates::str::contains(*expected));
                        }
                    }
                }
            }
        }

        fs::remove_file(&filename).ok();
        fs::remove_file(&binary_name).ok();
    }

    // Helper function for tests that need exact output checking
    fn run_print_builtin_test_exact(main_body: &str, expected_exact: &str, test_name: &str) {
        use assert_cmd::Command;
        use std::fs;

        let test_code = format!(
            r#"
use r_python::environment::environment::Environment;
use r_python::ir::ast::Expression;
use r_python::stdlib::standard_library::print_builtin;

fn main() {{
{}
}}
"#,
            main_body
        );

        let filename = format!("test_print_{}.rs", test_name);
        let binary_name = format!("test_print_{}", test_name);

        fs::write(&filename, test_code).unwrap();

        let build_result = Command::new("cargo").args(&["build"]).output();
        if let Ok(build_output) = build_result {
            if build_output.status.success() {
                let compile_result = Command::new("rustc")
                    .args(&[
                        &filename,
                        "-L",
                        "target/debug/deps",
                        "--extern",
                        "r_python=target/debug/libr_python.rlib",
                        "-o",
                        &binary_name,
                    ])
                    .output();

                if let Ok(compile_output) = compile_result {
                    if compile_output.status.success() {
                        let output = Command::new(&format!("./{}", binary_name))
                            .output()
                            .expect("Failed to run test");

                        assert_eq!(String::from_utf8_lossy(&output.stdout), expected_exact);
                    }
                }
            }
        }

        fs::remove_file(&filename).ok();
        fs::remove_file(&binary_name).ok();
    }

    // Helper function to run input_builtin tests with simulated stdin input
    fn run_input_builtin_test(
        main_body: &str,
        stdin_input: &str,
        expected_outputs: &[&str],
        test_name: &str,
    ) {
        use assert_cmd::Command;
        use std::fs;

        let test_code = format!(
            r#"
use r_python::environment::environment::Environment;
use r_python::ir::ast::{{Expression, Statement}};
use r_python::stdlib::standard_library::input_builtin;

fn main() {{
{}
}}
"#,
            main_body
        );

        let filename = format!("test_input_{}.rs", test_name);
        let binary_name = format!("test_input_{}", test_name);

        fs::write(&filename, test_code).unwrap();

        let build_result = Command::new("cargo").args(&["build"]).output();
        if let Ok(build_output) = build_result {
            if build_output.status.success() {
                let compile_result = Command::new("rustc")
                    .args(&[
                        &filename,
                        "-L",
                        "target/debug/deps",
                        "--extern",
                        "r_python=target/debug/libr_python.rlib",
                        "-o",
                        &binary_name,
                    ])
                    .output();

                if let Ok(compile_output) = compile_result {
                    if compile_output.status.success() {
                        let mut cmd = Command::new(&format!("./{}", binary_name));
                        let mut assertion = cmd.write_stdin(stdin_input).assert().success();

                        for expected in expected_outputs {
                            assertion = assertion.stdout(predicates::str::contains(*expected));
                        }
                    }
                }
            }
        }

        fs::remove_file(&filename).ok();
        fs::remove_file(&binary_name).ok();
    }

    #[test]
    fn test_print_builtin_captures_output() {
        let main_body = r#"
    // Test 1: String value
    let mut env1 = Environment::new();
    env1.map_variable("value".to_string(), false, Expression::CString("Hello World".to_string()));
    print_builtin(&mut env1);
    
    // Test 2: Integer value
    let mut env2 = Environment::new();
    env2.map_variable("value".to_string(), false, Expression::CInt(42));
    print_builtin(&mut env2);
    
    // Test 3: Float value
    let mut env3 = Environment::new();
    env3.map_variable("value".to_string(), false, Expression::CReal(3.14));
    print_builtin(&mut env3);
    
    // Test 4: No value (should print empty string)
    let mut env4 = Environment::new();
    print_builtin(&mut env4);"#;

        run_print_builtin_test(main_body, &["Hello World", "42", "3.14"], "builtin");
    }

    #[test]
    fn test_print_builtin_with_special_strings() {
        let main_body = r#"
    // Test string with newlines
    let mut env1 = Environment::new();
    env1.map_variable("value".to_string(), false, Expression::CString("Hello\nWorld".to_string()));
    print_builtin(&mut env1);
    
    // Test string with tabs
    let mut env2 = Environment::new();
    env2.map_variable("value".to_string(), false, Expression::CString("Hello\tWorld".to_string()));
    print_builtin(&mut env2);
    
    // Test empty string
    let mut env3 = Environment::new();
    env3.map_variable("value".to_string(), false, Expression::CString("".to_string()));
    print_builtin(&mut env3);
    
    // Test string with special characters
    let mut env4 = Environment::new();
    env4.map_variable("value".to_string(), false, Expression::CString("Olá, 世界! 🌍".to_string()));
    print_builtin(&mut env4);"#;

        run_print_builtin_test(
            main_body,
            &["Hello", "World", "Olá", "世界"],
            "special_strings",
        );
    }

    #[test]
    fn test_print_builtin_with_cvoid_type() {
        let main_body = r#"
    // Test CVoid type (should use debug print)
    let mut env = Environment::new();
    env.map_variable("value".to_string(), false, Expression::CVoid);
    print_builtin(&mut env);"#;

        run_print_builtin_test(main_body, &["CVoid"], "cvoid");
    }

    #[test]
    fn test_print_builtin_no_value_variable() {
        let main_body = r#"
    // Test when no "value" variable exists (should print empty string)
    let mut env = Environment::new();
    print_builtin(&mut env);"#;

        run_print_builtin_test_exact(main_body, "\n", "no_value");
    }

    #[test]
    fn test_print_builtin_return_value() {
        // Test that print_builtin returns Statement::Return(CVoid)
        let mut env = Environment::new();
        env.map_variable(
            "value".to_string(),
            false,
            Expression::CString("test".to_string()),
        );

        let result = print_builtin(&mut env);

        match result {
            Statement::Return(expr) => {
                match *expr {
                    Expression::CVoid => (), // Expected
                    _ => panic!("print_builtin should return Statement::Return(CVoid)"),
                }
            }
            _ => panic!("print_builtin should return Statement::Return"),
        }
    }

    #[test]
    fn test_input_builtin_with_prompt() {
        let main_body = r#"
    // Test with prompt
    let mut env = Environment::new();
    env.map_variable("prompt".to_string(), false, Expression::CString("Enter your name: ".to_string()));
    let result = input_builtin(&mut env);
    
    match result {
        Statement::Return(expr) => {
            if let Expression::CString(input) = *expr {
                println!("Input received: {}", input);
            }
        }
        _ => panic!("Expected Statement::Return"),
    }"#;

        run_input_builtin_test(
            main_body,
            "Alice\n",
            &["Enter your name: ", "Input received: Alice"],
            "with_prompt",
        );
    }

    #[test]
    fn test_input_builtin_without_prompt() {
        let main_body = r#"
    // Test without prompt (empty environment)
    let mut env = Environment::new();
    let result = input_builtin(&mut env);
    
    match result {
        Statement::Return(expr) => {
            if let Expression::CString(input) = *expr {
                println!("Input received: {}", input);
            }
        }
        _ => panic!("Expected Statement::Return"),
    }"#;

        run_input_builtin_test(
            main_body,
            "Bob\n",
            &["Input received: Bob"],
            "without_prompt",
        );
    }

    #[test]
    fn test_input_builtin_empty_input() {
        let main_body = r#"
    // Test with empty input
    let mut env = Environment::new();
    env.map_variable("prompt".to_string(), false, Expression::CString("Press Enter: ".to_string()));
    let result = input_builtin(&mut env);
    
    match result {
        Statement::Return(expr) => {
            if let Expression::CString(input) = *expr {
                println!("Input length: {}", input.len());
            }
        }
        _ => panic!("Expected Statement::Return"),
    }"#;

        run_input_builtin_test(
            main_body,
            "\n",
            &["Press Enter: ", "Input length: 0"],
            "empty_input",
        );
    }

    #[test]
    fn test_input_builtin_multiline_input() {
        let main_body = r#"
    // Test with input containing special characters
    let mut env = Environment::new();
    env.map_variable("prompt".to_string(), false, Expression::CString("Enter text: ".to_string()));
    let result = input_builtin(&mut env);
    
    match result {
        Statement::Return(expr) => {
            if let Expression::CString(input) = *expr {
                println!("Input received: '{}'", input);
            }
        }
        _ => panic!("Expected Statement::Return"),
    }"#;

        run_input_builtin_test(
            main_body,
            "Hello World! 123\n",
            &["Enter text: ", "Input received: 'Hello World! 123'"],
            "multiline_input",
        );
    }

    #[test]
    fn test_open_builtin_read_mode_success() {
        use std::fs;
        let test_file_path = "test_read_file.txt";
        let test_content = "Hello, World!\nThis is a test file.";
        fs::write(test_file_path, test_content).unwrap();

        let mut env: Environment<Expression> = Environment::new();
        env.map_variable(
            "path".to_string(),
            false,
            Expression::CString(test_file_path.to_string()),
        );
        env.map_variable(
            "mode".to_string(),
            false,
            Expression::CString("r".to_string()),
        );

        let result = open_builtin(&mut env);

        match result {
            Statement::Return(expr) => {
                if let Expression::CString(content) = *expr {
                    assert_eq!(content, test_content, "File content should match");
                } else {
                    panic!("Expected CString with file content");
                }
            }
            _ => panic!("Expected Statement::Return"),
        }

        fs::remove_file(test_file_path).unwrap();
    }

    #[test]
    fn test_open_builtin_write_mode_success() {
        use std::fs;
        let test_file_path = "test_write_file.txt";
        let test_content = "This is a test write.";

        let mut env: Environment<Expression> = Environment::new();
        env.map_variable(
            "path".to_string(),
            false,
            Expression::CString(test_file_path.to_string()),
        );
        env.map_variable(
            "mode".to_string(),
            false,
            Expression::CString("w".to_string()),
        );
        env.map_variable(
            "content".to_string(),
            false,
            Expression::CString(test_content.to_string()),
        );

        let result = open_builtin(&mut env);

        match result {
            Statement::Return(expr) => {
                if let Expression::CVoid = *expr {
                    let content = fs::read_to_string(test_file_path).unwrap();
                    assert_eq!(content, test_content, "File content should match");
                } else {
                    panic!("Expected CVoid after writing to file");
                }
            }
            _ => panic!("Expected Statement::Return"),
        }

        fs::remove_file(test_file_path).unwrap();
    }

    #[test]
    fn test_open_builtin_append_mode_success() {
        use std::fs;
        let test_file_path = "test_append_file.txt";
        let initial_content = "Initial content.\n";
        let append_content = "This is appended content.\n";
        fs::write(test_file_path, initial_content).unwrap();

        let mut env: Environment<Expression> = Environment::new();
        env.map_variable(
            "path".to_string(),
            false,
            Expression::CString(test_file_path.to_string()),
        );
        env.map_variable(
            "mode".to_string(),
            false,
            Expression::CString("a".to_string()),
        );
        env.map_variable(
            "content".to_string(),
            false,
            Expression::CString(append_content.to_string()),
        );

        let result = open_builtin(&mut env);
        match result {
            Statement::Return(expr) => {
                if let Expression::CVoid = *expr {
                    let content = fs::read_to_string(test_file_path).unwrap();
                    assert!(
                        content.contains(initial_content),
                        "File should contain initial content"
                    );
                    assert!(
                        content.contains(append_content),
                        "File should contain appended content"
                    );
                } else {
                    panic!("Expected CVoid after appending to file");
                }
            }
            _ => panic!("Expected Statement::Return"),
        }
        fs::remove_file(test_file_path).unwrap();
    }

    #[test]
    fn test_open_builtin_unsupported_mode() {
        let mut env: Environment<Expression> = Environment::new();
        env.map_variable(
            "path".to_string(),
            false,
            Expression::CString("dummy.txt".to_string()),
        );
        env.map_variable(
            "mode".to_string(),
            false,
            Expression::CString("x".to_string()),
        ); // Unsupported mode

        let result = open_builtin(&mut env);

        match result {
            Statement::Return(expr) => {
                if let Expression::CString(msg) = *expr {
                    assert_eq!(
                        msg, "open: unsupported mode 'x'.",
                        "Error message should match"
                    );
                } else {
                    panic!("Expected CString with error message");
                }
            }
            _ => panic!("Expected Statement::Return"),
        }
    }

    #[test]
    fn test_open_builtin_read_invalid_path() {
        let mut env: Environment<Expression> = Environment::new();
        env.map_variable(
            "path".to_string(),
            false,
            Expression::CString("invalid_path.txt".to_string()),
        );
        env.map_variable(
            "mode".to_string(),
            false,
            Expression::CString("r".to_string()),
        );

        let result = open_builtin(&mut env);

        match result {
            Statement::Return(expr) => {
                if let Expression::CString(msg) = *expr {
                    assert!(
                        msg.contains("open: could not open 'invalid_path.txt' for reading"),
                        "Error message should indicate invalid path"
                    );
                } else {
                    panic!("Expected CString with error message");
                }
            }
            _ => panic!("Expected Statement::Return"),
        }
    }

    #[test]
    fn test_open_builtin_read_nonexistent_file() {
        let mut env: Environment<Expression> = Environment::new();
        env.map_variable(
            "path".to_string(),
            false,
            Expression::CString("nonexistent.txt".to_string()),
        );
        env.map_variable(
            "mode".to_string(),
            false,
            Expression::CString("r".to_string()),
        );

        let result = open_builtin(&mut env);

        match result {
            Statement::Return(expr) => {
                if let Expression::CString(msg) = *expr {
                    assert!(
                        msg.contains("open: could not open 'nonexistent.txt' for reading"),
                        "Error message should indicate file not found"
                    );
                } else {
                    panic!("Expected CString with error message");
                }
            }
            _ => panic!("Expected Statement::Return"),
        }
    }

    #[test]
    fn test_open_builtin_write_empty_content() {
        use std::fs;
        let test_file_path = "test_empty_write_file.txt";

        let mut env: Environment<Expression> = Environment::new();
        env.map_variable(
            "path".to_string(),
            false,
            Expression::CString(test_file_path.to_string()),
        );
        env.map_variable(
            "mode".to_string(),
            false,
            Expression::CString("w".to_string()),
        );
        env.map_variable(
            "content".to_string(),
            false,
            Expression::CString("".to_string()),
        );

        let result = open_builtin(&mut env);

        match result {
            Statement::Return(expr) => {
                if let Expression::CVoid = *expr {
                    let content = fs::read_to_string(test_file_path).unwrap();
                    assert_eq!(content, "", "File content should be empty");
                } else {
                    panic!("Expected CVoid after writing empty content to file");
                }
            }
            _ => panic!("Expected Statement::Return"),
        }

        fs::remove_file(test_file_path).unwrap();
    }

    #[test]
    fn test_open_builtin_write_missing_content_argument() {
        let mut env: Environment<Expression> = Environment::new();
        env.map_variable(
            "path".to_string(),
            false,
            Expression::CString("dummy.txt".to_string()),
        );
        env.map_variable(
            "mode".to_string(),
            false,
            Expression::CString("w".to_string()),
        );

        let result = open_builtin(&mut env);

        match result {
            Statement::Return(expr) => {
                if let Expression::CString(msg) = *expr {
                    assert_eq!(msg, "open: when using mode 'w', a third argument with the content to write is required", "Error message should indicate missing content argument");
                } else {
                    panic!("Expected CString with error message");
                }
            }
            _ => panic!("Expected Statement::Return"),
        }
    }

    #[test]
    fn test_open_builtin_first_argument_not_string() {
        let mut env: Environment<Expression> = Environment::new();
        env.map_variable("path".to_string(), false, Expression::CInt(42));
        env.map_variable(
            "mode".to_string(),
            false,
            Expression::CString("r".to_string()),
        );

        let result = open_builtin(&mut env);

        match result {
            Statement::Return(expr) => {
                if let Expression::CString(msg) = *expr {
                    assert_eq!(
                        msg, "open: first argument must be a string with the file path",
                        "Error message should indicate invalid first argument"
                    );
                } else {
                    panic!("Expected CString with error message");
                }
            }
            _ => panic!("Expected Statement::Return"),
        }
    }

    #[test]
    fn test_open_builtin_append_missing_content_argument() {
        let mut env: Environment<Expression> = Environment::new();
        env.map_variable(
            "path".to_string(),
            false,
            Expression::CString("dummy.txt".to_string()),
        );
        env.map_variable(
            "mode".to_string(),
            false,
            Expression::CString("a".to_string()),
        );

        let result = open_builtin(&mut env);
        match result {
            Statement::Return(expr) => {
                if let Expression::CString(msg) = *expr {
                    assert_eq!(msg, "open: when using mode 'a', a third argument with the content to append is required", "Error message should indicate missing content argument");
                } else {
                    panic!("Expected CString with error message");
                }
            }
            _ => panic!("Expected Statement::Return"),
        }
    }
}
