#[cfg(test)]
mod integration_tests {
    use crate::environment::environment::Environment;
    use crate::ir::ast::Expression;
    use crate::interpreter::expression_eval::{eval, ExpressionResult};
    use crate::interpreter::builtins::register_builtins;

    #[test]
    fn test_subprocess_run_integration() {
        let mut env = Environment::new();
        register_builtins(&mut env);

        // Create a function call expression for subprocess.run
        let function_call = Expression::FuncCall(
            "subprocess.run".to_string(),
            vec![
                Expression::ListValue(vec![
                    Expression::CString("echo".to_string()),
                    Expression::CString("integration_test".to_string()),
                ]),
                Expression::CFalse, // shell=False
                Expression::CTrue,  // capture_output=True
            ],
        );

        // Evaluate the function call
        let result = eval(function_call, &env);
        assert!(result.is_ok());

        // Check that we get a CompletedProcess result
        match result.unwrap() {
            ExpressionResult::Value(Expression::CompletedProcess { returncode, stdout, stderr }) => {
                assert_eq!(returncode, 0);
                assert!(stdout.is_some());
                assert!(stderr.is_some());
                assert!(stdout.unwrap().contains("integration_test"));
            }
            _ => panic!("Expected CompletedProcess result from subprocess.run"),
        }
    }

    #[test]
    fn test_subprocess_run_shell_integration() {
        let mut env = Environment::new();
        register_builtins(&mut env);

        // Create a shell command function call
        let function_call = Expression::FuncCall(
            "subprocess.run".to_string(),
            vec![
                Expression::CString("echo 'shell integration test'".to_string()),
                Expression::CTrue,  // shell=True
                Expression::CTrue,  // capture_output=True
            ],
        );

        // Evaluate the function call
        let result = eval(function_call, &env);
        assert!(result.is_ok());

        // Check that we get a CompletedProcess result
        match result.unwrap() {
            ExpressionResult::Value(Expression::CompletedProcess { returncode, stdout, stderr }) => {
                assert_eq!(returncode, 0);
                assert!(stdout.is_some());
                assert!(stderr.is_some());
                assert!(stdout.unwrap().contains("shell integration test"));
            }
            _ => panic!("Expected CompletedProcess result from subprocess.run shell command"),
        }
    }

    #[test]
    fn test_subprocess_run_error_integration() {
        let mut env = Environment::new();
        register_builtins(&mut env);

        // Create a function call with a non-existent command
        let function_call = Expression::FuncCall(
            "subprocess.run".to_string(),
            vec![
                Expression::ListValue(vec![
                    Expression::CString("nonexistent_command_xyz123".to_string()),
                ]),
                Expression::CFalse, // shell=False
                Expression::CFalse, // capture_output=False
            ],
        );

        // Evaluate the function call
        let result = eval(function_call, &env);
        assert!(result.is_ok());

        // Check that we get an error result (CErr)
        match result.unwrap() {
            ExpressionResult::Value(Expression::CErr(error)) => {
                match *error {
                    Expression::CString(msg) => {
                        assert!(msg.contains("Command not found"));
                    }
                    _ => panic!("Expected string error message"),
                }
            }
            _ => panic!("Expected CErr result for non-existent command"),
        }
    }

    #[test]
    fn test_regular_function_call_still_works() {
        let mut env = Environment::new();
        register_builtins(&mut env);

        // Test that regular function calls still work (should fail with function not found)
        let function_call = Expression::FuncCall(
            "regular_function".to_string(),
            vec![Expression::CInt(42)],
        );

        let result = eval(function_call, &env);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Function regular_function not found"));
    }

    #[test]
    fn test_subprocess_run_ls_command_integration() {
        let mut env = Environment::new();
        register_builtins(&mut env);

        // Test ls command through RPython interpreter (Requirements 1.1, 1.2, 4.1, 4.2)
        let function_call = Expression::FuncCall(
            "subprocess.run".to_string(),
            vec![
                Expression::ListValue(vec![
                    Expression::CString("ls".to_string()),
                    Expression::CString("-la".to_string()),
                    Expression::CString(".".to_string()),
                ]),
                Expression::CFalse, // shell=False
                Expression::CTrue,  // capture_output=True
            ],
        );

        let result = eval(function_call, &env);
        assert!(result.is_ok());

        match result.unwrap() {
            ExpressionResult::Value(Expression::CompletedProcess { returncode, stdout, stderr }) => {
                assert_eq!(returncode, 0);
                assert!(stdout.is_some());
                assert!(stderr.is_some());
                let stdout_content = stdout.unwrap();
                assert!(!stdout_content.is_empty());
                // Should contain directory listing information
                assert!(stdout_content.contains(".") || stdout_content.contains("total"));
            }
            _ => panic!("Expected CompletedProcess result from ls command"),
        }
    }

    #[test]
    fn test_subprocess_run_with_different_argument_combinations() {
        let mut env = Environment::new();
        register_builtins(&mut env);

        // Test with only command argument (Requirements 1.1, 2.4)
        let function_call = Expression::FuncCall(
            "subprocess.run".to_string(),
            vec![
                Expression::ListValue(vec![
                    Expression::CString("echo".to_string()),
                    Expression::CString("minimal_args".to_string()),
                ]),
            ],
        );

        let result = eval(function_call, &env);
        assert!(result.is_ok());
        match result.unwrap() {
            ExpressionResult::Value(Expression::CompletedProcess { returncode, stdout, stderr }) => {
                assert_eq!(returncode, 0);
                assert!(stdout.is_none()); // capture_output defaults to False
                assert!(stderr.is_none());
            }
            _ => panic!("Expected CompletedProcess result"),
        }

        // Test with command and shell arguments (Requirements 2.1, 2.4)
        let function_call = Expression::FuncCall(
            "subprocess.run".to_string(),
            vec![
                Expression::CString("echo 'shell_only'".to_string()),
                Expression::CTrue,  // shell=True
            ],
        );

        let result = eval(function_call, &env);
        assert!(result.is_ok());
        match result.unwrap() {
            ExpressionResult::Value(Expression::CompletedProcess { returncode, stdout, stderr }) => {
                assert_eq!(returncode, 0);
                assert!(stdout.is_none()); // capture_output defaults to False
                assert!(stderr.is_none());
            }
            _ => panic!("Expected CompletedProcess result"),
        }
    }

    #[test]
    fn test_subprocess_run_comprehensive_integration() {
        let mut env = Environment::new();
        register_builtins(&mut env);

        // Test comprehensive scenario with shell, pipes, and output capture
        // (Requirements 2.1, 2.2, 3.1, 3.2, 4.1, 4.2)
        let function_call = Expression::FuncCall(
            "subprocess.run".to_string(),
            vec![
                Expression::CString("echo 'test line 1' && echo 'test line 2'".to_string()),
                Expression::CTrue,  // shell=True
                Expression::CTrue,  // capture_output=True
            ],
        );

        let result = eval(function_call, &env);
        assert!(result.is_ok());

        match result.unwrap() {
            ExpressionResult::Value(Expression::CompletedProcess { returncode, stdout, stderr }) => {
                assert_eq!(returncode, 0);
                assert!(stdout.is_some());
                assert!(stderr.is_some());
                let stdout_content = stdout.unwrap();
                assert!(stdout_content.contains("test line 1"));
                assert!(stdout_content.contains("test line 2"));
            }
            _ => panic!("Expected CompletedProcess result from comprehensive test"),
        }
    }

    #[test]
    fn test_subprocess_run_python_script_example() {
        let mut env = Environment::new();
        register_builtins(&mut env);

        // Test running a Python script through shell mode
        let function_call = Expression::FuncCall(
            "subprocess.run".to_string(),
            vec![
                Expression::CString("python3 example.py".to_string()),
                Expression::CTrue,  // shell=True (needed for python3 command)
                Expression::CTrue,  // capture_output=True
            ],
        );

        let result = eval(function_call, &env);
        assert!(result.is_ok());

        match result.unwrap() {
            ExpressionResult::Value(Expression::CompletedProcess { returncode, stdout, stderr }) => {
                assert_eq!(returncode, 0);
                assert!(stdout.is_some());
                assert!(stderr.is_some());
                let stdout_content = stdout.unwrap();
                
                // Print what we captured to show the test is working
                println!("=== Python Script Output Captured ===");
                println!("Return code: {}", returncode);
                println!("Stdout content:\n{}", stdout_content);
                println!("Stderr content: {:?}", stderr);
                println!("=====================================");
                
                // Verify the expected content
                assert!(stdout_content.contains("Hello from Python!"));
                assert!(stdout_content.contains("This is a test script."));
                assert!(stdout_content.contains("Current working directory:"));
            }
            _ => panic!("Expected CompletedProcess result from Python script execution"),
        }
    }

    #[test]
    fn test_subprocess_run_error_handling_integration() {
        let mut env = Environment::new();
        register_builtins(&mut env);

        // Test various error scenarios through RPython interpreter
        
        // Test invalid argument types (Requirements 4.3, 5.4)
        let function_call = Expression::FuncCall(
            "subprocess.run".to_string(),
            vec![
                Expression::CInt(42), // Invalid command type
            ],
        );

        let result = eval(function_call, &env);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("must be a list of strings or a string"));

        // Test invalid shell argument type
        let function_call = Expression::FuncCall(
            "subprocess.run".to_string(),
            vec![
                Expression::CString("echo test".to_string()),
                Expression::CString("invalid".to_string()), // Invalid shell type
            ],
        );

        let result = eval(function_call, &env);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("shell argument must be a boolean"));

        // Test command not found error propagation (Requirements 4.3, 4.4)
        let function_call = Expression::FuncCall(
            "subprocess.run".to_string(),
            vec![
                Expression::ListValue(vec![
                    Expression::CString("definitely_nonexistent_command_xyz".to_string()),
                ]),
                Expression::CFalse,
                Expression::CFalse,
            ],
        );

        let result = eval(function_call, &env);
        assert!(result.is_ok());
        match result.unwrap() {
            ExpressionResult::Value(Expression::CErr(error)) => {
                match *error {
                    Expression::CString(msg) => {
                        assert!(msg.contains("Command not found"));
                        assert!(msg.contains("definitely_nonexistent_command_xyz"));
                    }
                    _ => panic!("Expected string error message"),
                }
            }
            _ => panic!("Expected CErr result for command not found"),
        }
    }
}