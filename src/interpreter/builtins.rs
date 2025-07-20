use crate::environment::environment::Environment;
use crate::ir::ast::{Expression, Name};
use crate::stdlib::{run_command, run_shell_command, RunOptions};
use super::expression_eval::ExpressionResult;

/// Represents a built-in function that can be called from RPython
pub type BuiltinFunction = fn(Vec<Expression>, &Environment<Expression>) -> Result<ExpressionResult, String>;

/// Registry of built-in functions
pub struct BuiltinRegistry {
    functions: std::collections::HashMap<Name, BuiltinFunction>,
}

impl BuiltinRegistry {
    /// Create a new empty builtin registry
    pub fn new() -> Self {
        BuiltinRegistry {
            functions: std::collections::HashMap::new(),
        }
    }

    /// Register a built-in function
    pub fn register(&mut self, name: Name, func: BuiltinFunction) {
        self.functions.insert(name, func);
    }

    /// Look up a built-in function by name
    pub fn lookup(&self, name: &Name) -> Option<&BuiltinFunction> {
        self.functions.get(name)
    }
}

/// Global builtin registry instance using std::sync::OnceLock for thread-safe initialization
static BUILTIN_REGISTRY: std::sync::OnceLock<BuiltinRegistry> = std::sync::OnceLock::new();

/// Get the global builtin registry (thread-safe initialization)
fn get_builtin_registry() -> &'static BuiltinRegistry {
    BUILTIN_REGISTRY.get_or_init(|| {
        let mut registry = BuiltinRegistry::new();
        register_subprocess_run(&mut registry);
        registry
    })
}

/// Register all built-in functions with the environment
pub fn register_builtins(_env: &mut Environment<Expression>) {
    // Built-in functions are handled through the global registry
    // No need to populate the environment directly since we check
    // the registry in eval_builtin_function
}

/// Evaluate a built-in function call
pub fn eval_builtin_function(
    name: &Name,
    args: Vec<Expression>,
    env: &Environment<Expression>,
) -> Result<Option<ExpressionResult>, String> {
    let registry = get_builtin_registry();
    
    if let Some(builtin_func) = registry.lookup(name) {
        Ok(Some(builtin_func(args, env)?))
    } else {
        Ok(None)
    }
}

/// Register the subprocess.run built-in function
fn register_subprocess_run(registry: &mut BuiltinRegistry) {
    registry.register("subprocess.run".to_string(), subprocess_run_builtin);
}

/// Implementation of subprocess.run built-in function
fn subprocess_run_builtin(
    args: Vec<Expression>,
    env: &Environment<Expression>,
) -> Result<ExpressionResult, String> {
    // Validate argument count (1-3 arguments expected)
    if args.is_empty() || args.len() > 3 {
        return Err("subprocess.run() takes 1 to 3 arguments".to_string());
    }

    // Evaluate all arguments first
    let mut evaluated_args = Vec::new();
    for arg in args {
        match super::expression_eval::eval(arg, env)? {
            ExpressionResult::Value(expr) => evaluated_args.push(expr),
            ExpressionResult::Propagate(expr) => return Ok(ExpressionResult::Propagate(expr)),
        }
    }

    // Parse the command argument (first argument)
    let command = match &evaluated_args[0] {
        Expression::ListValue(list) => {
            // Command as list of strings
            let mut cmd_vec = Vec::new();
            for item in list {
                match item {
                    Expression::CString(s) => cmd_vec.push(s.clone()),
                    _ => return Err("subprocess.run() command list must contain only strings".to_string()),
                }
            }
            if cmd_vec.is_empty() {
                return Err("subprocess.run() command list cannot be empty".to_string());
            }
            cmd_vec
        }
        Expression::CString(s) => {
            // Single string command (will be used with shell=True)
            vec![s.clone()]
        }
        _ => return Err("subprocess.run() first argument must be a list of strings or a string".to_string()),
    };

    // Parse optional arguments (shell and capture_output)
    let mut options = RunOptions::default();
    
    // Second argument: shell (optional, default False)
    if evaluated_args.len() > 1 {
        match &evaluated_args[1] {
            Expression::CTrue => options.shell = true,
            Expression::CFalse => options.shell = false,
            _ => return Err("subprocess.run() shell argument must be a boolean".to_string()),
        }
    }

    // Third argument: capture_output (optional, default False)
    if evaluated_args.len() > 2 {
        match &evaluated_args[2] {
            Expression::CTrue => options.capture_output = true,
            Expression::CFalse => options.capture_output = false,
            _ => return Err("subprocess.run() capture_output argument must be a boolean".to_string()),
        }
    }

    // Execute the command based on shell option
    let result = if options.shell && command.len() == 1 {
        // Shell mode with single string command
        run_shell_command(command[0].clone(), options)
    } else if !options.shell {
        // Direct command execution
        run_command(command, options)
    } else {
        // Shell mode with command list - use first element as shell command
        run_shell_command(command[0].clone(), options)
    };

    // Convert result to RPython Expression
    match result {
        Ok(completed_process) => {
            Ok(ExpressionResult::Value(Expression::CompletedProcess {
                returncode: completed_process.returncode,
                stdout: completed_process.stdout,
                stderr: completed_process.stderr,
            }))
        }
        Err(subprocess_error) => {
            // Convert SubprocessError to String using the From implementation
            let error_msg: String = subprocess_error.into();
            // Return error as a Result type (CErr)
            Ok(ExpressionResult::Value(Expression::CErr(Box::new(
                Expression::CString(error_msg)
            ))))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::environment::environment::Environment;

    fn create_test_env() -> Environment<Expression> {
        let mut env = Environment::new();
        register_builtins(&mut env);
        env
    }

    #[test]
    fn test_builtin_registry_creation() {
        let registry = get_builtin_registry();
        assert!(registry.lookup(&"subprocess.run".to_string()).is_some());
    }

    #[test]
    fn test_eval_builtin_function_exists() {
        let env = create_test_env();
        let args = vec![
            Expression::ListValue(vec![
                Expression::CString("echo".to_string()),
                Expression::CString("test".to_string()),
            ]),
            Expression::CFalse, // shell=False
            Expression::CTrue,  // capture_output=True
        ];

        let result = eval_builtin_function(&"subprocess.run".to_string(), args, &env);
        assert!(result.is_ok());
        assert!(result.unwrap().is_some());
    }

    #[test]
    fn test_eval_builtin_function_not_exists() {
        let env = create_test_env();
        let args = vec![];

        let result = eval_builtin_function(&"nonexistent.function".to_string(), args, &env);
        assert!(result.is_ok());
        assert!(result.unwrap().is_none());
    }

    #[test]
    fn test_subprocess_run_basic_command() {
        let env = create_test_env();
        let args = vec![
            Expression::ListValue(vec![
                Expression::CString("echo".to_string()),
                Expression::CString("hello".to_string()),
            ]),
            Expression::CFalse, // shell=False
            Expression::CTrue,  // capture_output=True
        ];

        let result = subprocess_run_builtin(args, &env);
        assert!(result.is_ok());
        
        match result.unwrap() {
            ExpressionResult::Value(Expression::CompletedProcess { returncode, stdout, stderr }) => {
                assert_eq!(returncode, 0);
                assert!(stdout.is_some());
                assert!(stderr.is_some());
                assert!(stdout.unwrap().contains("hello"));
            }
            _ => panic!("Expected CompletedProcess result"),
        }
    }

    #[test]
    fn test_subprocess_run_shell_command() {
        let env = create_test_env();
        let args = vec![
            Expression::CString("echo shell_test".to_string()),
            Expression::CTrue,  // shell=True
            Expression::CTrue,  // capture_output=True
        ];

        let result = subprocess_run_builtin(args, &env);
        assert!(result.is_ok());
        
        match result.unwrap() {
            ExpressionResult::Value(Expression::CompletedProcess { returncode, stdout, stderr }) => {
                assert_eq!(returncode, 0);
                assert!(stdout.is_some());
                assert!(stderr.is_some());
                assert!(stdout.unwrap().contains("shell_test"));
            }
            _ => panic!("Expected CompletedProcess result"),
        }
    }

    #[test]
    fn test_subprocess_run_invalid_arguments() {
        let env = create_test_env();
        
        // Test with no arguments
        let result = subprocess_run_builtin(vec![], &env);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("takes 1 to 3 arguments"));

        // Test with too many arguments
        let result = subprocess_run_builtin(vec![
            Expression::CString("echo".to_string()),
            Expression::CFalse,
            Expression::CFalse,
            Expression::CFalse, // 4th argument - too many
        ], &env);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("takes 1 to 3 arguments"));
    }

    #[test]
    fn test_subprocess_run_invalid_command_type() {
        let env = create_test_env();
        let args = vec![
            Expression::CInt(42), // Invalid command type
        ];

        let result = subprocess_run_builtin(args, &env);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("must be a list of strings or a string"));
    }

    #[test]
    fn test_subprocess_run_invalid_shell_argument() {
        let env = create_test_env();
        let args = vec![
            Expression::CString("echo test".to_string()),
            Expression::CInt(1), // Invalid shell argument type
        ];

        let result = subprocess_run_builtin(args, &env);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("shell argument must be a boolean"));
    }

    #[test]
    fn test_subprocess_run_invalid_capture_output_argument() {
        let env = create_test_env();
        let args = vec![
            Expression::CString("echo test".to_string()),
            Expression::CFalse,
            Expression::CString("invalid".to_string()), // Invalid capture_output argument type
        ];

        let result = subprocess_run_builtin(args, &env);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("capture_output argument must be a boolean"));
    }

    #[test]
    fn test_subprocess_run_command_not_found() {
        let env = create_test_env();
        let args = vec![
            Expression::ListValue(vec![
                Expression::CString("nonexistent_command_12345".to_string()),
            ]),
            Expression::CFalse, // shell=False
            Expression::CFalse,  // capture_output=False
        ];

        let result = subprocess_run_builtin(args, &env);
        assert!(result.is_ok());
        
        // Should return an error wrapped in CErr
        match result.unwrap() {
            ExpressionResult::Value(Expression::CErr(error)) => {
                match *error {
                    Expression::CString(msg) => {
                        assert!(msg.contains("Command not found"));
                        assert!(msg.contains("nonexistent_command_12345"));
                    }
                    _ => panic!("Expected string error message"),
                }
            }
            _ => panic!("Expected CErr result for command not found"),
        }
    }

    #[test]
    fn test_subprocess_error_integration_with_rpython_result_system() {
        let env = create_test_env();
        
        // Test InvalidArguments error
        let result = subprocess_run_builtin(vec![], &env);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("takes 1 to 3 arguments"));

        // Test CommandNotFound error converted to CErr
        let args = vec![
            Expression::ListValue(vec![
                Expression::CString("nonexistent_cmd_xyz".to_string()),
            ]),
        ];
        let result = subprocess_run_builtin(args, &env);
        assert!(result.is_ok());
        match result.unwrap() {
            ExpressionResult::Value(Expression::CErr(error)) => {
                match *error {
                    Expression::CString(msg) => {
                        assert!(msg.starts_with("Command not found:"));
                        assert!(msg.contains("nonexistent_cmd_xyz"));
                    }
                    _ => panic!("Expected string error message"),
                }
            }
            _ => panic!("Expected CErr result"),
        }

        // Test InvalidArguments error for empty command list
        let args = vec![
            Expression::ListValue(vec![]), // Empty command list
        ];
        let result = subprocess_run_builtin(args, &env);
        // This should return an error at the builtin level because empty command list
        // is caught during argument validation
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("command list cannot be empty"));
    }
}