use std::process::{Command, Stdio};
use super::types::{CompletedProcess, RunOptions, SubprocessError};

/// Convert bytes to string, handling both text and binary output appropriately
fn bytes_to_string(bytes: &[u8]) -> String {
    // Handle empty output
    if bytes.is_empty() {
        return String::new();
    }
    
    // Try UTF-8 first, fall back to lossy conversion for binary data
    match std::str::from_utf8(bytes) {
        Ok(s) => s.to_string(),
        Err(_) => {
            // For binary data, use lossy conversion which replaces invalid UTF-8 sequences
            String::from_utf8_lossy(bytes).to_string()
        }
    }
}

/// Execute a command directly without shell interpretation
pub fn run_command(
    command: Vec<String>, 
    options: RunOptions
) -> Result<CompletedProcess, SubprocessError> {
    if command.is_empty() {
        return Err(SubprocessError::InvalidArguments("Command cannot be empty".to_string()));
    }

    let program = &command[0];
    let args = &command[1..];

    let mut cmd = Command::new(program);
    cmd.args(args);

    // Configure stdio based on capture_output option
    if options.capture_output {
        cmd.stdout(Stdio::piped());
        cmd.stderr(Stdio::piped());
    }

    // Execute the command
    match cmd.output() {
        Ok(output) => {
            // Handle output capture based on options
            let stdout = if options.capture_output {
                Some(bytes_to_string(&output.stdout))
            } else {
                None
            };

            let stderr = if options.capture_output {
                Some(bytes_to_string(&output.stderr))
            } else {
                None
            };

            let returncode = output.status.code().unwrap_or(-1);

            Ok(CompletedProcess {
                returncode,
                stdout,
                stderr,
            })
        }
        Err(e) => {
            Err(SubprocessError::from_io_error(e, program))
        }
    }
}

/// Execute a command through the system shell
pub fn run_shell_command(
    command: String, 
    options: RunOptions
) -> Result<CompletedProcess, SubprocessError> {
    if command.trim().is_empty() {
        return Err(SubprocessError::InvalidArguments("Shell command cannot be empty".to_string()));
    }

    // Determine the shell command based on the operating system
    let (shell_program, shell_arg) = if cfg!(target_os = "windows") {
        ("cmd", "/C")
    } else {
        ("sh", "-c")
    };

    let mut cmd = Command::new(shell_program);
    cmd.arg(shell_arg);
    cmd.arg(&command);

    // Configure stdio based on capture_output option
    if options.capture_output {
        cmd.stdout(Stdio::piped());
        cmd.stderr(Stdio::piped());
    }

    // Execute the command
    match cmd.output() {
        Ok(output) => {
            // Handle output capture based on options
            let stdout = if options.capture_output {
                Some(bytes_to_string(&output.stdout))
            } else {
                None
            };

            let stderr = if options.capture_output {
                Some(bytes_to_string(&output.stderr))
            } else {
                None
            };

            let returncode = output.status.code().unwrap_or(-1);

            Ok(CompletedProcess {
                returncode,
                stdout,
                stderr,
            })
        }
        Err(e) => {
            Err(SubprocessError::from_io_error(e, shell_program))
        }
    }
}

use std::process::{Child, ExitStatus};
use std::io;
#[cfg(not(windows))]
use nix::{
    sys::signal::{self, Signal},
    unistd::Pid,
};

/// Struct representing a running process.
/// It wraps a `std::process::Child` and provides a wait method.
pub struct Processo {
    pub processo: Child,
}

impl Processo {
    /// Waits for the process to finish and returns its exit code.
    /// Returns -1 if the exit code cannot be determined.
    pub fn wait(&mut self) -> io::Result<i32> {
        let status: ExitStatus = self.processo.wait()?;
        Ok(status.code().unwrap_or(-1))
    }
    
    #[cfg(windows)]
    pub fn terminate(&mut self) -> io::Result<()> {
        self.processo.kill()
    }

    #[cfg(not(windows))]
    pub fn terminate(&mut self) -> io::Result<()> {
        let pid = Pid::from_raw(self.processo.id() as i32);
        match signal::kill(pid, Signal::SIGTERM) {
            Ok(_) => Ok(()),
            Err(e) => Err(io::Error::new(io::ErrorKind::Other, e)),
        }
    }

    pub fn kill(&mut self) -> io::Result<()> {
        self.processo.kill()
    }
   

}

use std::process::{ChildStdin, ChildStdout, ChildStderr};

/// Representa um processo em execução com acesso a stdin, stdout e stderr.
pub struct PopenProcess {
    pub child: Child,
    pub stdin: Option<ChildStdin>,
    pub stdout: Option<ChildStdout>,
    pub stderr: Option<ChildStderr>,
}

/// Executa um comando e retorna um processo com streams abertos (estilo popen)
pub fn popen_command(
    command: Vec<String>,
    options: RunOptions,
) -> Result<PopenProcess, SubprocessError> {
    if command.is_empty() {
        return Err(SubprocessError::InvalidArguments("Command cannot be empty".to_string()));
    }

    let program = &command[0];
    let args = &command[1..];

    let mut cmd = Command::new(program);
    cmd.args(args);
    cmd.stdin(Stdio::piped());

    // Redireciona stdout/stderr para pipes conforme solicitado
	if options.capture_output {
        cmd.stdout(Stdio::piped());
        cmd.stderr(Stdio::piped());
    }

    match cmd.spawn() {
        Ok(mut child) => {
            let stdin = child.stdin.take();
            let stdout = if options.capture_output { child.stdout.take() } else { None };
            let stderr = if options.capture_output { child.stderr.take() } else { None };

            Ok(PopenProcess {
                child,
                stdin,
                stdout,
                stderr,
            })
        }
        Err(e) => Err(SubprocessError::from_io_error(e, program)),
    }
}



#[cfg(test)]
mod tests {
    use super::*;
    use std::process::Command;
    use std::thread;
    use std::time::Duration;

    #[test]
    fn test_basic_command_execution() {
        let result = run_command(
            vec!["echo".to_string(), "hello".to_string()],
            RunOptions { shell: false, capture_output: true }
        );
        assert!(result.is_ok());
        let process = result.unwrap();
        assert_eq!(process.returncode, 0);
        assert!(process.stdout.is_some());
        assert!(process.stdout.unwrap().contains("hello"));
    }

    #[test]
    fn test_shell_command_execution() {
        let result = run_shell_command(
            "echo hello".to_string(),
            RunOptions { shell: true, capture_output: true }
        );
        assert!(result.is_ok());
        let process = result.unwrap();
        assert_eq!(process.returncode, 0);
        assert!(process.stdout.is_some());
        assert!(process.stdout.unwrap().contains("hello"));
    }

    #[test]
    fn test_command_not_found() {
        let result = run_command(
            vec!["nonexistent_command_12345".to_string()],
            RunOptions { shell: false, capture_output: false }
        );
        assert!(result.is_err());
        match result.unwrap_err() {
            SubprocessError::CommandNotFound(cmd) => {
                assert_eq!(cmd, "nonexistent_command_12345");
            }
            _ => panic!("Expected CommandNotFound error"),
        }
    }

    #[test]
    fn test_empty_command() {
        let result = run_command(
            vec![],
            RunOptions { shell: false, capture_output: false }
        );
        assert!(result.is_err());
        match result.unwrap_err() {
            SubprocessError::InvalidArguments(msg) => {
                assert!(msg.contains("Command cannot be empty"));
            }
            _ => panic!("Expected InvalidArguments error"),
        }
    }

    #[test]
    fn test_empty_shell_command() {
        let result = run_shell_command(
            "".to_string(),
            RunOptions { shell: true, capture_output: false }
        );
        assert!(result.is_err());
        match result.unwrap_err() {
            SubprocessError::InvalidArguments(msg) => {
                assert!(msg.contains("Shell command cannot be empty"));
            }
            _ => panic!("Expected InvalidArguments error"),
        }
    }

    // Test output capture functionality (Requirements 3.1, 3.2, 3.3)
    #[test]
    fn test_stdout_capture() {
        let result = run_command(
            vec!["echo".to_string(), "test output".to_string()],
            RunOptions { shell: false, capture_output: true }
        );
        assert!(result.is_ok());
        let process = result.unwrap();
        assert_eq!(process.returncode, 0);
        assert!(process.stdout.is_some());
        assert!(process.stderr.is_some());
        assert!(process.stdout.unwrap().contains("test output"));
    }

    #[test]
    fn test_stderr_capture() {
        // Use a command that writes to stderr - ls with invalid directory
        let result = run_command(
            vec!["ls".to_string(), "/nonexistent_directory_12345".to_string()],
            RunOptions { shell: false, capture_output: true }
        );
        assert!(result.is_ok());
        let process = result.unwrap();
        assert_ne!(process.returncode, 0); // Should fail
        assert!(process.stdout.is_some());
        assert!(process.stderr.is_some());
        // stderr should contain error message
        let stderr = process.stderr.unwrap();
        assert!(!stderr.is_empty());
    }

    #[test]
    fn test_no_capture_output() {
        // When capture_output=false, stdout and stderr should be None
        let result = run_command(
            vec!["echo".to_string(), "not captured".to_string()],
            RunOptions { shell: false, capture_output: false }
        );
        assert!(result.is_ok());
        let process = result.unwrap();
        assert_eq!(process.returncode, 0);
        assert!(process.stdout.is_none());
        assert!(process.stderr.is_none());
    }

    #[test]
    fn test_empty_output_capture() {
        // Test command that produces no output
        let result = run_command(
            vec!["true".to_string()], // 'true' command produces no output
            RunOptions { shell: false, capture_output: true }
        );
        assert!(result.is_ok());
        let process = result.unwrap();
        assert_eq!(process.returncode, 0);
        assert!(process.stdout.is_some());
        assert!(process.stderr.is_some());
        assert_eq!(process.stdout.unwrap(), "");
        assert_eq!(process.stderr.unwrap(), "");
    }

    #[test]
    fn test_shell_output_capture() {
        // Test shell command with output capture
        let result = run_shell_command(
            "echo 'shell output'".to_string(),
            RunOptions { shell: true, capture_output: true }
        );
        assert!(result.is_ok());
        let process = result.unwrap();
        assert_eq!(process.returncode, 0);
        assert!(process.stdout.is_some());
        assert!(process.stderr.is_some());
        assert!(process.stdout.unwrap().contains("shell output"));
    }

    #[test]
    fn test_bytes_to_string_helper() {
        // Test the bytes_to_string helper function directly
        assert_eq!(bytes_to_string(b""), "");
        assert_eq!(bytes_to_string(b"hello"), "hello");
        assert_eq!(bytes_to_string(b"hello\n"), "hello\n");
        
        // Test with valid UTF-8
        let utf8_bytes = "Hello, 世界!".as_bytes();
        assert_eq!(bytes_to_string(utf8_bytes), "Hello, 世界!");
        
        // Test with invalid UTF-8 (should use lossy conversion)
        let invalid_utf8 = vec![0xFF, 0xFE, 0x48, 0x65, 0x6C, 0x6C, 0x6F];
        let result = bytes_to_string(&invalid_utf8);
        assert!(!result.is_empty()); // Should produce some output, even if lossy
    }

    #[test]
    fn test_multiline_output_capture() {
        // Test capturing multiline output
        let result = run_shell_command(
            "printf 'line1\\nline2\\nline3'".to_string(),
            RunOptions { shell: true, capture_output: true }
        );
        assert!(result.is_ok());
        let process = result.unwrap();
        assert_eq!(process.returncode, 0);
        assert!(process.stdout.is_some());
        let stdout = process.stdout.unwrap();
        assert!(stdout.contains("line1"));
        assert!(stdout.contains("line2"));
        assert!(stdout.contains("line3"));
    }

    #[test]
    fn test_subprocess_error_display() {
        // Test that SubprocessError Display implementation works correctly
        let error1 = SubprocessError::InvalidArguments("test message".to_string());
        assert_eq!(error1.to_string(), "Invalid arguments: test message");

        let error2 = SubprocessError::CommandNotFound("test_cmd".to_string());
        assert_eq!(error2.to_string(), "Command not found: test_cmd");

        let error3 = SubprocessError::PermissionDenied("test_cmd".to_string());
        assert_eq!(error3.to_string(), "Permission denied: test_cmd");

        let error4 = SubprocessError::ExecutionFailed("test failure".to_string());
        assert_eq!(error4.to_string(), "Execution failed: test failure");

        let error5 = SubprocessError::OutputCaptureError("capture failed".to_string());
        assert_eq!(error5.to_string(), "Output capture error: capture failed");
    }

    #[test]
    fn test_subprocess_error_from_string() {
        // Test that SubprocessError can be converted to String
        let error = SubprocessError::CommandNotFound("test_cmd".to_string());
        let error_string: String = error.into();
        assert_eq!(error_string, "Command not found: test_cmd");
    }

    #[test]
    fn test_subprocess_error_from_io_error() {
        // Test the from_io_error helper function
        let not_found_error = std::io::Error::new(std::io::ErrorKind::NotFound, "file not found");
        let subprocess_error = SubprocessError::from_io_error(not_found_error, "test_command");
        match subprocess_error {
            SubprocessError::CommandNotFound(cmd) => assert_eq!(cmd, "test_command"),
            _ => panic!("Expected CommandNotFound error"),
        }

        let permission_error = std::io::Error::new(std::io::ErrorKind::PermissionDenied, "permission denied");
        let subprocess_error = SubprocessError::from_io_error(permission_error, "test_command");
        match subprocess_error {
            SubprocessError::PermissionDenied(cmd) => assert_eq!(cmd, "test_command"),
            _ => panic!("Expected PermissionDenied error"),
        }

        let other_error = std::io::Error::new(std::io::ErrorKind::Other, "other error");
        let subprocess_error = SubprocessError::from_io_error(other_error, "test_command");
        match subprocess_error {
            SubprocessError::ExecutionFailed(msg) => {
                assert!(msg.contains("test_command"));
                assert!(msg.contains("other error"));
            }
            _ => panic!("Expected ExecutionFailed error"),
        }
    }

    // Additional tests to ensure complete coverage of requirements
    #[test]
    fn test_ls_command_execution() {
        // Test ls command execution (Requirement 1.1, 1.2)
        let result = run_command(
            vec!["ls".to_string(), "-la".to_string(), ".".to_string()],
            RunOptions { shell: false, capture_output: true }
        );
        assert!(result.is_ok());
        let process = result.unwrap();
        assert_eq!(process.returncode, 0);
        assert!(process.stdout.is_some());
        assert!(process.stderr.is_some());
        // Should contain current directory listing
        let stdout = process.stdout.unwrap();
        assert!(!stdout.is_empty());
    }

    #[test]
    fn test_echo_command_variations() {
        // Test various echo command patterns (Requirements 1.1, 1.4, 3.1, 3.2)
        
        // Test echo with multiple arguments
        let result = run_command(
            vec!["echo".to_string(), "hello".to_string(), "world".to_string(), "test".to_string()],
            RunOptions { shell: false, capture_output: true }
        );
        assert!(result.is_ok());
        let process = result.unwrap();
        assert_eq!(process.returncode, 0);
        assert!(process.stdout.is_some());
        let stdout = process.stdout.unwrap();
        assert!(stdout.contains("hello"));
        assert!(stdout.contains("world"));
        assert!(stdout.contains("test"));

        // Test echo with special characters
        let result = run_command(
            vec!["echo".to_string(), "test@#$%^&*()".to_string()],
            RunOptions { shell: false, capture_output: true }
        );
        assert!(result.is_ok());
        let process = result.unwrap();
        assert_eq!(process.returncode, 0);
        assert!(process.stdout.unwrap().contains("test@#$%^&*()"));
    }

    #[test]
    fn test_shell_command_with_pipes() {
        // Test shell command with pipes (Requirements 2.1, 2.2, 3.1)
        let result = run_shell_command(
            "echo 'hello world' | wc -w".to_string(),
            RunOptions { shell: true, capture_output: true }
        );
        assert!(result.is_ok());
        let process = result.unwrap();
        assert_eq!(process.returncode, 0);
        assert!(process.stdout.is_some());
        // Should output "2" (word count)
        let stdout_content = process.stdout.unwrap();
        let stdout = stdout_content.trim();
        assert!(stdout.contains("2"));
    }

    #[test]
    fn test_shell_command_with_environment_variables() {
        // Test shell command with environment variable expansion (Requirement 2.2)
        let result = run_shell_command(
            "echo $HOME".to_string(),
            RunOptions { shell: true, capture_output: true }
        );
        assert!(result.is_ok());
        let process = result.unwrap();
        assert_eq!(process.returncode, 0);
        assert!(process.stdout.is_some());
        // Should contain some path (HOME environment variable)
        let stdout = process.stdout.unwrap();
        assert!(!stdout.trim().is_empty());
        // On most systems, HOME should contain a path separator
        assert!(stdout.contains("/") || stdout.contains("\\"));
    }

    #[test]
    fn test_command_return_codes() {
        // Test that return codes are properly captured (Requirements 1.4, 4.2, 4.4)
        
        // Test successful command (return code 0)
        let result = run_command(
            vec!["true".to_string()],
            RunOptions { shell: false, capture_output: false }
        );
        assert!(result.is_ok());
        let process = result.unwrap();
        assert_eq!(process.returncode, 0);

        // Test failing command (non-zero return code)
        let result = run_command(
            vec!["false".to_string()],
            RunOptions { shell: false, capture_output: false }
        );
        assert!(result.is_ok());
        let process = result.unwrap();
        assert_ne!(process.returncode, 0);
        assert_eq!(process.returncode, 1);
    }

    #[test]
    fn test_comprehensive_output_capture_scenarios() {
        // Test comprehensive output capture scenarios (Requirements 3.1, 3.2, 3.3, 3.4, 3.5)
        
        // Test command that outputs to both stdout and stderr
        let result = run_shell_command(
            "echo 'stdout message' && echo 'stderr message' >&2".to_string(),
            RunOptions { shell: true, capture_output: true }
        );
        assert!(result.is_ok());
        let process = result.unwrap();
        assert_eq!(process.returncode, 0);
        assert!(process.stdout.is_some());
        assert!(process.stderr.is_some());
        assert!(process.stdout.unwrap().contains("stdout message"));
        assert!(process.stderr.unwrap().contains("stderr message"));

        // Test command with large output
        let result = run_shell_command(
            "for i in {1..10}; do echo \"Line $i\"; done".to_string(),
            RunOptions { shell: true, capture_output: true }
        );
        assert!(result.is_ok());
        let process = result.unwrap();
        assert_eq!(process.returncode, 0);
        assert!(process.stdout.is_some());
        let stdout = process.stdout.unwrap();
        assert!(stdout.contains("Line 1"));
        assert!(stdout.contains("Line 10"));
    }

        #[test]
    fn test_wait_success() {
        // Test command exits successfully with code 0
        let mut processo = Processo {
            processo: Command::new("true").spawn().unwrap(),
        };
        let exit_code = processo.wait().unwrap();
        assert_eq!(exit_code, 0);
    }

    #[test]
    fn test_wait_failure() {
        // Test command exits with failure code (usually 1)        
        let mut processo = Processo {
            processo: Command::new("false").spawn().unwrap(),
        };
        let exit_code = processo.wait().unwrap();
        assert_eq!(exit_code, 1);
    }

    #[test]
    fn test_wait_killed() {
        // Test command is killed before finishing; should return -1        
        let mut processo = Processo {
            processo: Command::new("sleep").arg("5").spawn().unwrap(),
        };
        processo.processo.kill().unwrap();
        let exit_code = processo.wait().unwrap();
        assert_eq!(exit_code, -1); // May vary by OS
    }
    // Terminate tests
    fn create_long_running_command() -> Command {
        if cfg!(windows) {
            let mut cmd = Command::new("timeout");
            cmd.arg("/T").arg("30");
            cmd
        } else {
            let mut cmd = Command::new("sleep");
            cmd.arg("30");
            cmd
        }
    }

    #[test]
    fn test_terminate_a_running_process() {
        let child = create_long_running_command()
            .spawn()
            .expect("Falha ao iniciar processo para o teste de terminate");

        let mut processo = Processo { processo: child };

        thread::sleep(Duration::from_millis(100));

        processo.terminate().expect("Falha ao chamar terminate");

        let exit_code = processo.wait().expect("Falha ao esperar pelo processo terminado");

        let expected_code = if cfg!(windows) { 1 } else { -1 };
        assert_eq!(exit_code, expected_code, "O código de saída após terminate não foi o esperado.");
    }

    #[test]
    fn test_kill_a_running_process() {
        let child = create_long_running_command()
            .spawn()
            .expect("Falha ao iniciar processo para o teste de kill");


        let mut processo = Processo { processo: child };

        thread::sleep(Duration::from_millis(100));

        processo.kill().expect("Falha ao chamar kill");


        let exit_code = processo.wait().expect("Falha ao esperar pelo processo morto");

        let expected_code = if cfg!(windows) { 1 } else { -1 };
        assert_eq!(exit_code, expected_code, "O código de saída após kill não foi o esperado.");
    }

    #[test]
    fn test_wait_on_a_process_that_finishes_normally() {
        let mut command = if cfg!(windows) {
            let mut cmd = Command::new("timeout");
            cmd.arg("/T").arg("1");
            cmd
        } else {
            let mut cmd = Command::new("sleep");
            cmd.arg("1");
            cmd
        };
        
        let child = command.spawn().expect("Falha ao iniciar processo curto");
        let mut processo = Processo { processo: child };
        
        let exit_code = processo.wait().expect("Falha ao esperar pelo processo");
        assert_eq!(exit_code, 0);
    }
	
	use std::io::{Read, Write};

	#[test]
	fn test_popen_cat_stdin_stdout() {
		// Comando que apenas reflete a entrada
		let mut process = popen_command(
			vec!["cat".to_string()],
			RunOptions { shell: false, capture_output: true }
		).expect("Falha ao iniciar processo");

		let input = "Mensagem via stdin\nOutra linha\n";
		
		// Escreve no stdin do processo
		if let Some(stdin) = process.stdin.as_mut() {
			stdin.write_all(input.as_bytes()).expect("Falha ao escrever no stdin");
		}

		// Fecha stdin para que o processo finalize (cat só sai quando stdin fecha)
		drop(process.stdin.take());

		// Espera a saída do processo
		let output = process.child.wait_with_output().expect("Falha ao esperar processo");

		// Verifica se a saída é igual à entrada
		let stdout = String::from_utf8_lossy(&output.stdout);
		assert_eq!(stdout, input);

		// stderr deve estar vazio
		assert!(output.stderr.is_empty());
		assert_eq!(output.status.code().unwrap_or(-1), 0);
	}

	#[test]
	fn test_popen_error_output() {
		let mut process = popen_command(
			vec!["ls".to_string(), "/naoexiste".to_string()],
			RunOptions { shell: false, capture_output: true }
		).expect("Falha ao iniciar processo");

		let output = process.child.wait_with_output().unwrap();

		assert_ne!(output.status.code().unwrap_or(-1), 0);
		let stderr = String::from_utf8_lossy(&output.stderr);
		assert!(stderr.contains("No such file") || stderr.contains("não existe"));
	}

}