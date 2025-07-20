#[derive(Debug, Clone, PartialEq)]
pub struct CompletedProcess {
    pub returncode: i32,
    pub stdout: Option<String>,
    pub stderr: Option<String>,
}

#[derive(Debug, Clone)]
pub struct RunOptions {
    pub shell: bool,
    pub capture_output: bool,
}

impl Default for RunOptions {
    fn default() -> Self {
        RunOptions {
            shell: false,
            capture_output: false,
        }
    }
}

/// Comprehensive error types for subprocess operations
#[derive(Debug, Clone, PartialEq)]
pub enum SubprocessError {
    /// Invalid arguments provided to subprocess.run()
    InvalidArguments(String),
    /// Command executable not found in PATH
    CommandNotFound(String),
    /// Permission denied when trying to execute command
    PermissionDenied(String),
    /// General execution failure
    ExecutionFailed(String),
    /// Error capturing command output
    OutputCaptureError(String),
}

impl std::fmt::Display for SubprocessError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SubprocessError::InvalidArguments(msg) => write!(f, "Invalid arguments: {}", msg),
            SubprocessError::CommandNotFound(cmd) => write!(f, "Command not found: {}", cmd),
            SubprocessError::PermissionDenied(cmd) => write!(f, "Permission denied: {}", cmd),
            SubprocessError::ExecutionFailed(msg) => write!(f, "Execution failed: {}", msg),
            SubprocessError::OutputCaptureError(msg) => write!(f, "Output capture error: {}", msg),
        }
    }
}

impl std::error::Error for SubprocessError {}

/// Convert SubprocessError to String for RPython's Result type system
impl From<SubprocessError> for String {
    fn from(error: SubprocessError) -> String {
        error.to_string()
    }
}

/// Convert std::io::Error to SubprocessError with context
impl SubprocessError {
    pub fn from_io_error(error: std::io::Error, command: &str) -> Self {
        match error.kind() {
            std::io::ErrorKind::NotFound => {
                SubprocessError::CommandNotFound(command.to_string())
            }
            std::io::ErrorKind::PermissionDenied => {
                SubprocessError::PermissionDenied(command.to_string())
            }
            _ => {
                SubprocessError::ExecutionFailed(format!("{}: {}", command, error))
            }
        }
    }
}