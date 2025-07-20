use std::io; // Importa std::io::Error para facilitar a conversão de erros de I/O.

/// Enum para representar os diferentes tipos de erros que podem ocorrer
/// durante a execução de subprocessos.
#[derive(Debug, PartialEq)] // Adicione PartialEq para poder comparar em testes
pub enum SubprocessError {
    CommandNotFound(String),
    ExecutionFailed { // O comando executado retornou um código de saída diferente de zero
        command_name: String,
        exit_code: Option<i32>,
        stdout: Option<String>,
        stderr: Option<String>,
    },
    IoError(String), // Outros erros de I/O genéricos
    InvalidArguments(String), // Erros de validação de argumentos para subprocess.run
    PermissionDenied(String), // Erro de permissão negada
    OutputCaptureError(String), // Erro na captura da saída
    Other(String), // Para erros genéricos ou não mapeados.
}

// Implementa a conversão de SubprocessError para String.
impl From<SubprocessError> for String {
    fn from(err: SubprocessError) -> Self {
        match err {
            SubprocessError::CommandNotFound(cmd_info) => format!("Command not found: {}", cmd_info),
            SubprocessError::ExecutionFailed { command_name, exit_code, stdout, stderr } => {
                format!(
                    "Command '{}' failed with exit code {:?}. Stdout: {:?}, Stderr: {:?}",
                    command_name, exit_code, stdout, stderr
                )
            },
            SubprocessError::IoError(msg) => format!("I/O Error: {}", msg),
            SubprocessError::InvalidArguments(msg) => format!("Invalid arguments: {}", msg),
            SubprocessError::PermissionDenied(msg) => format!("Permission denied: {}", msg),
            SubprocessError::OutputCaptureError(msg) => format!("Output capture error: {}", msg),
            SubprocessError::Other(msg) => format!("Subprocess Error: {}", msg),
        }
    }
}

impl SubprocessError {
    /// Converte um std::io::Error para um SubprocessError mais específico.
    pub fn from_io_error(err: io::Error, command_name: &str) -> Self {
        match err.kind() {
            io::ErrorKind::NotFound => SubprocessError::CommandNotFound(command_name.to_string()),
            io::ErrorKind::PermissionDenied => SubprocessError::PermissionDenied(command_name.to_string()),
            _ => SubprocessError::IoError(format!("{}: {}", command_name, err)), // Usar IoError para outros erros
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use std::io::{Error, ErrorKind};

    #[test]
    fn test_subprocess_error_from_io_error_not_found() {
        let io_err = Error::new(ErrorKind::NotFound, "No such file or directory (os error 2)");
        let sub_err = SubprocessError::from_io_error(io_err, "nonexistent_cmd");
        match sub_err {
            SubprocessError::CommandNotFound(cmd) => {
                assert_eq!(cmd, "nonexistent_cmd");
            },
            _ => panic!("Expected CommandNotFound error"),
        }
    }

    #[test]
    fn test_subprocess_error_from_io_error_permission_denied() {
        let io_err = Error::new(ErrorKind::PermissionDenied, "Permission denied (os error 13)");
        let sub_err = SubprocessError::from_io_error(io_err, "restricted_cmd");
        match sub_err {
            SubprocessError::PermissionDenied(cmd) => {
                assert_eq!(cmd, "restricted_cmd");
            },
            _ => panic!("Expected PermissionDenied error"),
        }
    }

    #[test]
    fn test_subprocess_error_from_io_error_other() {
        let io_err = Error::new(ErrorKind::Other, "Some generic IO error");
        let sub_err = SubprocessError::from_io_error(io_err, "some_cmd");
        match sub_err {
            SubprocessError::IoError(msg) => {
                assert!(msg.contains("some_cmd"));
                assert!(msg.contains("Some generic IO error"));
            },
            _ => panic!("Expected IoError"),
        }
    }

    #[test]
    fn test_subprocess_error_to_string_command_not_found() {
        let err = SubprocessError::CommandNotFound("nonexistent_cmd".to_string());
        let msg: String = err.into();
        assert_eq!(msg, "Command not found: nonexistent_cmd");
    }

    #[test]
    fn test_subprocess_error_to_string_execution_failed() {
        let err = SubprocessError::ExecutionFailed {
            command_name: "ls".to_string(),
            exit_code: Some(1),
            stdout: Some("".to_string()),
            stderr: Some("ls: cannot access 'nonexistent': No such file or directory\n".to_string()),
        };
        let msg: String = err.into();
        assert_eq!(msg, "Command 'ls' failed with exit code Some(1). Stdout: Some(\"\"), Stderr: Some(\"ls: cannot access 'nonexistent': No such file or directory\\n\")");
    }

    #[test]
    fn test_subprocess_error_to_string_invalid_arguments() {
        let err = SubprocessError::InvalidArguments("Empty command list".to_string());
        let msg: String = err.into();
        assert_eq!(msg, "Invalid arguments: Empty command list");
    }

    #[test]
    fn test_subprocess_error_to_string_permission_denied() {
        let err = SubprocessError::PermissionDenied("restricted_file".to_string());
        let msg: String = err.into();
        assert_eq!(msg, "Permission denied: restricted_file");
    }

    #[test]
    fn test_subprocess_error_to_string_output_capture_error() {
        let err = SubprocessError::OutputCaptureError("Failed to read output".to_string());
        let msg: String = err.into();
        assert_eq!(msg, "Output capture error: Failed to read output");
    }
}