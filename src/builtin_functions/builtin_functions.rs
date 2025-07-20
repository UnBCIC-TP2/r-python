// src/builtin_functions.rs

use crate::interpreter::interpreter::Value; // Importa o enum Value do seu interpretador
use std::io::{self, Write}; // Para operações de I/O (stdout().flush(), stdin().read_line())

/// Implementa a função built-in 'print()'.
/// Recebe um vetor de argumentos `Value` avaliados e os imprime separados por espaços,
/// seguidos por uma quebra de linha.
pub fn r_print(args: Vec<Value>) {
    for (i, arg) in args.iter().enumerate() {
        print!("{}", arg.to_string()); // Usa a trait ToString que implementamos em Value
        if i < args.len() - 1 {
            print!(" "); // Adiciona um espaço entre os argumentos
        }
    }
    println!(); // Imprime uma nova linha no final
}

/// Implementa a função built-in 'input()'.
/// Opcionalmente, recebe uma string de prompt. Lê uma linha da entrada padrão (stdin).
/// Retorna a linha lida como um `Value::CString`.
pub fn r_input(prompt: Option<String>) -> Result<Value, io::Error> {
    if let Some(p) = prompt {
        print!("{}", p);
        io::stdout().flush()?; // Garante que o prompt seja exibido imediatamente
    }

    let mut buffer = String::new();
    io::stdin().read_line(&mut buffer)?; // Lê a linha do usuário
    // Remove os caracteres de quebra de linha (Windows \r\n, Unix \n)
    Ok(Value::CString(buffer.trim_end_matches(&['\r', '\n'][..]).to_string()))
}
