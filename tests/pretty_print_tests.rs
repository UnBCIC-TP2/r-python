// tests/pretty_print_tests.rs

use r_python::ir::ast::Statement;
use r_python::parser::parse;
use r_python::pretty_print::{pretty, ToDoc, concat, hardline, nil, Doc};
use std::rc::Rc;

/// Função auxiliar que converte um Vec<Statement> em um único Doc,
/// com cada statement separado por duas quebras de linha.
fn program_to_doc(program_ast: &[Statement]) -> Rc<Doc> {
    if program_ast.is_empty() {
        return nil();
    }

    program_ast
        .iter()
        .map(|stmt| stmt.to_doc())
        .reduce(|acc, doc| concat(acc, concat(hardline(), concat(hardline(), doc))))
        .unwrap_or_else(nil)
}

#[test]
fn test_assignment_sequence_pretty_print() {
    // 1. Um programa de teste que usa apenas a funcionalidade de atribuição,
    //    que é a única confirmada como funcional pelo parser.
    //    Os comandos devem ser separados por ';'.
    let input_program = "x = 10; y = 20; z = x + y;";

    // 2. O parse agora deve consumir a entrada inteira com sucesso.
    let (remaining, ast) = parse(input_program).unwrap();
    assert!(remaining.trim().is_empty(), "O parser não consumiu toda a entrada.");

    // 3. Gabarito da saída formatada para a sequência de atribuições.
    let expected_output = 
r#"x = 10;

y = 20;

z = x + y;"#;

    // 4. Converte a AST para Doc e renderiza.
    let program_doc = program_to_doc(&ast);
    let formatted_program = pretty(80, &program_doc);

    // 5. Compara a saída com o gabarito.
    assert_eq!(formatted_program, expected_output, "A saída do pretty-printer não corresponde ao esperado.");
}