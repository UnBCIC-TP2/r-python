use std::collections::HashMap;

use crate::ir::ast::{Expression, Name, Type}; // Importa Type do ast

type ErrorMessage = String;

// O ambiente para o verificador de tipos armazena o TIPO das variáveis
type Environment = HashMap<Name, Type>;

pub fn check(exp: Expression, env: &Environment) -> Result<Type, ErrorMessage> {
    match exp {
        Expression::CTrue => Ok(Type::Bool), // Corrigido para Type::Bool
        Expression::CFalse => Ok(Type::Bool), // Corrigido para Type::Bool
        Expression::CInt(_) => Ok(Type::Int), // Corrigido para Type::Int
        Expression::CReal(_) => Ok(Type::Real), // Corrigido para Type::Real
        Expression::CString(_) => Ok(Type::String), // Corrigido para Type::String

        Expression::Add(l, r) => check_bin_arithmetic_expression(*l, *r, env),
        Expression::Sub(l, r) => check_bin_arithmetic_expression(*l, *r, env),
        Expression::Mul(l, r) => check_bin_arithmetic_expression(*l, *r, env),
        Expression::Div(l, r) => check_bin_arithmetic_expression(*l, *r, env),
        Expression::Rmd(l, r) => check_bin_arithmetic_expression(*l, *r, env), // NOVO: Tratamento para Rmd

        Expression::And(l, r) => check_bin_boolean_expression(*l, *r, env),
        Expression::Or(l, r) => check_bin_boolean_expression(*l, *r, env),
        Expression::Not(e) => check_not_expression(*e, env),

        Expression::EQ(l, r) => check_bin_relational_expression(*l, *r, env),
        Expression::GT(l, r) => check_bin_relational_expression(*l, *r, env),
        Expression::LT(l, r) => check_bin_relational_expression(*l, *r, env),
        Expression::GTE(l, r) => check_bin_relational_expression(*l, *r, env),

        Expression::LTE(l, r) => check_bin_relational_expression(*l, *r, env),
        Expression::ReadFile(path) => check_file_read_expression(*path, env),
        Expression::WriteFile(path, content) => check_file_write_expression(*path, *content, env),
        Expression::FileExists(path) => check_file_exists_expression(*path, env),
        Expression::FunctionCall(name, args) => check_function_call(name, args, env),
        Expression::Var(name) => check_variable(name, env),
        _ => Err(String::from("not implemented yet")),

        Expression::LTE(l, r) => check_bin_relational_expression(*l, *r, env), // CORRIGIDO: Deve ser relacional, não booleana

        // NOVO: Tratamento para variáveis
        Expression::Var(name) => match env.get(&name) {
            Some(var_type) => Ok(var_type.clone()),
            None => Err(format!("Erro de Tipo: Variável '{}' não declarada.", name)),
        },

        // NOVO: Tratamento para chamadas de função (print, input)
        Expression::Call(func_name, args) => {
            // Avalia os tipos dos argumentos
            let arg_types: Result<Vec<Type>, ErrorMessage> = args
                .into_iter()
                .map(|arg_expr| check(arg_expr, env))
                .collect();

            let arg_types = arg_types?; // Propaga erro se algum argumento tiver tipo inválido

            match func_name.as_str() {
                "print" => {
                    // 'print' pode aceitar qualquer tipo de argumento e retorna None
                    // Não há uma verificação de tipo estrita para os argumentos de print
                    // Você pode adicionar validações aqui se quiser restringir o que printa.
                    Ok(Type::None)
                },
                "input" => {
                    // 'input' espera 0 ou 1 argumento (o prompt), que deve ser uma String.
                    // Retorna uma String.
                    if arg_types.len() > 1 {
                        Err(String::from("Erro de Tipo: input() espera 0 ou 1 argumento."))
                    } else if arg_types.len() == 1 {
                        if arg_types[0] != Type::String {
                            Err(String::from("Erro de Tipo: O prompt de input() deve ser uma String."))
                        } else {
                            Ok(Type::String)
                        }
                    } else {
                        Ok(Type::String) // input() sem prompt retorna String
                    }
                },
                _ => Err(format!("Erro de Tipo: Função indefinida '{}'.", func_name)),
            }
        },

    }
}

fn check_bin_arithmetic_expression(
    left: Expression,
    right: Expression,
    env: &Environment,
) -> Result<Type, ErrorMessage> {
    let left_type = check(left, env)?;
    let right_type = check(right, env)?;

    match (left_type, right_type) {
        (Type::Int, Type::Int) => Ok(Type::Int), // Corrigido para Type::Int
        (Type::Int, Type::Real) => Ok(Type::Real), // Corrigido para Type::Int, Type::Real
        (Type::Real, Type::Int) => Ok(Type::Real), // Corrigido para Type::Real, Type::Int
        (Type::Real, Type::Real) => Ok(Type::Real), // Corrigido para Type::Real
        _ => Err(String::from("[Erro de Tipo] esperando valores de tipo numérico.")),
    }
}

fn check_bin_boolean_expression(
    left: Expression,
    right: Expression,
    env: &Environment,
) -> Result<Type, ErrorMessage> {
    let left_type = check(left, env)?;
    let right_type = check(right, env)?;

    match (left_type, right_type) {
        (Type::Bool, Type::Bool) => Ok(Type::Bool), // Corrigido para Type::Bool
        _ => Err(String::from("[Erro de Tipo] esperando valores de tipo booleano.")),
    }
}

fn check_not_expression(exp: Expression, env: &Environment) -> Result<Type, ErrorMessage> {
    let exp_type = check(exp, env)?;

    match exp_type {
        Type::Bool => Ok(Type::Bool), // Corrigido para Type::Bool
        _ => Err(String::from("[Erro de Tipo] esperando um valor de tipo booleano.")),
    }
}

fn check_bin_relational_expression(
    left: Expression,
    right: Expression,
    env: &Environment,
) -> Result<Type, ErrorMessage> {
    let left_type = check(left, env)?;
    let right_type = check(right, env)?;

    match (left_type, right_type) {
        (Type::Int, Type::Int) => Ok(Type::Bool), // Corrigido para Type::Int
        (Type::Int, Type::Real) => Ok(Type::Bool), // Corrigido para Type::Int, Type::Real
        (Type::Real, Type::Int) => Ok(Type::Bool), // Corrigido para Type::Real, Type::Int
        (Type::Real, Type::Real) => Ok(Type::Bool), // Corrigido para Type::Real
        _ => Err(String::from("[Erro de Tipo] esperando valores de tipo numérico para comparação.")),
    }
}

fn check_file_read_expression(path: Expression, env: &Environment) -> Result<Type, ErrorMessage> {
    let path_type = check(path, env)?;
    match path_type {
        Type::TString => Ok(Type::TString),
        _ => Err(String::from("[Type Error] read_file expects a string path.")),
    }
}

fn check_file_write_expression(
    path: Expression,
    content: Expression,
    env: &Environment,
) -> Result<Type, ErrorMessage> {
    let path_type = check(path, env)?;
    let content_type = check(content, env)?;
    
    match (path_type, content_type) {
        (Type::TString, Type::TString) => Ok(Type::TBool),
        _ => Err(String::from("[Type Error] write_file expects string path and content.")),
    }
}

fn check_file_exists_expression(path: Expression, env: &Environment) -> Result<Type, ErrorMessage> {
    let path_type = check(path, env)?;
    match path_type {
        Type::TString => Ok(Type::TBool),
        _ => Err(String::from("[Type Error] file_exists expects a string path.")),
    }
}

fn check_function_call(
    _name: String,
    _args: Vec<Expression>,
    _env: &Environment,
) -> Result<Type, ErrorMessage> {
    // TODO: Implement proper function call type checking
    Err(String::from("[Type Error] Function calls not yet implemented."))
}

fn check_variable(name: String, env: &Environment) -> Result<Type, ErrorMessage> {
    match env.get(&name) {
        Some(var_type) => Ok(var_type.clone()),
        None => Err(format!("[Type Error] Variable '{}' not found.", name)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::ir::ast::Expression::*;
    use crate::ir::ast::Type::*; // Importa as variantes de Type corretamente

    // Testes para TList e TTuple removidos, pois essas variantes não existem no Type atual.
    // Você pode adicioná-los de volta quando implementar esses tipos.

    #[test]
    fn check_constant() {
        let env = HashMap::new();
        let c10 = CInt(10);
        assert_eq!(check(c10, &env), Ok(Int)); // Corrigido para Int
    }

    #[test]
    fn check_add_integers() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let c20 = CInt(20);
        let add = Add(Box::new(c10), Box::new(c20));

        assert_eq!(check(add, &env), Ok(Int)); // Corrigido para Int
    }

    #[test]
    fn check_add_reals() {
        let env = HashMap::new();
        let c10 = CReal(10.5);
        let c20 = CReal(20.3);
        let add = Add(Box::new(c10), Box::new(c20));

        assert_eq!(check(add, &env), Ok(Real)); // Corrigido para Real
    }

    #[test]
    fn check_add_real_and_integer() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let c20 = CReal(20.3);
        let add = Add(Box::new(c10), Box::new(c20));

        assert_eq!(check(add, &env), Ok(Real)); // Corrigido para Real
    }

    #[test]
    fn check_add_integer_and_real() {
        let env = HashMap::new();
        let c10 = CReal(10.5);
        let c20 = CInt(20);
        let add = Add(Box::new(c10), Box::new(c20));

        assert_eq!(check(add, &env), Ok(Real)); // Corrigido para Real
    }

    #[test]
    fn check_type_error_arithmetic_expression() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let bool_val = CFalse; // Renomeado para evitar conflito com Type::Bool
        let add = Add(Box::new(c10), Box::new(bool_val));

        assert_eq!(
            check(add, &env),
            Err(String::from("[Erro de Tipo] esperando valores de tipo numérico."))
        );
    }

    #[test]
    fn check_type_error_not_expression() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let not = Not(Box::new(c10));

        assert_eq!(
            check(not, &env),
            Err(String::from("[Erro de Tipo] esperando um valor de tipo booleano."))
        );
    }

    #[test]
    fn check_type_error_and_expression() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let bool_val = CTrue; // Renomeado
        let and = And(Box::new(c10), Box::new(bool_val));

        assert_eq!(
            check(and, &env),
            Err(String::from("[Erro de Tipo] esperando valores de tipo booleano."))
        );
    }

    #[test]
    fn check_type_error_or_expression() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let bool_val = CTrue; // Renomeado
        let or = Or(Box::new(c10), Box::new(bool_val));

        assert_eq!(
            check(or, &env),
            Err(String::from("[Erro de Tipo] esperando valores de tipo booleano."))
        );
    }

    // NOVOS TESTES PARA VERIFICADOR DE TIPOS

    #[test]
    fn check_variable_lookup() {
        let mut env = HashMap::new();
        env.insert(String::from("x"), Int);
        env.insert(String::from("y"), Bool);
        env.insert(String::from("s"), String);

        assert_eq!(check(Var(String::from("x")), &env), Ok(Int));
        assert_eq!(check(Var(String::from("y")), &env), Ok(Bool));
        assert_eq!(check(Var(String::from("s")), &env), Ok(String));
        assert_eq!(
            check(Var(String::from("z")), &env),
            Err(String::from("Erro de Tipo: Variável 'z' não declarada."))
        );
    }

    #[test]
    fn check_print_call() {
        let env = HashMap::new();
        // print() com um inteiro
        let print_int = Call(String::from("print"), vec![CInt(10)]);
        assert_eq!(check(print_int, &env), Ok(None)); // print retorna None

        // print() com um booleano
        let print_bool = Call(String::from("print"), vec![CTrue]);
        assert_eq!(check(print_bool, &env), Ok(None));

        // print() com uma string (se o parser suportar literais de string)
        let print_string = Call(String::from("print"), vec![CString(String::from("hello"))]);
        assert_eq!(check(print_string, &env), Ok(None));

        // print() com múltiplos argumentos
        let print_multi = Call(String::from("print"), vec![CInt(1), CTrue, CString(String::from("test"))]);
        assert_eq!(check(print_multi, &env), Ok(None));
    }

    #[test]
    fn check_input_call() {
        let env = HashMap::new();
        // input() sem argumentos
        let input_no_args = Call(String::from("input"), vec![]);
        assert_eq!(check(input_no_args, &env), Ok(String)); // input retorna String

        // input() com prompt de string
        let input_with_string_prompt = Call(String::from("input"), vec![CString(String::from("Enter name: "))]);
        assert_eq!(check(input_with_string_prompt, &env), Ok(String));

        // input() com prompt de tipo incorreto
        let input_with_int_prompt = Call(String::from("input"), vec![CInt(123)]);
        assert_eq!(
            check(input_with_int_prompt, &env),
            Err(String::from("Erro de Tipo: O prompt de input() deve ser uma String."))
        );

        // input() com muitos argumentos
        let input_too_many_args = Call(String::from("input"), vec![CString(String::from("a")), CString(String::from("b"))]);
        assert_eq!(
            check(input_too_many_args, &env),
            Err(String::from("Erro de Tipo: input() espera 0 ou 1 argumento."))
        );
    }
}
