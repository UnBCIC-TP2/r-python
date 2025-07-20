use std::collections::HashMap;
use std::fs;
use std::path::Path;
use crate::ir::ast::{Expression, Name, Statement};
use crate::builtin_functions::builtin_functions;
type ErrorMessage = String;

// Este é o enum que representa os VALORES em tempo de execução.
// Ele NÃO deve conter variantes que são nós da Árvore de Sintaxe Abstrata (Expression).
#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    CInt(i32),
    CReal(f64),
    CTrue,
    CFalse,
    CString(String),
    CNone,
}

// Implementação de `ToString` para que `Value` possa ser convertido em String facilmente para `print()`.
impl ToString for Value {
    fn to_string(&self) -> String {
        match self {
            Value::CInt(i) => i.to_string(),
            Value::CReal(f) => f.to_string(),
            Value::CTrue => "True".to_string(),
            Value::CFalse => "False".to_string(),
            Value::CString(s) => s.clone(),
            Value::CNone => "None".to_string(),
        }
    }
}

// O ambiente (onde as variáveis são armazenadas) agora mapeia Nomes para Valores.
type Environment = HashMap<Name, Value>;

pub fn eval(exp: Expression, env: &Environment) -> Result<Value, ErrorMessage> {
    match exp {
        // Operações Aritméticas
        Expression::Add(lhs, rhs) => add(*lhs, *rhs, env),
        Expression::Sub(lhs, rhs) => sub(*lhs, *rhs, env),
        Expression::Mul(lhs, rhs) => mul(*lhs, *rhs, env),
        Expression::Div(lhs, rhs) => div(*lhs, *rhs, env),
        Expression::Rmd(lhs, rhs) => rmd(*lhs, *rhs, env),

        // Operações Lógicas (Booleanas)
        Expression::And(lhs, rhs) => and(*lhs, *rhs, env),
        Expression::Or(lhs, rhs) => or(*lhs, *rhs, env),
        Expression::Not(lhs) => not(*lhs, env),

        // Operações Relacionais
        Expression::EQ(lhs, rhs) => eq(*lhs, *rhs, env),
        Expression::GT(lhs, rhs) => gt(*lhs, *rhs, env),
        Expression::LT(lhs, rhs) => lt(*lhs, *rhs, env),
        Expression::GTE(lhs, rhs) => gte(*lhs, *rhs, env),
        Expression::LTE(lhs, rhs) => lte(*lhs, *rhs, env),
        Expression::ReadFile(path) => read_file(*path, env),
        Expression::WriteFile(path, content) => write_file(*path, *content, env),
        Expression::FileExists(path) => file_exists(*path, env),
        Expression::FunctionCall(name, args) => function_call(name, args, env),
        Expression::Var(name) => lookup(name, env),

        // Lidar com Chamadas de Função (print, input, etc.)
        Expression::Call(func_name, args) => {
            let evaluated_args: Result<Vec<Value>, ErrorMessage> = args
                .into_iter()
                .map(|arg_expr| eval(arg_expr, env))
                .collect();

            let evaluated_args = evaluated_args?;

            match func_name.as_str() {
                "print" => {
                    builtin_functions::r_print(evaluated_args);
                    Ok(Value::CNone)
                },
                "input" => {
                    let prompt = if evaluated_args.is_empty() {
                        None
                    } else if evaluated_args.len() == 1 {
                        Some(evaluated_args[0].to_string())
                    } else {
                        return Err(String::from("input() espera 0 ou 1 argumento."));
                    };
                    builtin_functions::r_input(prompt).map_err(|e| format!("Erro de entrada: {}", e))
                },
                _ => Err(format!("Função indefinida: {}", func_name)),
            }
        },

        // Constantes (valores literais da AST são mapeados para os tipos Value)
        Expression::CInt(i) => Ok(Value::CInt(i)),
        Expression::CReal(f) => Ok(Value::CReal(f)),
        Expression::CTrue => Ok(Value::CTrue),
        Expression::CFalse => Ok(Value::CFalse),
        Expression::CString(s) => Ok(Value::CString(s)),
    }
}

fn lookup(name: String, env: &Environment) -> Result<Value, ErrorMessage> {
    match env.get(&name) {
        Some(value) => Ok(value.clone()),
        None => Err(format!("Variável {} não encontrada", name)),
    }
}

/* Arithmetic Operations */
fn eval_binary_arith_op<F>(
    lhs: Expression,
    rhs: Expression,
    env: &Environment,
    op: F,
    error_msg: &str,
) -> Result<Value, ErrorMessage>
where
    F: Fn(f64, f64) -> f64,
{
    let v1 = eval(lhs, env)?;
    let v2 = eval(rhs, env)?;
    match (v1, v2) {
        (Value::CInt(v1), Value::CInt(v2)) => {
            Ok(Value::CInt(op(v1 as f64, v2 as f64) as i32))
        }
        (Value::CInt(v1), Value::CReal(v2)) => Ok(Value::CReal(op(v1 as f64, v2))),
        (Value::CReal(v1), Value::CInt(v2)) => Ok(Value::CReal(op(v1, v2 as f64))),
        (Value::CReal(v1), Value::CReal(v2)) => Ok(Value::CReal(op(v1, v2))),
        _ => Err(error_msg.to_string()),
    }
}

fn add(lhs: Expression, rhs: Expression, env: &Environment) -> Result<Value, ErrorMessage> {
    eval_binary_arith_op(
        lhs,
        rhs,
        env,
        |a, b| a + b,
        "adição '(+)' é definida apenas para números (inteiros e reais).",
    )
}

fn sub(lhs: Expression, rhs: Expression, env: &Environment) -> Result<Value, ErrorMessage> {
    eval_binary_arith_op(
        lhs,
        rhs,
        env,
        |a, b| a - b,
        "subtração '(-)' é definida apenas para números (inteiros e reais).",
    )
}

fn mul(lhs: Expression, rhs: Expression, env: &Environment) -> Result<Value, ErrorMessage> {
    eval_binary_arith_op(
        lhs,
        rhs,
        env,
        |a, b| a * b,
        "multiplicação '(*)' é definida apenas para números (inteiros e reais).",
    )
}

fn div(lhs: Expression, rhs: Expression, env: &Environment) -> Result<Value, ErrorMessage> {
    eval_binary_arith_op(
        lhs,
        rhs,
        env,
        |a, b| a / b,
        "divisão '(/)' é definida apenas para números (inteiros e reais).",
    )
}

fn rmd(lhs: Expression, rhs: Expression, env: &Environment) -> Result<Value, ErrorMessage> {
    eval_binary_arith_op(
        lhs,
        rhs,
        env,
        |a, b| a % b,
        "operação de resto '(%)' é definida apenas para números (inteiros e reais).",
    )
}
/* Boolean Expressions */
fn eval_binary_boolean_op<F>(
    lhs: Expression,
    rhs: Expression,
    env: &Environment,
    op: F,
    error_msg: &str,
) -> Result<Value, ErrorMessage>
where
    F: Fn(bool, bool) -> Value,
{
    let v1 = eval(lhs, env)?;
    let v2 = eval(rhs, env)?;
    match (v1, v2) {
        (Value::CTrue, Value::CTrue) => Ok(op(true, true)),
        (Value::CTrue, Value::CFalse) => Ok(op(true, false)),
        (Value::CFalse, Value::CTrue) => Ok(op(false, true)),
        (Value::CFalse, Value::CFalse) => Ok(op(false, false)),
        _ => Err(error_msg.to_string()),
    }
}

fn and(lhs: Expression, rhs: Expression, env: &Environment) -> Result<Value, ErrorMessage> {
    eval_binary_boolean_op(
        lhs,
        rhs,
        env,
        |a, b| {
            if a && b {
                Value::CTrue
            } else {
                Value::CFalse
            }
        },
        "'and' é definido apenas para booleanos.",
    )
}

fn or(lhs: Expression, rhs: Expression, env: &Environment) -> Result<Value, ErrorMessage> {
    eval_binary_boolean_op(
        lhs,
        rhs,
        env,
        |a, b| {
            if a || b {
                Value::CTrue
            } else {
                Value::CFalse
            }
        },
        "'or' é definido apenas para booleanos.",
    )
}

fn not(lhs: Expression, env: &Environment) -> Result<Value, ErrorMessage> {
    let v = eval(lhs, env)?;
    match v {
        Value::CTrue => Ok(Value::CFalse),
        Value::CFalse => Ok(Value::CTrue),
        _ => Err(String::from("'not' é definido apenas para booleanos.")),
    }
}

/* Relational Operations */
fn eval_binary_rel_op<F>(
    lhs: Expression,
    rhs: Expression,
    env: &Environment,
    op: F,
    error_msg: &str,
) -> Result<Value, ErrorMessage>
where
    F: Fn(f64, f64) -> Value,
{
    let v1 = eval(lhs, env)?;
    let v2 = eval(rhs, env)?;
    match (v1, v2) {
        (Value::CInt(v1), Value::CInt(v2)) => Ok(op(v1 as f64, v2 as f64)),
        (Value::CInt(v1), Value::CReal(v2)) => Ok(op(v1 as f64, v2)),
        (Value::CReal(v1), Value::CInt(v2)) => Ok(op(v1, v2 as f64)),
        (Value::CReal(v1), Value::CReal(v2)) => Ok(op(v1, v2)),
        _ => Err(error_msg.to_string()),
    }
}

fn eq(lhs: Expression, rhs: Expression, env: &Environment) -> Result<Value, ErrorMessage> {
    eval_binary_rel_op(
        lhs,
        rhs,
        env,
        |a, b| {
            if a == b {
                Value::CTrue
            } else {
                Value::CFalse
            }
        },
        "(==) é definido apenas para números (inteiros e reais).",
    )
}

fn gt(lhs: Expression, rhs: Expression, env: &Environment) -> Result<Value, ErrorMessage> {
    eval_binary_rel_op(
        lhs,
        rhs,
        env,
        |a, b| {
            if a > b {
                Value::CTrue
            } else {
                Value::CFalse
            }
        },
        "(>) é definido apenas para números (inteiros e reais).",
    )
}

fn lt(lhs: Expression, rhs: Expression, env: &Environment) -> Result<Value, ErrorMessage> {
    eval_binary_rel_op(
        lhs,
        rhs,
        env,
        |a, b| {
            if a < b {
                Value::CTrue
            } else {
                Value::CFalse
            }
        },
        "(<) é definido apenas para números (inteiros e reais).",
    )
}

fn gte(lhs: Expression, rhs: Expression, env: &Environment) -> Result<Value, ErrorMessage> {
    eval_binary_rel_op(
        lhs,
        rhs,
        env,
        |a, b| {
            if a >= b {
                Value::CTrue
            } else {
                Value::CFalse
            }
        },
        "(>=) é definido apenas para números (inteiros e reais).",
    )
}

fn lte(lhs: Expression, rhs: Expression, env: &Environment) -> Result<Value, ErrorMessage> {
    eval_binary_rel_op(
        lhs,
        rhs,
        env,
        |a, b| {
            if a <= b {
                Value::CTrue
            } else {
                Value::CFalse
            }
        },
        "(<=) é definido apenas para números (inteiros e reais).",
    )
}

pub fn execute(stmt: Statement, env: Environment) -> Result<Environment, ErrorMessage> {
    match stmt {
        Statement::Assignment(name, exp) => {
            let value = eval(*exp, &env)?;
            let mut new_env = env;
            new_env.insert(name.clone(), value);
            Ok(new_env.clone())
        }
        Statement::IfThenElse(cond, stmt_then, stmt_else) => {
            let value = eval(*cond, &env)?;
            match value {
                Value::CTrue => execute(*stmt_then, env),
                Value::CFalse => match stmt_else {
                    Some(else_statement) => execute(*else_statement, env),
                    None => Ok(env),
                },
                _ => Err(String::from("Condição 'if/else' espera um valor booleano.")),
            }
        }
        Statement::Block(statements) => {
            let mut current_env = env;
            for s in statements {
                current_env = execute(s, current_env)?;
            }
            Ok(current_env)
        }
        Statement::While(cond, stmt_body) => {
            let mut current_env = env;
            let mut condition_value = eval(cond.as_ref().clone(), &current_env)?;

            while condition_value == Value::CTrue {
                current_env = execute(stmt_body.as_ref().clone(), current_env)?;
                condition_value = eval(cond.as_ref().clone(), &current_env)?;
            }
            Ok(current_env)
        }
        Statement::Sequence(s1, s2) => execute(*s1, env).and_then(|new_env| execute(*s2, new_env)),
        Statement::VarDeclaration(name) => {
            let mut new_env = env;
            new_env.insert(name, Value::CNone);
            Ok(new_env)
        }
        Statement::ValDeclaration(name) => {
            let mut new_env = env;
            new_env.insert(name, Value::CNone);
            Ok(new_env)
        }
        Statement::ExpressionStmt(exp) => {
            // CORRIGIDO: `exp` já é um Expression, não um Box<Expression>, então não precisa de *
            eval(exp, &env)?;
            Ok(env)
        },
    }
}

/* File Operations */
fn read_file(path_expr: Expression, env: &Environment) -> Result<Expression, ErrorMessage> {
    let path_value = eval(path_expr, env)?;
    match path_value {
        Expression::CString(path) => {
            match fs::read_to_string(&path) {
                Ok(content) => Ok(Expression::CString(content)),
                Err(e) => Err(format!("Failed to read file '{}': {}", path, e)),
            }
        }
        _ => Err(String::from("read_file expects a string path.")),
    }
}

fn write_file(
    path_expr: Expression,
    content_expr: Expression,
    env: &Environment,
) -> Result<Expression, ErrorMessage> {
    let path_value = eval(path_expr, env)?;
    let content_value = eval(content_expr, env)?;
    
    match (path_value, content_value) {
        (Expression::CString(path), Expression::CString(content)) => {
            match fs::write(&path, &content) {
                Ok(_) => Ok(Expression::CTrue),
                Err(e) => {
                    eprintln!("Warning: Failed to write file '{}': {}", path, e);
                    Ok(Expression::CFalse)
                }
            }
        }
        _ => Err(String::from("write_file expects string path and content.")),
    }
}

fn file_exists(path_expr: Expression, env: &Environment) -> Result<Expression, ErrorMessage> {
    let path_value = eval(path_expr, env)?;
    match path_value {
        Expression::CString(path) => {
            if Path::new(&path).exists() {
                Ok(Expression::CTrue)
            } else {
                Ok(Expression::CFalse)
            }
        }
        _ => Err(String::from("file_exists expects a string path.")),
    }
}

fn function_call(
    name: String,
    args: Vec<Expression>,
    env: &Environment,
) -> Result<Expression, ErrorMessage> {
    match name.as_str() {
        "read_file" => {
            if args.len() != 1 {
                return Err(String::from("read_file expects exactly 1 argument."));
            }
            read_file(args[0].clone(), env)
        }
        "write_file" => {
            if args.len() != 2 {
                return Err(String::from("write_file expects exactly 2 arguments."));
            }
            write_file(args[0].clone(), args[1].clone(), env)
        }
        "file_exists" => {
            if args.len() != 1 {
                return Err(String::from("file_exists expects exactly 1 argument."));
            }
            file_exists(args[0].clone(), env)
        }
        _ => Err(format!("Unknown function: {}", name)),
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::interpreter::interpreter::Value::*;
    use crate::ir::ast::Expression;
    use crate::ir::ast::Statement::*;
    use crate::parser::parser::parse_sequence_statement;

    use approx::relative_eq;

    #[test]
    fn eval_constant() {
        let env = HashMap::new();
        let c10 = Expression::CInt(10);
        let c20 = Expression::CInt(20);

        assert_eq!(eval(c10, &env), Ok(CInt(10)));
        assert_eq!(eval(c20, &env), Ok(CInt(20)));
    }

    #[test]
    fn eval_add_expression1() {
        let env = HashMap::new();
        let c10 = Expression::CInt(10);
        let c20 = Expression::CInt(20);
        let add1 = Expression::Add(Box::new(c10), Box::new(c20));
        assert_eq!(eval(add1, &env), Ok(CInt(30)));
    }

    #[test]
    fn eval_add_expression2() {
        let env = HashMap::new();
        let c10 = Expression::CInt(10);
        let c20 = Expression::CInt(20);
        let c30 = Expression::CInt(30);
        let add1 = Expression::Add(Box::new(c10), Box::new(c20));
        let add2 = Expression::Add(Box::new(add1), Box::new(c30));
        assert_eq!(eval(add2, &env), Ok(CInt(60)));
    }

    #[test]
    fn eval_add_expression3() {
        let env = HashMap::new();
        let c10 = Expression::CInt(10);
        let c20 = Expression::CReal(20.5);
        let add1 = Expression::Add(Box::new(c10), Box::new(c20));
        assert_eq!(eval(add1, &env), Ok(CReal(30.5)));
    }

    #[test]
    fn eval_sub_expression1() {
        let env = HashMap::new();
        let c10 = Expression::CInt(10);
        let c20 = Expression::CInt(20);
        let sub1 = Expression::Sub(Box::new(c20), Box::new(c10));
        assert_eq!(eval(sub1, &env), Ok(CInt(10)));
    }

    #[test]
    fn eval_sub_expression2() {
        let env = HashMap::new();
        let c100 = Expression::CInt(100);
        let c200 = Expression::CInt(300);
        let sub1 = Expression::Sub(Box::new(c200), Box::new(c100));
        assert_eq!(eval(sub1, &env), Ok(CInt(200)));
    }

    #[test]
    fn eval_sub_expression3() {
        let env = HashMap::new();
        let c100 = Expression::CReal(100.5);
        let c300 = Expression::CInt(300);
        let sub1 = Expression::Sub(Box::new(c300), Box::new(c100));
        assert_eq!(eval(sub1, &env), Ok(CReal(199.5)));
    }

    #[test]
    fn eval_mul_expression1() {
        let env = HashMap::new();
        let c10 = Expression::CInt(10);
        let c20 = Expression::CInt(20);
        let mul1 = Expression::Mul(Box::new(c10), Box::new(c20));
        assert_eq!(eval(mul1, &env), Ok(CInt(200)));
    }

    #[test]
    fn eval_mul_expression2() {
        let env = HashMap::new();
        let c10 = Expression::CReal(10.5);
        let c20 = Expression::CInt(20);
        let mul1 = Expression::Mul(Box::new(c10), Box::new(c20));
        assert_eq!(eval(mul1, &env), Ok(CReal(210.0)));
    }

    #[test]
    fn eval_div_expression1() {
        let env = HashMap::new();
        let c10 = Expression::CInt(10);
        let c20 = Expression::CInt(20);
        let div1 = Expression::Div(Box::new(c20), Box::new(c10));
        assert_eq!(eval(div1, &env), Ok(CInt(2)));
    }

    #[test]
    fn eval_div_expression2() {
        let env = HashMap::new();
        let c10 = Expression::CInt(10);
        let c3 = Expression::CInt(3);
        let div1 = Expression::Div(Box::new(c10), Box::new(c3));
        assert_eq!(eval(div1, &env), Ok(CInt(3)));
    }

    #[test]
    fn eval_div_expression3() {
        let env = HashMap::new();
        let c3 = Expression::CInt(3);
        let c21 = Expression::CInt(21);
        let div1 = Expression::Div(Box::new(c21), Box::new(c3));
        assert_eq!(eval(div1, &env), Ok(CInt(7)));
    }

    #[test]
    fn eval_div_expression4() {
        let env = HashMap::new();
        let c10 = Expression::CInt(10);
        let c3 = Expression::CReal(3.0);
        let div1 = Expression::Div(Box::new(c10), Box::new(c3));
        let res = eval(div1, &env);
        match res {
            Ok(CReal(v)) => assert!(relative_eq!(v, 3.3333333333333335, epsilon = f64::EPSILON)),
            Err(msg) => assert!(false, "{}", msg),
            _ => assert!(false, "Não esperado."),
        }
    }

    #[test]
    fn eval_variable() {
        let env = HashMap::from([(String::from("x"), CInt(10)), (String::from("y"), CInt(20))]);
        let v1 = Expression::Var(String::from("x"));
        let v2 = Expression::Var(String::from("y"));
        assert_eq!(eval(v1, &env), Ok(CInt(10)));
        assert_eq!(eval(v2, &env), Ok(CInt(20)));
    }

    #[test]
    fn eval_expression_with_variables() {
        let env = HashMap::from([(String::from("a"), CInt(5)), (String::from("b"), CInt(3))]);
        let expr = Expression::Mul(
            Box::new(Expression::Var(String::from("a"))),
            Box::new(Expression::Add(Box::new(Expression::Var(String::from("b"))), Box::new(Expression::CInt(2)))),
        );
        assert_eq!(eval(expr, &env), Ok(CInt(25)));
    }

    #[test]
    fn eval_nested_expressions() {
        let env = HashMap::new();
        let expr = Expression::Add(
            Box::new(Expression::Mul(Box::new(Expression::CInt(2)), Box::new(Expression::CInt(3)))),
            Box::new(Expression::Sub(Box::new(Expression::CInt(10)), Box::new(Expression::CInt(4)))),
        );
        assert_eq!(eval(expr, &env), Ok(CInt(12)));
    }

    #[test]
    fn eval_variable_not_found() {
        let env = HashMap::new();
        let var_expr = Expression::Var(String::from("z"));

        assert_eq!(
            eval(var_expr, &env),
            Err(String::from("Variável z não encontrada"))
        );
    }

    #[test]
    fn execute_assignment() {
        let env = HashMap::new();
        let assign_stmt = Assignment(String::from("x"), Box::new(Expression::CInt(42)));

        match execute(assign_stmt, env) {
            Ok(new_env) => assert_eq!(new_env.get("x"), Some(&CInt(42))),
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn eval_summation() {
        let env = HashMap::new();

        let a1 = Assignment(String::from("x"), Box::new(Expression::CInt(10)));
        let a2 = Assignment(String::from("y"), Box::new(Expression::CInt(0)));
        let a3 = Assignment(
            String::from("y"),
            Box::new(Expression::Add(
                Box::new(Expression::Var(String::from("y"))),
                Box::new(Expression::Var(String::from("x"))),
            )),
        );
        let a4 = Assignment(
            String::from("x"),
            Box::new(Expression::Sub(Box::new(Expression::Var(String::from("x"))), Box::new(Expression::CInt(1)))),
        );

        let seq1 = Sequence(Box::new(a3), Box::new(a4));

        let while_statement = While(
            Box::new(Expression::GT(Box::new(Expression::Var(String::from("x"))), Box::new(Expression::CInt(0)))),
            Box::new(seq1),
        );

        let seq2 = Sequence(Box::new(a2), Box::new(while_statement));
        let program = Sequence(Box::new(a1), Box::new(seq2));

        match execute(program, env) {
            Ok(new_env) => {
                assert_eq!(new_env.get("y"), Some(&CInt(55)));
                assert_eq!(new_env.get("x"), Some(&CInt(0)));
            }
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn eval_simple_if_then_else() {
        let env = HashMap::new();

        let condition = Expression::GT(Box::new(Expression::Var(String::from("x"))), Box::new(Expression::CInt(5)));
        let then_stmt = Assignment(String::from("y"), Box::new(Expression::CInt(1)));
        let else_stmt = Assignment(String::from("y"), Box::new(Expression::CInt(0)));

        let if_statement = IfThenElse(
            Box::new(condition),
            Box::new(then_stmt),
            Some(Box::new(else_stmt)),
        );

        let setup_stmt = Assignment(String::from("x"), Box::new(Expression::CInt(10)));
        let program = Sequence(Box::new(setup_stmt), Box::new(if_statement));

        match execute(program, env) {
            Ok(new_env) => assert_eq!(new_env.get("y"), Some(&CInt(1))),
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn eval_if_then_optional_else() {
        let env = HashMap::new();

        let second_condition = Expression::LT(Box::new(Expression::Var(String::from("x"))), Box::new(Expression::CInt(0)));
        let second_then_stmt = Assignment(String::from("y"), Box::new(Expression::CInt(5)));

        let second_if_stmt =
            IfThenElse(Box::new(second_condition), Box::new(second_then_stmt), None);

        let else_setup_stmt = Assignment(String::from("y"), Box::new(Expression::CInt(2)));
        let else_stmt = Sequence(Box::new(else_setup_stmt), Box::new(second_if_stmt));

        let first_condition = Expression::EQ(
            Box::new(Expression::Var(String::from("x"))),
            Box::new(Expression::Var(String::from("y"))),
        );
        let first_then_stmt = Assignment(String::from("y"), Box::new(Expression::CInt(1)));

        let first_if_stmt = IfThenElse(
            Box::new(first_condition),
            Box::new(first_then_stmt),
            Some(Box::new(else_stmt)),
        );

        let second_assignment = Assignment(String::from("y"), Box::new(Expression::CInt(0)));
        let setup_stmt = Sequence(Box::new(second_assignment), Box::new(first_if_stmt));

        let first_assignment = Assignment(String::from("x"), Box::new(Expression::CInt(1)));
        let program = Sequence(Box::new(first_assignment), Box::new(setup_stmt));

        match execute(program, env) {
            Ok(new_env) => assert_eq!(new_env.get("y"), Some(&CInt(2))),
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn eval_while_using_rmd() {
        let env = HashMap::new();
        let a1 = Assignment(String::from("x"), Box::new(Expression::CInt(1)));
        let a2 = Assignment(String::from("y"), Box::new(Expression::CInt(1800)));
        let a3 = Assignment(String::from("z"), Box::new(Expression::CInt(0)));
        let a4 = Assignment(
            String::from("z"),
            Box::new(Expression::Add(Box::new(Expression::Var(String::from("z"))), Box::new(Expression::CInt(1)))),
        );
        let a5 = Assignment(
            String::from("z"),
            Box::new(Expression::Add(Box::new(Expression::Var(String::from("z"))), Box::new(Expression::CInt(1)))),
        );
        let a6 = Assignment(
            String::from("x"),
            Box::new(Expression::Add(Box::new(Expression::Var(String::from("x"))), Box::new(Expression::CInt(1)))),
        );

        let if_statement1 = IfThenElse(
            Box::new(Expression::EQ(
                Box::new(Expression::Rmd(Box::new(Expression::Var(String::from("x"))), Box::new(Expression::CInt(2)))),
                Box::new(Expression::CInt(0)),
            )),
            Box::new(a4),
            None,
        );
        let if_statement2 = IfThenElse(
            Box::new(Expression::EQ(
                Box::new(Expression::Rmd(
                    Box::new(Expression::Div(
                        Box::new(Expression::Var(String::from("y"))),
                        Box::new(Expression::Var(String::from("x"))),
                    )),
                    Box::new(Expression::CInt(2)),
                )),
                Box::new(Expression::CInt(0)),
            )),
            Box::new(a5),
            None,
        );

        let seq = Sequence(Box::new(if_statement1), Box::new(if_statement2));
        let if_statement = IfThenElse(
            Box::new(Expression::EQ(
                Box::new(Expression::Rmd(
                    Box::new(Expression::Var(String::from("y"))),
                    Box::new(Expression::Var(String::from("x"))),
                )),
                Box::new(Expression::CInt(0)),
            )),
            Box::new(seq),
            None,
        );

        let seq1 = Sequence(Box::new(if_statement), Box::new(a6));

        let while_statement = While(
            Box::new(Expression::LTE(
                Box::new(Expression::Mul(
                    Box::new(Expression::Var(String::from("x"))),
                    Box::new(Expression::Var(String::from("x"))),
                )),
                Box::new(Expression::Var(String::from("y"))),
            )),
            Box::new(seq1),
        );

        let seq2 = Sequence(Box::new(Sequence(Box::new(a1), Box::new(a2))), Box::new(a3));

        let program = Sequence(Box::new(seq2), Box::new(while_statement));

        match execute(program, env) {
            Ok(new_env) => {
                assert_eq!(new_env.get("x"), Some(&CInt(43)));
                assert_eq!(new_env.get("z"), Some(&CInt(27)));
            }
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn eval_while_with_if() {
        let env = HashMap::new();

        let a1 = Assignment(String::from("x"), Box::new(Expression::CInt(1)));
        let a2 = Assignment(String::from("y"), Box::new(Expression::CInt(16)));
        let a3 = Assignment(String::from("z"), Box::new(Expression::CInt(16)));
        let a4 = Assignment(String::from("a"), Box::new(Expression::CInt(0)));
        let a5 = Assignment(
            String::from("m"),
            Box::new(Expression::Div(
                Box::new(Expression::Add(
                    Box::new(Expression::Var(String::from("x"))),
                    Box::new(Expression::Var(String::from("y"))),
                )),
                Box::new(Expression::CInt(2)),
            )),
        );
        let a6 = Assignment(String::from("a"), Box::new(Expression::Var(String::from("m"))));
        let a7 = Assignment(
            String::from("x"),
            Box::new(Expression::Add(Box::new(Expression::Var(String::from("m"))), Box::new(Expression::CInt(1)))),
        );
        let a8 = Assignment(
            String::from("y"),
            Box::new(Expression::Sub(Box::new(Expression::Var(String::from("m"))), Box::new(Expression::CInt(1)))),
        );

        let seq = Sequence(Box::new(a6), Box::new(a7));

        let if_statement: Statement = IfThenElse(
            Box::new(Expression::LTE(
                Box::new(Expression::Mul(
                    Box::new(Expression::Var(String::from("m"))),
                    Box::new(Expression::Var(String::from("m"))),
                )),
                Box::new(Expression::Var(String::from("z"))),
            )),
            Box::new(seq),
            Some(Box::new(a8)),
        );

        let while_statement = While(
            Box::new(Expression::And(
                Box::new(Expression::LTE(
                    Box::new(Expression::Var(String::from("x"))),
                    Box::new(Expression::Var(String::from("y"))),
                )),
                Box::new(Expression::Not(Box::new(Expression::EQ(
                    Box::new(Expression::Mul(
                        Box::new(Expression::Var(String::from("a"))),
                        Box::new(Expression::Var(String::from("a"))),
                    )),
                    Box::new(Expression::Var(String::from("z"))),
                )))),
            )),
            Box::new(Sequence(Box::new(a5), Box::new(if_statement))),
        );

        let seq1 = Sequence(
            Box::new(a1),
            Box::new(Sequence(
                Box::new(a2),
                Box::new(Sequence(Box::new(a3), Box::new(a4))),
            )),
        );

        let program = Sequence(Box::new(seq1), Box::new(while_statement));

        match execute(program, env) {
            Ok(new_env) => {
                assert_eq!(new_env.get("x"), Some(&CInt(5)));
                assert_eq!(new_env.get("y"), Some(&CInt(7)));
                assert_eq!(new_env.get("a"), Some(&CInt(4)));
            }
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn eval_while_with_boolean() {
        let env = HashMap::new();
        let a1 = Assignment(String::from("x"), Box::new(Expression::CTrue));
        let a2 = Assignment(String::from("y"), Box::new(Expression::CInt(1)));
        let a3 = Assignment(String::from("x"), Box::new(Expression::CFalse));
        let a4 = Assignment(
            String::from("y"),
            Box::new(Expression::Mul(Box::new(Expression::Var(String::from("y"))), Box::new(Expression::CInt(2)))),
        );

        let if_then_else_statement = IfThenElse(
            Box::new(Expression::GT(
                Box::new(Expression::Var(String::from("y"))),
                Box::new(Expression::CInt(1000000000)),
            )),
            Box::new(a3),
            Some(Box::new(a4)),
        );

        let while_statement = While(
            Box::new(Expression::Var(String::from("x"))),
            Box::new(if_then_else_statement),
        );

        let program = Sequence(
            Box::new(a1),
            Box::new(Sequence(Box::new(a2), Box::new(while_statement))),
        );

        match execute(program, env) {
            Ok(new_env) => {
                assert_eq!(new_env.get("x"), Some(&CFalse));
                assert_eq!(new_env.get("y"), Some(&CInt(1073741824)));
            }
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn eval_complex_sequence() {
        let env = HashMap::new();

        let a1 = Assignment(String::from("x"), Box::new(Expression::CInt(5)));
        let a2 = Assignment(String::from("y"), Box::new(Expression::CInt(0)));
        let a3 = Assignment(
            String::from("z"),
            Box::new(Expression::Add(
                Box::new(Expression::Mul(Box::new(Expression::CInt(2)), Box::new(Expression::Var(String::from("x"))))),
                Box::new(Expression::CInt(3)),
            )),
        );

        let program = Sequence(Box::new(a1), Box::new(Sequence(Box::new(a2), Box::new(a3))));

        match execute(program, env) {
            Ok(new_env) => {
                assert_eq!(new_env.get("x"), Some(&CInt(5)));
                assert_eq!(new_env.get("y"), Some(&CInt(0)));
                assert_eq!(new_env.get("z"), Some(&CInt(13)));
            }
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn eval_another_if_test() {
        let input = "x = 10\nif x > 0:\n    y = 1\nelse:\n    y = 2";
        let res = parse_sequence_statement(input);
        match res {
            Ok((_, program)) => {
                let env = HashMap::new();
                match execute(program, env) {
                    Ok(new_env) => {
                        assert_eq!(new_env.get("x"), Some(&CInt(10)));
                        assert_eq!(new_env.get("y"), Some(&CInt(1)));
                    }
                    Err(s) => assert!(false, "{}", s),
                }
            }
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn test_file_exists() {
        let env = HashMap::new();
        
        // Test with a file that should exist (Cargo.toml)
        let cargo_exists = file_exists(Expression::CString("Cargo.toml".to_string()), &env);
        assert_eq!(cargo_exists, Ok(Expression::CTrue));
        
        // Test with a file that shouldn't exist
        let nonexistent = file_exists(Expression::CString("nonexistent_file.txt".to_string()), &env);
        assert_eq!(nonexistent, Ok(Expression::CFalse));
    }

    #[test]
    fn test_write_and_read_file() {
        let env = HashMap::new();
        let test_file = "test_file.txt";
        let test_content = "Hello, World!\nThis is a test file.";
        
        // Test writing file
        let write_result = write_file(
            Expression::CString(test_file.to_string()),
            Expression::CString(test_content.to_string()),
            &env,
        );
        assert_eq!(write_result, Ok(Expression::CTrue));
        
        // Test reading file
        let read_result = read_file(Expression::CString(test_file.to_string()), &env);
        assert_eq!(read_result, Ok(Expression::CString(test_content.to_string())));
        
        // Clean up test file
        let _ = fs::remove_file(test_file);
    }

    #[test]
    fn test_read_nonexistent_file() {
        let env = HashMap::new();
        let read_result = read_file(Expression::CString("nonexistent_file.txt".to_string()), &env);
        assert!(read_result.is_err());
        assert!(read_result.unwrap_err().contains("Failed to read file"));
    }

    #[test]
    fn test_function_call() {
        let env = HashMap::new();
        
        // Test read_file function call
        let file_exists_call = function_call(
            "file_exists".to_string(),
            vec![Expression::CString("Cargo.toml".to_string())],
            &env,
        );
        assert_eq!(file_exists_call, Ok(Expression::CTrue));
        
        // Test with wrong number of arguments
        let wrong_args = function_call(
            "file_exists".to_string(),
            vec![
                Expression::CString("file1.txt".to_string()),
                Expression::CString("file2.txt".to_string()),
            ],
            &env,
        );
        assert!(wrong_args.is_err());
        
        // Test unknown function
        let unknown_func = function_call(
            "unknown_function".to_string(),
            vec![Expression::CString("test".to_string())],
            &env,
        );
        assert!(unknown_func.is_err());
    }
}
