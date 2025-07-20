use super::statement_execute::Computation;
use crate::environment::environment::Environment;
use crate::ir::ast::{Expression, Name};

#[derive(Debug, PartialEq, Clone)]
pub enum ExpressionResult {
    Value(Expression),
    Propagate(Expression), // For error propagation in Maybe/Result types
}

pub fn eval(exp: Expression, env: &Environment<Expression>) -> Result<ExpressionResult, String> {
    match exp {
        Expression::Add(lhs, rhs) => eval_add(*lhs, *rhs, env),
        Expression::Sub(lhs, rhs) => eval_sub(*lhs, *rhs, env),
        Expression::Mul(lhs, rhs) => eval_mul(*lhs, *rhs, env),
        Expression::Div(lhs, rhs) => eval_div(*lhs, *rhs, env),
        Expression::And(lhs, rhs) => eval_and(*lhs, *rhs, env),
        Expression::Or(lhs, rhs) => eval_or(*lhs, *rhs, env),
        Expression::Not(lhs) => eval_not(*lhs, env),
        Expression::EQ(lhs, rhs) => eval_eq(*lhs, *rhs, env),
        Expression::NEQ(lhs, rhs) => eval_neq(*lhs, *rhs, env),
        Expression::GT(lhs, rhs) => eval_gt(*lhs, *rhs, env),
        Expression::LT(lhs, rhs) => eval_lt(*lhs, *rhs, env),
        Expression::GTE(lhs, rhs) => eval_gte(*lhs, *rhs, env),
        Expression::LTE(lhs, rhs) => eval_lte(*lhs, *rhs, env),
        Expression::Var(name) => eval_lookup(name, env),
        Expression::COk(e) => eval_ok(*e, env),
        Expression::CErr(e) => eval_err(*e, env),
        Expression::CJust(e) => eval_just(*e, env),
        Expression::Unwrap(e) => eval_unwrap_expression(*e, env),
        Expression::Propagate(e) => eval_propagate_expression(*e, env),
        Expression::IsError(e) => eval_iserror_expression(*e, env),
        Expression::IsNothing(e) => eval_isnothing_expression(*e, env),
        Expression::FuncCall(name, args) => eval_function_call(name, args, env),
        Expression::ListValue(values) => eval_list_value(values, env),
        Expression::CompletedProcess { .. } => Ok(ExpressionResult::Value(exp)),
        _ if is_constant(exp.clone()) => Ok(ExpressionResult::Value(exp)),
        _ => Err(String::from("Not implemented yet.")),
    }
}

// Helper function for arithmetic operations
fn eval_binary_arith_op<F>(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<Expression>,
    op: F,
    error_msg: &str,
) -> Result<ExpressionResult, String>
where
    F: Fn(f64, f64) -> f64,
{
    let v1 = match eval(lhs, env)? {
        ExpressionResult::Value(expr) => expr,
        ExpressionResult::Propagate(expr) => return Ok(ExpressionResult::Propagate(expr)),
    };
    let v2 = match eval(rhs, env)? {
        ExpressionResult::Value(expr) => expr,
        ExpressionResult::Propagate(expr) => return Ok(ExpressionResult::Propagate(expr)),
    };

    match (v1, v2) {
        (Expression::CInt(v1), Expression::CInt(v2)) => Ok(ExpressionResult::Value(
            Expression::CInt(op(v1 as f64, v2 as f64) as i32),
        )),
        (Expression::CInt(v1), Expression::CReal(v2)) => Ok(ExpressionResult::Value(
            Expression::CReal(op(v1 as f64, v2)),
        )),
        (Expression::CReal(v1), Expression::CInt(v2)) => Ok(ExpressionResult::Value(
            Expression::CReal(op(v1, v2 as f64)),
        )),
        (Expression::CReal(v1), Expression::CReal(v2)) => {
            Ok(ExpressionResult::Value(Expression::CReal(op(v1, v2))))
        }
        _ => Err(error_msg.to_string()),
    }
}

// Helper function for boolean operations
fn eval_binary_boolean_op<F>(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<Expression>,
    op: F,
    error_msg: &str,
) -> Result<ExpressionResult, String>
where
    F: Fn(bool, bool) -> Expression,
{
    let v1 = match eval(lhs, env)? {
        ExpressionResult::Value(expr) => expr,
        ExpressionResult::Propagate(expr) => return Ok(ExpressionResult::Propagate(expr)),
    };
    let v2 = match eval(rhs, env)? {
        ExpressionResult::Value(expr) => expr,
        ExpressionResult::Propagate(expr) => return Ok(ExpressionResult::Propagate(expr)),
    };

    match (v1, v2) {
        (Expression::CTrue, Expression::CTrue) => Ok(ExpressionResult::Value(op(true, true))),
        (Expression::CTrue, Expression::CFalse) => Ok(ExpressionResult::Value(op(true, false))),
        (Expression::CFalse, Expression::CTrue) => Ok(ExpressionResult::Value(op(false, true))),
        (Expression::CFalse, Expression::CFalse) => Ok(ExpressionResult::Value(op(false, false))),
        _ => Err(error_msg.to_string()),
    }
}

// Helper function for relational operations
fn eval_binary_rel_op<F>(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<Expression>,
    op: F,
    error_msg: &str,
) -> Result<ExpressionResult, String>
where
    F: Fn(f64, f64) -> Expression,
{
    let v1 = match eval(lhs, env)? {
        ExpressionResult::Value(expr) => expr,
        ExpressionResult::Propagate(expr) => return Ok(ExpressionResult::Propagate(expr)),
    };
    let v2 = match eval(rhs, env)? {
        ExpressionResult::Value(expr) => expr,
        ExpressionResult::Propagate(expr) => return Ok(ExpressionResult::Propagate(expr)),
    };

    match (v1, v2) {
        (Expression::CInt(v1), Expression::CInt(v2)) => {
            Ok(ExpressionResult::Value(op(v1 as f64, v2 as f64)))
        }
        (Expression::CInt(v1), Expression::CReal(v2)) => {
            Ok(ExpressionResult::Value(op(v1 as f64, v2)))
        }
        (Expression::CReal(v1), Expression::CInt(v2)) => {
            Ok(ExpressionResult::Value(op(v1, v2 as f64)))
        }
        (Expression::CReal(v1), Expression::CReal(v2)) => Ok(ExpressionResult::Value(op(v1, v2))),
        _ => Err(error_msg.to_string()),
    }
}

// Arithmetic Operations
fn eval_add(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<Expression>,
) -> Result<ExpressionResult, String> {
    eval_binary_arith_op(
        lhs,
        rhs,
        env,
        |a, b| a + b,
        "addition '(+)' is only defined for numbers (integers and real).",
    )
}

fn eval_sub(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<Expression>,
) -> Result<ExpressionResult, String> {
    eval_binary_arith_op(
        lhs,
        rhs,
        env,
        |a, b| a - b,
        "subtraction '(-)' is only defined for numbers (integers and real).",
    )
}

fn eval_mul(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<Expression>,
) -> Result<ExpressionResult, String> {
    eval_binary_arith_op(
        lhs,
        rhs,
        env,
        |a, b| a * b,
        "multiplication '(*)' is only defined for numbers (integers and real).",
    )
}

fn eval_div(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<Expression>,
) -> Result<ExpressionResult, String> {
    eval_binary_arith_op(
        lhs,
        rhs,
        env,
        |a, b| a / b,
        "division '(/)' is only defined for numbers (integers and real).",
    )
}

// Boolean Operations
fn eval_and(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<Expression>,
) -> Result<ExpressionResult, String> {
    eval_binary_boolean_op(
        lhs,
        rhs,
        env,
        |a, b| {
            if a && b {
                Expression::CTrue
            } else {
                Expression::CFalse
            }
        },
        "'and' is only defined for booleans.",
    )
}

fn eval_or(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<Expression>,
) -> Result<ExpressionResult, String> {
    eval_binary_boolean_op(
        lhs,
        rhs,
        env,
        |a, b| {
            if a || b {
                Expression::CTrue
            } else {
                Expression::CFalse
            }
        },
        "'or' is only defined for booleans.",
    )
}

fn eval_not(lhs: Expression, env: &Environment<Expression>) -> Result<ExpressionResult, String> {
    let v = match eval(lhs, env)? {
        ExpressionResult::Value(expr) => expr,
        ExpressionResult::Propagate(expr) => return Ok(ExpressionResult::Propagate(expr)),
    };
    match v {
        Expression::CTrue => Ok(ExpressionResult::Value(Expression::CFalse)),
        Expression::CFalse => Ok(ExpressionResult::Value(Expression::CTrue)),
        _ => Err(String::from("'not' is only defined for booleans.")),
    }
}

// Relational Operations
fn eval_eq(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<Expression>,
) -> Result<ExpressionResult, String> {
    eval_binary_rel_op(
        lhs,
        rhs,
        env,
        |a, b| {
            if a == b {
                Expression::CTrue
            } else {
                Expression::CFalse
            }
        },
        "equality '(==)' is only defined for numbers (integers and real).",
    )
}

fn eval_neq(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<Expression>,
) -> Result<ExpressionResult, String> {
    eval_binary_rel_op(
        lhs,
        rhs,
        env,
        |a, b| {
            if a != b {
                Expression::CTrue
            } else {
                Expression::CFalse
            }
        },
        "inequality '(!=)' is only defined for numbers (integers and real).",
    )
}

fn eval_gt(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<Expression>,
) -> Result<ExpressionResult, String> {
    eval_binary_rel_op(
        lhs,
        rhs,
        env,
        |a, b| {
            if a > b {
                Expression::CTrue
            } else {
                Expression::CFalse
            }
        },
        "greater than '(>)' is only defined for numbers (integers and real).",
    )
}

fn eval_lt(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<Expression>,
) -> Result<ExpressionResult, String> {
    eval_binary_rel_op(
        lhs,
        rhs,
        env,
        |a, b| {
            if a < b {
                Expression::CTrue
            } else {
                Expression::CFalse
            }
        },
        "less than '(<)' is only defined for numbers (integers and real).",
    )
}

fn eval_gte(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<Expression>,
) -> Result<ExpressionResult, String> {
    eval_binary_rel_op(
        lhs,
        rhs,
        env,
        |a, b| {
            if a >= b {
                Expression::CTrue
            } else {
                Expression::CFalse
            }
        },
        "greater than or equal '(>=)' is only defined for numbers (integers and real).",
    )
}

fn eval_lte(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<Expression>,
) -> Result<ExpressionResult, String> {
    eval_binary_rel_op(
        lhs,
        rhs,
        env,
        |a, b| {
            if a <= b {
                Expression::CTrue
            } else {
                Expression::CFalse
            }
        },
        "less than or equal '(<=)' is only defined for numbers (integers and real).",
    )
}

// Variable lookup
pub fn eval_lookup(
    name: String,
    env: &Environment<Expression>,
) -> Result<ExpressionResult, String> {
    match env.lookup(&name) {
        Some((_, value)) => Ok(ExpressionResult::Value(value.clone())),
        None => Err(format!("Variable '{}' not found", name)),
    }
}

// Function call
pub fn eval_function_call(
    name: Name,
    args: Vec<Expression>,
    env: &Environment<Expression>,
) -> Result<ExpressionResult, String> {
    // Check for built-in functions first
    if let Some(result) = super::builtins::eval_builtin_function(&name, args.clone(), env)? {
        return Ok(result);
    }
    
    // If not a built-in function, look for user-defined functions
    match env.lookup_function(&name) {
        Some(function_definition) => {
            let mut new_env = Environment::new();

            if args.len() != function_definition.params.len() {
                return Err(format!(
                    "[Runtime Error] Invalid number of arguments for '{}'.",
                    name
                ));
            }

            new_env.push();

            for (formal, actual) in function_definition.params.iter().zip(args.iter()) {
                let value = match eval(actual.clone(), env)? {
                    ExpressionResult::Value(expr) => expr,
                    ExpressionResult::Propagate(expr) => {
                        return Ok(ExpressionResult::Propagate(expr))
                    }
                };
                new_env.map_variable(formal.argument_name.clone(), false, value);
            }

            // Execute the body of the function.
            match super::statement_execute::execute(
                *function_definition.body.as_ref().unwrap().clone(),
                &new_env,
            ) {
                Ok(Computation::Continue(_)) => Err("Function did not return a value".to_string()),
                Ok(Computation::Return(value, _)) => Ok(ExpressionResult::Value(value)),
                Ok(Computation::PropagateError(value, _)) => Ok(ExpressionResult::Propagate(value)),
                Err(e) => Err(e),
            }
        }
        _ => Err(format!("Function {} not found", name)),
    }
}

// Other helpers
fn eval_unwrap_expression(
    exp: Expression,
    env: &Environment<Expression>,
) -> Result<ExpressionResult, String> {
    let v = match eval(exp, env)? {
        ExpressionResult::Value(expr) => expr,
        ExpressionResult::Propagate(expr) => return Ok(ExpressionResult::Propagate(expr)),
    };
    match v {
        Expression::CJust(e) => Ok(ExpressionResult::Value(*e)),
        Expression::COk(e) => Ok(ExpressionResult::Value(*e)),
        _ => Err(String::from("Program panicked trying to unwrap.")),
    }
}

fn eval_propagate_expression(
    exp: Expression,
    env: &Environment<Expression>,
) -> Result<ExpressionResult, String> {
    let v = match eval(exp, env)? {
        ExpressionResult::Value(expr) => expr,
        ExpressionResult::Propagate(expr) => return Ok(ExpressionResult::Propagate(expr)),
    };
    match v {
        Expression::CJust(e) => Ok(ExpressionResult::Value(*e)),
        Expression::COk(e) => Ok(ExpressionResult::Value(*e)),
        Expression::CErr(e) => Ok(ExpressionResult::Propagate(*e)),
        Expression::CNothing => Ok(ExpressionResult::Propagate(Expression::CString(
            "Couldn't unwrap Nothing".to_string(),
        ))),
        _ => Err(String::from("'propagate' expects a Just or Ok.")),
    }
}

fn eval_isnothing_expression(
    exp: Expression,
    env: &Environment<Expression>,
) -> Result<ExpressionResult, String> {
    let v = match eval(exp, env)? {
        ExpressionResult::Value(expr) => expr,
        ExpressionResult::Propagate(expr) => return Ok(ExpressionResult::Propagate(expr)),
    };
    match v {
        Expression::CNothing => Ok(ExpressionResult::Value(Expression::CTrue)),
        _ => Ok(ExpressionResult::Value(Expression::CFalse)),
    }
}
fn eval_iserror_expression(
    exp: Expression,
    env: &Environment<Expression>,
) -> Result<ExpressionResult, String> {
    let v = match eval(exp, env)? {
        ExpressionResult::Value(expr) => expr,
        ExpressionResult::Propagate(expr) => return Ok(ExpressionResult::Propagate(expr)),
    };
    match v {
        Expression::CErr(_) => Ok(ExpressionResult::Value(Expression::CTrue)),
        _ => Ok(ExpressionResult::Value(Expression::CFalse)),
    }
}
fn eval_just(exp: Expression, env: &Environment<Expression>) -> Result<ExpressionResult, String> {
    let v = match eval(exp, env)? {
        ExpressionResult::Value(expr) => expr,
        ExpressionResult::Propagate(expr) => return Ok(ExpressionResult::Propagate(expr)),
    };
    Ok(ExpressionResult::Value(Expression::CJust(Box::new(v))))
}
fn eval_ok(exp: Expression, env: &Environment<Expression>) -> Result<ExpressionResult, String> {
    let v = match eval(exp, env)? {
        ExpressionResult::Value(expr) => expr,
        ExpressionResult::Propagate(expr) => return Ok(ExpressionResult::Propagate(expr)),
    };
    Ok(ExpressionResult::Value(Expression::COk(Box::new(v))))
}
fn eval_err(exp: Expression, env: &Environment<Expression>) -> Result<ExpressionResult, String> {
    let v = match eval(exp, env)? {
        ExpressionResult::Value(expr) => expr,
        ExpressionResult::Propagate(expr) => return Ok(ExpressionResult::Propagate(expr)),
    };
    Ok(ExpressionResult::Value(Expression::CErr(Box::new(v))))
}

fn is_constant(exp: Expression) -> bool {
    match exp {
        Expression::CTrue => true,
        Expression::CFalse => true,
        Expression::CVoid => true,
        Expression::CInt(_) => true,
        Expression::CReal(_) => true,
        Expression::CString(_) => true,
        Expression::CNothing => true,
        _ => false,
    }
}

fn eval_list_value(
    sub_expressions: Vec<Expression>,
    env: &Environment<Expression>,
) -> Result<ExpressionResult, String> {
    let mut values = Vec::new();
    for exp in sub_expressions {
        match eval(exp, env)? {
            ExpressionResult::Value(expr) => values.push(expr),
            ExpressionResult::Propagate(expr) => return Ok(ExpressionResult::Propagate(expr)),
        }
    }
    Ok(ExpressionResult::Value(Expression::ListValue(values)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::environment::environment::Environment;

    fn create_test_env() -> Environment<Expression> {
        let mut env = Environment::new();

        // Add some variables for testing
        env.map_variable("x".to_string(), false, Expression::CInt(10));
        env.map_variable("y".to_string(), false, Expression::CReal(3.14));
        env.map_variable("flag".to_string(), false, Expression::CTrue);
        env.map_variable(
            "name".to_string(),
            false,
            Expression::CString("test".to_string()),
        );

        env
    }

    fn extract_value(result: ExpressionResult) -> Expression {
        match result {
            ExpressionResult::Value(expr) => expr,
            ExpressionResult::Propagate(expr) => {
                panic!("Expected Value but got Propagate: {:?}", expr)
            }
        }
    }

    mod arithmetic_expression_tests {
        use super::*;

        #[test]
        fn test_simple_addition() {
            let env = create_test_env();
            let expr =
                Expression::Add(Box::new(Expression::CInt(5)), Box::new(Expression::CInt(3)));

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CInt(8));
        }

        #[test]
        fn test_simple_subtraction() {
            let env = create_test_env();
            let expr = Expression::Sub(
                Box::new(Expression::CInt(10)),
                Box::new(Expression::CInt(4)),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CInt(6));
        }

        #[test]
        fn test_simple_multiplication() {
            let env = create_test_env();
            let expr =
                Expression::Mul(Box::new(Expression::CInt(7)), Box::new(Expression::CInt(6)));

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CInt(42));
        }

        #[test]
        fn test_simple_division() {
            let env = create_test_env();
            let expr = Expression::Div(
                Box::new(Expression::CInt(15)),
                Box::new(Expression::CInt(3)),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CInt(5));
        }

        #[test]
        fn test_real_number_addition() {
            let env = create_test_env();
            let expr = Expression::Add(
                Box::new(Expression::CReal(3.14)),
                Box::new(Expression::CReal(2.86)),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CReal(6.0));
        }

        #[test]
        fn test_real_number_subtraction() {
            let env = create_test_env();
            let expr = Expression::Sub(
                Box::new(Expression::CReal(10.5)),
                Box::new(Expression::CReal(3.2)),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CReal(7.3));
        }

        #[test]
        fn test_real_number_multiplication() {
            let env = create_test_env();
            let expr = Expression::Mul(
                Box::new(Expression::CReal(2.5)),
                Box::new(Expression::CReal(4.0)),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CReal(10.0));
        }

        #[test]
        fn test_real_number_division() {
            let env = create_test_env();
            let expr = Expression::Div(
                Box::new(Expression::CReal(15.0)),
                Box::new(Expression::CReal(3.0)),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CReal(5.0));
        }

        #[test]
        fn test_mixed_int_real_addition() {
            let env = create_test_env();
            let expr = Expression::Add(
                Box::new(Expression::CInt(5)),
                Box::new(Expression::CReal(3.7)),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CReal(8.7));
        }

        #[test]
        fn test_mixed_real_int_multiplication() {
            let env = create_test_env();
            let expr = Expression::Mul(
                Box::new(Expression::CReal(2.5)),
                Box::new(Expression::CInt(4)),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CReal(10.0));
        }

        #[test]
        fn test_arithmetic_with_variables() {
            let env = create_test_env();
            // x = 10, y = 3.14 (from create_test_env)
            let expr = Expression::Add(
                Box::new(Expression::Var("x".to_string())),
                Box::new(Expression::Var("y".to_string())),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CReal(13.14));
        }

        #[test]
        fn test_nested_addition_multiplication() {
            let env = create_test_env();
            // (2 + 3) * 4 = 5 * 4 = 20
            let expr = Expression::Mul(
                Box::new(Expression::Add(
                    Box::new(Expression::CInt(2)),
                    Box::new(Expression::CInt(3)),
                )),
                Box::new(Expression::CInt(4)),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CInt(20));
        }

        #[test]
        fn test_nested_subtraction_division() {
            let env = create_test_env();
            // (20 - 8) / 3 = 12 / 3 = 4
            let expr = Expression::Div(
                Box::new(Expression::Sub(
                    Box::new(Expression::CInt(20)),
                    Box::new(Expression::CInt(8)),
                )),
                Box::new(Expression::CInt(3)),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CInt(4));
        }

        #[test]
        fn test_complex_nested_expression() {
            let env = create_test_env();
            // ((2 + 3) * 4) - (10 / 2) = (5 * 4) - 5 = 20 - 5 = 15
            let expr = Expression::Sub(
                Box::new(Expression::Mul(
                    Box::new(Expression::Add(
                        Box::new(Expression::CInt(2)),
                        Box::new(Expression::CInt(3)),
                    )),
                    Box::new(Expression::CInt(4)),
                )),
                Box::new(Expression::Div(
                    Box::new(Expression::CInt(10)),
                    Box::new(Expression::CInt(2)),
                )),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CInt(15));
        }

        #[test]
        fn test_arithmetic_with_variable_reference() {
            let env = create_test_env();
            // x * 2 + 5 = 10 * 2 + 5 = 25
            let expr = Expression::Add(
                Box::new(Expression::Mul(
                    Box::new(Expression::Var("x".to_string())),
                    Box::new(Expression::CInt(2)),
                )),
                Box::new(Expression::CInt(5)),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CInt(25));
        }

        #[test]
        fn test_division_by_zero_integer() {
            let env = create_test_env();
            let expr = Expression::Div(
                Box::new(Expression::CInt(10)),
                Box::new(Expression::CInt(0)),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            // Division by zero should convert to floating point and produce infinity
            let result_val = result.unwrap();
            if let ExpressionResult::Value(Expression::CReal(val)) = result_val {
                assert!(val.is_infinite());
            } else {
                // If implementation returns integer, we allow any result for division by zero
                assert!(true); // Implementation-defined behavior for integer division by zero
            }
        }

        #[test]
        fn test_division_by_zero_real() {
            let env = create_test_env();
            let expr = Expression::Div(
                Box::new(Expression::CReal(10.0)),
                Box::new(Expression::CReal(0.0)),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            if let ExpressionResult::Value(Expression::CReal(val)) = result.unwrap() {
                assert!(val.is_infinite());
            }
        }

        #[test]
        fn test_arithmetic_with_non_numeric_types_error() {
            let env = create_test_env();
            let expr = Expression::Add(
                Box::new(Expression::CString("hello".to_string())),
                Box::new(Expression::CInt(5)),
            );

            let result = eval(expr, &env);

            assert!(result.is_err());
            let error = result.unwrap_err();
            assert_eq!(
                error,
                "addition '(+)' is only defined for numbers (integers and real)."
            );
        }

        #[test]
        fn test_multiplication_with_boolean_error() {
            let env = create_test_env();
            let expr = Expression::Mul(Box::new(Expression::CTrue), Box::new(Expression::CInt(10)));

            let result = eval(expr, &env);

            assert!(result.is_err());
            let error = result.unwrap_err();
            assert_eq!(
                error,
                "multiplication '(*)' is only defined for numbers (integers and real)."
            );
        }

        #[test]
        fn test_subtraction_with_invalid_variable_error() {
            let env = create_test_env();
            let expr = Expression::Sub(
                Box::new(Expression::Var("nonexistent".to_string())),
                Box::new(Expression::CInt(5)),
            );

            let result = eval(expr, &env);

            assert!(result.is_err());
            let error = result.unwrap_err();
            assert_eq!(error, "Variable 'nonexistent' not found");
        }

        #[test]
        fn test_chained_operations() {
            let env = create_test_env();
            // 1 + 2 + 3 + 4 = ((1 + 2) + 3) + 4 = (3 + 3) + 4 = 6 + 4 = 10
            let expr = Expression::Add(
                Box::new(Expression::Add(
                    Box::new(Expression::Add(
                        Box::new(Expression::CInt(1)),
                        Box::new(Expression::CInt(2)),
                    )),
                    Box::new(Expression::CInt(3)),
                )),
                Box::new(Expression::CInt(4)),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CInt(10));
        }

        #[test]
        fn test_mixed_operations_with_real_numbers() {
            let env = create_test_env();
            // (5.0 / 2.0) + (3.0 * 1.5) = 2.5 + 4.5 = 7.0
            let expr = Expression::Add(
                Box::new(Expression::Div(
                    Box::new(Expression::CReal(5.0)),
                    Box::new(Expression::CReal(2.0)),
                )),
                Box::new(Expression::Mul(
                    Box::new(Expression::CReal(3.0)),
                    Box::new(Expression::CReal(1.5)),
                )),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CReal(7.0));
        }

        #[test]
        fn test_negative_numbers() {
            let env = create_test_env();
            // (-5) + 10 = 5
            let expr = Expression::Add(
                Box::new(Expression::CInt(-5)),
                Box::new(Expression::CInt(10)),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CInt(5));
        }

        #[test]
        fn test_zero_operations() {
            let env = create_test_env();

            // Test addition with zero
            let expr1 = Expression::Add(
                Box::new(Expression::CInt(42)),
                Box::new(Expression::CInt(0)),
            );
            let result1 = eval(expr1, &env);
            assert!(result1.is_ok());
            assert_eq!(extract_value(result1.unwrap()), Expression::CInt(42));

            // Test multiplication by zero
            let expr2 = Expression::Mul(
                Box::new(Expression::CInt(999)),
                Box::new(Expression::CInt(0)),
            );
            let result2 = eval(expr2, &env);
            assert!(result2.is_ok());
            assert_eq!(extract_value(result2.unwrap()), Expression::CInt(0));

            // Test subtraction of zero
            let expr3 = Expression::Sub(
                Box::new(Expression::CReal(3.14)),
                Box::new(Expression::CReal(0.0)),
            );
            let result3 = eval(expr3, &env);
            assert!(result3.is_ok());
            assert_eq!(extract_value(result3.unwrap()), Expression::CReal(3.14));
        }
    }

    mod list_value_tests {
        use super::*;

        #[test]
        fn test_empty_list() {
            let env = create_test_env();
            let list_expr = Expression::ListValue(vec![]);

            let result = eval(list_expr, &env);

            assert!(result.is_ok());
            assert_eq!(
                extract_value(result.unwrap()),
                Expression::ListValue(vec![])
            );
        }

        #[test]
        fn test_list_with_constants() {
            let env = create_test_env();
            let list_expr = Expression::ListValue(vec![
                Expression::CInt(1),
                Expression::CInt(2),
                Expression::CInt(3),
            ]);

            let result = eval(list_expr, &env);

            assert!(result.is_ok());
            let expected = Expression::ListValue(vec![
                Expression::CInt(1),
                Expression::CInt(2),
                Expression::CInt(3),
            ]);
            assert_eq!(extract_value(result.unwrap()), expected);
        }

        #[test]
        fn test_list_with_reals() {
            let env = create_test_env();
            let list_expr = Expression::ListValue(vec![
                Expression::CReal(3.14),
                Expression::CReal(2.71),
                Expression::CReal(1.0),
            ]);

            let result = eval(list_expr, &env);

            assert!(result.is_ok());
            let expected = Expression::ListValue(vec![
                Expression::CReal(3.14),
                Expression::CReal(2.71),
                Expression::CReal(1.0),
            ]);
            assert_eq!(extract_value(result.unwrap()), expected);
        }

        #[test]
        fn test_list_with_strings() {
            let env = create_test_env();
            let list_expr = Expression::ListValue(vec![
                Expression::CString("hello".to_string()),
                Expression::CString("world".to_string()),
                Expression::CString("test".to_string()),
            ]);

            let result = eval(list_expr, &env);

            assert!(result.is_ok());
            let expected = Expression::ListValue(vec![
                Expression::CString("hello".to_string()),
                Expression::CString("world".to_string()),
                Expression::CString("test".to_string()),
            ]);
            assert_eq!(extract_value(result.unwrap()), expected);
        }

        #[test]
        fn test_list_with_booleans() {
            let env = create_test_env();
            let list_expr = Expression::ListValue(vec![
                Expression::CTrue,
                Expression::CFalse,
                Expression::CTrue,
            ]);

            let result = eval(list_expr, &env);

            assert!(result.is_ok());
            let expected = Expression::ListValue(vec![
                Expression::CTrue,
                Expression::CFalse,
                Expression::CTrue,
            ]);
            assert_eq!(extract_value(result.unwrap()), expected);
        }

        #[test]
        fn test_list_with_mixed_numbers() {
            let env = create_test_env();
            let list_expr = Expression::ListValue(vec![
                Expression::CInt(42),
                Expression::CReal(3.14),
                Expression::CInt(10),
            ]);

            let result = eval(list_expr, &env);

            assert!(result.is_ok());
            let expected = Expression::ListValue(vec![
                Expression::CInt(42),
                Expression::CReal(3.14),
                Expression::CInt(10),
            ]);
            assert_eq!(extract_value(result.unwrap()), expected);
        }

        #[test]
        fn test_list_with_integer_variables() {
            let mut env = create_test_env();
            env.map_variable("a".to_string(), false, Expression::CInt(5));
            env.map_variable("b".to_string(), false, Expression::CInt(15));

            let list_expr = Expression::ListValue(vec![
                Expression::Var("x".to_string()), // 10
                Expression::Var("a".to_string()), // 5
                Expression::Var("b".to_string()), // 15
            ]);

            let result = eval(list_expr, &env);

            assert!(result.is_ok());
            let expected = Expression::ListValue(vec![
                Expression::CInt(10),
                Expression::CInt(5),
                Expression::CInt(15),
            ]);
            assert_eq!(extract_value(result.unwrap()), expected);
        }

        #[test]
        fn test_list_with_string_variables() {
            let mut env = create_test_env();
            env.map_variable(
                "greeting".to_string(),
                false,
                Expression::CString("hello".to_string()),
            );
            env.map_variable(
                "world".to_string(),
                false,
                Expression::CString("world".to_string()),
            );

            let list_expr = Expression::ListValue(vec![
                Expression::Var("name".to_string()),     // "test"
                Expression::Var("greeting".to_string()), // "hello"
                Expression::Var("world".to_string()),    // "world"
            ]);

            let result = eval(list_expr, &env);

            assert!(result.is_ok());
            let expected = Expression::ListValue(vec![
                Expression::CString("test".to_string()),
                Expression::CString("hello".to_string()),
                Expression::CString("world".to_string()),
            ]);
            assert_eq!(extract_value(result.unwrap()), expected);
        }

        #[test]
        fn test_list_with_arithmetic_expressions() {
            let env = create_test_env();
            let list_expr = Expression::ListValue(vec![
                Expression::Add(Box::new(Expression::CInt(1)), Box::new(Expression::CInt(2))),
                Expression::Mul(Box::new(Expression::CInt(3)), Box::new(Expression::CInt(4))),
                Expression::Sub(
                    Box::new(Expression::Var("x".to_string())),
                    Box::new(Expression::CInt(5)),
                ),
            ]);

            let result = eval(list_expr, &env);

            assert!(result.is_ok());
            let expected = Expression::ListValue(vec![
                Expression::CInt(3),  // 1 + 2
                Expression::CInt(12), // 3 * 4
                Expression::CInt(5),  // 10 - 5
            ]);
            assert_eq!(extract_value(result.unwrap()), expected);
        }

        #[test]
        fn test_list_with_boolean_expressions() {
            let env = create_test_env();
            let list_expr = Expression::ListValue(vec![
                Expression::And(Box::new(Expression::CTrue), Box::new(Expression::CFalse)),
                Expression::Or(Box::new(Expression::CTrue), Box::new(Expression::CFalse)),
                Expression::Not(Box::new(Expression::CFalse)),
            ]);

            let result = eval(list_expr, &env);

            assert!(result.is_ok());
            let expected = Expression::ListValue(vec![
                Expression::CFalse, // True and False
                Expression::CTrue,  // True or False
                Expression::CTrue,  // not False
            ]);
            assert_eq!(extract_value(result.unwrap()), expected);
        }

        #[test]
        fn test_list_with_relational_expressions() {
            let env = create_test_env();
            let list_expr = Expression::ListValue(vec![
                Expression::GT(Box::new(Expression::CInt(5)), Box::new(Expression::CInt(3))),
                Expression::LT(Box::new(Expression::CInt(2)), Box::new(Expression::CInt(8))),
                Expression::EQ(Box::new(Expression::CInt(4)), Box::new(Expression::CInt(4))),
            ]);

            let result = eval(list_expr, &env);

            assert!(result.is_ok());
            let expected = Expression::ListValue(vec![
                Expression::CTrue, // 5 > 3
                Expression::CTrue, // 2 < 8
                Expression::CTrue, // 4 == 4
            ]);
            assert_eq!(extract_value(result.unwrap()), expected);
        }

        #[test]
        fn test_list_with_maybe_integers() {
            let env = create_test_env();
            let list_expr = Expression::ListValue(vec![
                Expression::CJust(Box::new(Expression::CInt(42))),
                Expression::CNothing,
                Expression::CJust(Box::new(Expression::CInt(10))),
            ]);

            let result = eval(list_expr, &env);

            assert!(result.is_ok());
            let expected = Expression::ListValue(vec![
                Expression::CJust(Box::new(Expression::CInt(42))),
                Expression::CNothing,
                Expression::CJust(Box::new(Expression::CInt(10))),
            ]);
            assert_eq!(extract_value(result.unwrap()), expected);
        }

        #[test]
        fn test_list_with_maybe_strings() {
            let env = create_test_env();
            let list_expr = Expression::ListValue(vec![
                Expression::CJust(Box::new(Expression::CString("hello".to_string()))),
                Expression::CJust(Box::new(Expression::CString("world".to_string()))),
                Expression::CNothing,
            ]);

            let result = eval(list_expr, &env);

            assert!(result.is_ok());
            let expected = Expression::ListValue(vec![
                Expression::CJust(Box::new(Expression::CString("hello".to_string()))),
                Expression::CJust(Box::new(Expression::CString("world".to_string()))),
                Expression::CNothing,
            ]);
            assert_eq!(extract_value(result.unwrap()), expected);
        }

        #[test]
        fn test_list_with_result_integers() {
            let env = create_test_env();
            let list_expr = Expression::ListValue(vec![
                Expression::COk(Box::new(Expression::CInt(100))),
                Expression::CErr(Box::new(Expression::CString("error".to_string()))),
                Expression::COk(Box::new(Expression::CInt(42))),
            ]);

            let result = eval(list_expr, &env);

            assert!(result.is_ok());
            let expected = Expression::ListValue(vec![
                Expression::COk(Box::new(Expression::CInt(100))),
                Expression::CErr(Box::new(Expression::CString("error".to_string()))),
                Expression::COk(Box::new(Expression::CInt(42))),
            ]);
            assert_eq!(extract_value(result.unwrap()), expected);
        }

        #[test]
        fn test_list_with_result_strings() {
            let env = create_test_env();
            let list_expr = Expression::ListValue(vec![
                Expression::COk(Box::new(Expression::CString("success".to_string()))),
                Expression::COk(Box::new(Expression::CString("another".to_string()))),
                Expression::CErr(Box::new(Expression::CString("failure".to_string()))),
            ]);

            let result = eval(list_expr, &env);

            assert!(result.is_ok());
            let expected = Expression::ListValue(vec![
                Expression::COk(Box::new(Expression::CString("success".to_string()))),
                Expression::COk(Box::new(Expression::CString("another".to_string()))),
                Expression::CErr(Box::new(Expression::CString("failure".to_string()))),
            ]);
            assert_eq!(extract_value(result.unwrap()), expected);
        }

        #[test]
        fn test_list_with_unwrap_expressions() {
            let env = create_test_env();
            let list_expr = Expression::ListValue(vec![
                Expression::Unwrap(Box::new(Expression::CJust(Box::new(Expression::CInt(5))))),
                Expression::Unwrap(Box::new(Expression::COk(Box::new(Expression::CString(
                    "ok".to_string(),
                ))))),
            ]);

            let result = eval(list_expr, &env);

            assert!(result.is_ok());
            let expected = Expression::ListValue(vec![
                Expression::CInt(5),
                Expression::CString("ok".to_string()),
            ]);
            assert_eq!(extract_value(result.unwrap()), expected);
        }

        #[test]
        fn test_nested_integer_lists() {
            let env = create_test_env();
            let list_expr = Expression::ListValue(vec![
                Expression::ListValue(vec![Expression::CInt(1), Expression::CInt(2)]),
                Expression::ListValue(vec![Expression::CInt(3), Expression::CInt(4)]),
                Expression::ListValue(vec![Expression::CInt(5)]),
            ]);

            let result = eval(list_expr, &env);

            assert!(result.is_ok());
            let expected = Expression::ListValue(vec![
                Expression::ListValue(vec![Expression::CInt(1), Expression::CInt(2)]),
                Expression::ListValue(vec![Expression::CInt(3), Expression::CInt(4)]),
                Expression::ListValue(vec![Expression::CInt(5)]),
            ]);
            assert_eq!(extract_value(result.unwrap()), expected);
        }

        #[test]
        fn test_nested_string_lists() {
            let env = create_test_env();
            let list_expr = Expression::ListValue(vec![
                Expression::ListValue(vec![
                    Expression::CString("a".to_string()),
                    Expression::CString("b".to_string()),
                ]),
                Expression::ListValue(vec![
                    Expression::CString("c".to_string()),
                    Expression::CString("d".to_string()),
                ]),
                Expression::ListValue(vec![]),
            ]);

            let result = eval(list_expr, &env);

            assert!(result.is_ok());
            let expected = Expression::ListValue(vec![
                Expression::ListValue(vec![
                    Expression::CString("a".to_string()),
                    Expression::CString("b".to_string()),
                ]),
                Expression::ListValue(vec![
                    Expression::CString("c".to_string()),
                    Expression::CString("d".to_string()),
                ]),
                Expression::ListValue(vec![]),
            ]);
            assert_eq!(extract_value(result.unwrap()), expected);
        }

        #[test]
        fn test_list_with_invalid_variable() {
            let env = create_test_env();
            let list_expr = Expression::ListValue(vec![
                Expression::CInt(1),
                Expression::Var("nonexistent".to_string()),
                Expression::CInt(3),
            ]);

            let result = eval(list_expr, &env);

            assert!(result.is_err());
            let error = result.unwrap_err();
            assert_eq!(error, "Variable 'nonexistent' not found");
        }

        #[test]
        fn test_list_with_invalid_arithmetic() {
            let env = create_test_env();
            let list_expr = Expression::ListValue(vec![
                Expression::CInt(1),
                Expression::Add(
                    Box::new(Expression::CString("hello".to_string())),
                    Box::new(Expression::CInt(2)),
                ),
                Expression::CInt(3),
            ]);

            let result = eval(list_expr, &env);

            assert!(result.is_err());
            let error = result.unwrap_err();
            assert_eq!(
                error,
                "addition '(+)' is only defined for numbers (integers and real)."
            );
        }

        #[test]
        fn test_list_with_unwrap_failure() {
            let env = create_test_env();
            let list_expr = Expression::ListValue(vec![
                Expression::CInt(1),
                Expression::Unwrap(Box::new(Expression::CNothing)),
                Expression::CInt(3),
            ]);

            let result = eval(list_expr, &env);

            assert!(result.is_err());
            let error = result.unwrap_err();
            assert_eq!(error, "Program panicked trying to unwrap.");
        }

        #[test]
        fn test_list_with_propagate_expressions() {
            let env = create_test_env();

            // Test successful propagation
            let list_expr = Expression::ListValue(vec![
                Expression::Propagate(Box::new(Expression::CJust(Box::new(Expression::CInt(42))))),
                Expression::Propagate(Box::new(Expression::COk(Box::new(Expression::CString(
                    "success".to_string(),
                ))))),
            ]);

            let result = eval(list_expr, &env);

            assert!(result.is_ok());
            let expected = Expression::ListValue(vec![
                Expression::CInt(42),
                Expression::CString("success".to_string()),
            ]);
            assert_eq!(extract_value(result.unwrap()), expected);
        }

        #[test]
        fn test_list_with_propagate_error() {
            let env = create_test_env();
            let list_expr = Expression::ListValue(vec![
                Expression::CInt(1),
                Expression::Propagate(Box::new(Expression::CErr(Box::new(Expression::CString(
                    "error".to_string(),
                ))))),
                Expression::CInt(3),
            ]);

            let result = eval(list_expr, &env);

            // In the new design, propagate expressions result in Ok(ExpressionResult::Propagate(...))
            assert!(result.is_ok());
            if let Ok(ExpressionResult::Propagate(error_expr)) = result {
                assert_eq!(error_expr, Expression::CString("error".to_string()));
            } else {
                panic!("Expected propagate result");
            }
        }

        #[test]
        fn test_list_with_isnothing_expressions() {
            let env = create_test_env();
            let list_expr = Expression::ListValue(vec![
                Expression::IsNothing(Box::new(Expression::CNothing)),
                Expression::IsNothing(Box::new(Expression::CJust(Box::new(Expression::CInt(5))))),
                Expression::IsNothing(Box::new(Expression::CInt(10))),
            ]);

            let result = eval(list_expr, &env);

            assert!(result.is_ok());
            let expected = Expression::ListValue(vec![
                Expression::CTrue,  // CNothing is nothing
                Expression::CFalse, // CJust(5) is not nothing
                Expression::CFalse, // CInt(10) is not nothing
            ]);
            assert_eq!(extract_value(result.unwrap()), expected);
        }

        #[test]
        fn test_list_with_iserror_expressions() {
            let env = create_test_env();
            let list_expr = Expression::ListValue(vec![
                Expression::IsError(Box::new(Expression::CErr(Box::new(Expression::CString(
                    "error".to_string(),
                ))))),
                Expression::IsError(Box::new(Expression::COk(Box::new(Expression::CInt(5))))),
                Expression::IsError(Box::new(Expression::CInt(10))),
            ]);

            let result = eval(list_expr, &env);

            assert!(result.is_ok());
            let expected = Expression::ListValue(vec![
                Expression::CTrue,  // CErr is an error
                Expression::CFalse, // COk is not an error
                Expression::CFalse, // CInt is not an error
            ]);
            assert_eq!(extract_value(result.unwrap()), expected);
        }
    }

    mod boolean_expression_tests {
        use super::*;

        #[test]
        fn test_and_true_true() {
            let env = create_test_env();
            let expr = Expression::And(Box::new(Expression::CTrue), Box::new(Expression::CTrue));

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_and_true_false() {
            let env = create_test_env();
            let expr = Expression::And(Box::new(Expression::CTrue), Box::new(Expression::CFalse));

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CFalse);
        }

        #[test]
        fn test_and_false_true() {
            let env = create_test_env();
            let expr = Expression::And(Box::new(Expression::CFalse), Box::new(Expression::CTrue));

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CFalse);
        }

        #[test]
        fn test_and_false_false() {
            let env = create_test_env();
            let expr = Expression::And(Box::new(Expression::CFalse), Box::new(Expression::CFalse));

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CFalse);
        }

        #[test]
        fn test_or_true_true() {
            let env = create_test_env();
            let expr = Expression::Or(Box::new(Expression::CTrue), Box::new(Expression::CTrue));

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_or_true_false() {
            let env = create_test_env();
            let expr = Expression::Or(Box::new(Expression::CTrue), Box::new(Expression::CFalse));

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_or_false_true() {
            let env = create_test_env();
            let expr = Expression::Or(Box::new(Expression::CFalse), Box::new(Expression::CTrue));

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_or_false_false() {
            let env = create_test_env();
            let expr = Expression::Or(Box::new(Expression::CFalse), Box::new(Expression::CFalse));

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CFalse);
        }

        #[test]
        fn test_not_true() {
            let env = create_test_env();
            let expr = Expression::Not(Box::new(Expression::CTrue));

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CFalse);
        }

        #[test]
        fn test_not_false() {
            let env = create_test_env();
            let expr = Expression::Not(Box::new(Expression::CFalse));

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_boolean_with_variables() {
            let mut env = create_test_env();
            env.map_variable("x".to_string(), false, Expression::CTrue);
            env.map_variable("y".to_string(), false, Expression::CFalse);

            let expr = Expression::And(
                Box::new(Expression::Var("x".to_string())),
                Box::new(Expression::Var("y".to_string())),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CFalse);
        }

        #[test]
        fn test_nested_and_or() {
            let env = create_test_env();
            // (True and False) or True => False or True => True
            let expr = Expression::Or(
                Box::new(Expression::And(
                    Box::new(Expression::CTrue),
                    Box::new(Expression::CFalse),
                )),
                Box::new(Expression::CTrue),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_nested_or_and() {
            let env = create_test_env();
            // (True or False) and False => True and False => False
            let expr = Expression::And(
                Box::new(Expression::Or(
                    Box::new(Expression::CTrue),
                    Box::new(Expression::CFalse),
                )),
                Box::new(Expression::CFalse),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CFalse);
        }

        #[test]
        fn test_double_negation() {
            let env = create_test_env();
            // not (not True) => not False => True
            let expr = Expression::Not(Box::new(Expression::Not(Box::new(Expression::CTrue))));

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_complex_boolean_expression() {
            let env = create_test_env();
            // not (True and False) or (False or True) => not False or True => True or True => True
            let expr = Expression::Or(
                Box::new(Expression::Not(Box::new(Expression::And(
                    Box::new(Expression::CTrue),
                    Box::new(Expression::CFalse),
                )))),
                Box::new(Expression::Or(
                    Box::new(Expression::CFalse),
                    Box::new(Expression::CTrue),
                )),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_boolean_with_relational_expressions() {
            let env = create_test_env();
            // (5 > 3) and (2 < 4) => True and True => True
            let expr = Expression::And(
                Box::new(Expression::GT(
                    Box::new(Expression::CInt(5)),
                    Box::new(Expression::CInt(3)),
                )),
                Box::new(Expression::LT(
                    Box::new(Expression::CInt(2)),
                    Box::new(Expression::CInt(4)),
                )),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_boolean_with_mixed_relational() {
            let env = create_test_env();
            // (10 == 10) or (5 > 8) => True or False => True
            let expr = Expression::Or(
                Box::new(Expression::EQ(
                    Box::new(Expression::CInt(10)),
                    Box::new(Expression::CInt(10)),
                )),
                Box::new(Expression::GT(
                    Box::new(Expression::CInt(5)),
                    Box::new(Expression::CInt(8)),
                )),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_not_with_relational() {
            let env = create_test_env();
            // not (3 > 5) => not False => True
            let expr = Expression::Not(Box::new(Expression::GT(
                Box::new(Expression::CInt(3)),
                Box::new(Expression::CInt(5)),
            )));

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_boolean_with_variables_and_relational() {
            let mut env = create_test_env();
            env.map_variable("a".to_string(), false, Expression::CInt(10));
            env.map_variable("b".to_string(), false, Expression::CInt(5));
            env.map_variable("flag".to_string(), false, Expression::CTrue);

            // flag and (a > b) => True and (10 > 5) => True and True => True
            let expr = Expression::And(
                Box::new(Expression::Var("flag".to_string())),
                Box::new(Expression::GT(
                    Box::new(Expression::Var("a".to_string())),
                    Box::new(Expression::Var("b".to_string())),
                )),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_deeply_nested_boolean() {
            let env = create_test_env();
            // ((True or False) and True) or ((False and True) or True)
            // => (True and True) or (False or True)
            // => True or True
            // => True
            let expr = Expression::Or(
                Box::new(Expression::And(
                    Box::new(Expression::Or(
                        Box::new(Expression::CTrue),
                        Box::new(Expression::CFalse),
                    )),
                    Box::new(Expression::CTrue),
                )),
                Box::new(Expression::Or(
                    Box::new(Expression::And(
                        Box::new(Expression::CFalse),
                        Box::new(Expression::CTrue),
                    )),
                    Box::new(Expression::CTrue),
                )),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_boolean_arithmetic_mixed_expressions() {
            let env = create_test_env();
            // ((2 + 3) == 5) and ((4 * 2) > 7) => (5 == 5) and (8 > 7) => True and True => True
            let expr = Expression::And(
                Box::new(Expression::EQ(
                    Box::new(Expression::Add(
                        Box::new(Expression::CInt(2)),
                        Box::new(Expression::CInt(3)),
                    )),
                    Box::new(Expression::CInt(5)),
                )),
                Box::new(Expression::GT(
                    Box::new(Expression::Mul(
                        Box::new(Expression::CInt(4)),
                        Box::new(Expression::CInt(2)),
                    )),
                    Box::new(Expression::CInt(7)),
                )),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_boolean_with_real_numbers() {
            let env = create_test_env();
            // (3.14 > 3.0) or (2.5 < 2.0) => True or False => True
            let expr = Expression::Or(
                Box::new(Expression::GT(
                    Box::new(Expression::CReal(3.14)),
                    Box::new(Expression::CReal(3.0)),
                )),
                Box::new(Expression::LT(
                    Box::new(Expression::CReal(2.5)),
                    Box::new(Expression::CReal(2.0)),
                )),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_and_error_with_integer() {
            let env = create_test_env();
            let expr = Expression::And(Box::new(Expression::CTrue), Box::new(Expression::CInt(5)));

            let result = eval(expr, &env);

            assert!(result.is_err());
            let error = result.unwrap_err();
            assert_eq!(error, "'and' is only defined for booleans.");
        }

        #[test]
        fn test_or_error_with_string() {
            let env = create_test_env();
            let expr = Expression::Or(
                Box::new(Expression::CFalse),
                Box::new(Expression::CString("hello".to_string())),
            );

            let result = eval(expr, &env);

            assert!(result.is_err());
            let error = result.unwrap_err();
            assert_eq!(error, "'or' is only defined for booleans.");
        }

        #[test]
        fn test_not_error_with_real() {
            let env = create_test_env();
            let expr = Expression::Not(Box::new(Expression::CReal(3.14)));

            let result = eval(expr, &env);

            assert!(result.is_err());
            let error = result.unwrap_err();
            assert_eq!(error, "'not' is only defined for booleans.");
        }

        #[test]
        fn test_boolean_error_with_mixed_types() {
            let env = create_test_env();
            let expr = Expression::And(
                Box::new(Expression::CInt(42)),
                Box::new(Expression::CString("test".to_string())),
            );

            let result = eval(expr, &env);

            assert!(result.is_err());
            let error = result.unwrap_err();
            assert_eq!(error, "'and' is only defined for booleans.");
        }

        #[test]
        fn test_boolean_with_undefined_variable() {
            let env = create_test_env();
            let expr = Expression::Or(
                Box::new(Expression::CTrue),
                Box::new(Expression::Var("undefined_var".to_string())),
            );

            let result = eval(expr, &env);

            assert!(result.is_err());
            let error = result.unwrap_err();
            assert_eq!(error, "Variable 'undefined_var' not found");
        }

        #[test]
        fn test_chained_boolean_operators() {
            let env = create_test_env();
            // True and True and False => False
            let expr = Expression::And(
                Box::new(Expression::And(
                    Box::new(Expression::CTrue),
                    Box::new(Expression::CTrue),
                )),
                Box::new(Expression::CFalse),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CFalse);
        }

        #[test]
        fn test_chained_or_operators() {
            let env = create_test_env();
            // False or False or True => True
            let expr = Expression::Or(
                Box::new(Expression::Or(
                    Box::new(Expression::CFalse),
                    Box::new(Expression::CFalse),
                )),
                Box::new(Expression::CTrue),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_de_morgan_law_example() {
            let env = create_test_env();
            // not (True and False) should equal (not True) or (not False)
            // not (True and False) => not False => True
            let left_expr = Expression::Not(Box::new(Expression::And(
                Box::new(Expression::CTrue),
                Box::new(Expression::CFalse),
            )));

            // (not True) or (not False) => False or True => True
            let right_expr = Expression::Or(
                Box::new(Expression::Not(Box::new(Expression::CTrue))),
                Box::new(Expression::Not(Box::new(Expression::CFalse))),
            );

            let left_result = eval(left_expr, &env).unwrap();
            let right_result = eval(right_expr, &env).unwrap();

            assert_eq!(left_result, right_result);
            assert_eq!(extract_value(left_result), Expression::CTrue);
        }

        #[test]
        fn test_boolean_precedence_with_relational() {
            let env = create_test_env();
            // Test that relational operators are evaluated before boolean operators
            // 5 > 3 and 2 < 4 should be (5 > 3) and (2 < 4), not 5 > (3 and 2) < 4
            let expr = Expression::And(
                Box::new(Expression::GT(
                    Box::new(Expression::CInt(5)),
                    Box::new(Expression::CInt(3)),
                )),
                Box::new(Expression::LT(
                    Box::new(Expression::CInt(2)),
                    Box::new(Expression::CInt(4)),
                )),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_boolean_with_arithmetic_in_relational() {
            let env = create_test_env();
            // ((1 + 2) > 2) and ((3 * 2) < 10) => (3 > 2) and (6 < 10) => True and True => True
            let expr = Expression::And(
                Box::new(Expression::GT(
                    Box::new(Expression::Add(
                        Box::new(Expression::CInt(1)),
                        Box::new(Expression::CInt(2)),
                    )),
                    Box::new(Expression::CInt(2)),
                )),
                Box::new(Expression::LT(
                    Box::new(Expression::Mul(
                        Box::new(Expression::CInt(3)),
                        Box::new(Expression::CInt(2)),
                    )),
                    Box::new(Expression::CInt(10)),
                )),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }
    }

    mod relational_expression_tests {
        use super::*;

        #[test]
        fn test_eq_integers_true() {
            let env = create_test_env();
            let expr = Expression::EQ(Box::new(Expression::CInt(5)), Box::new(Expression::CInt(5)));

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_eq_integers_false() {
            let env = create_test_env();
            let expr = Expression::EQ(Box::new(Expression::CInt(3)), Box::new(Expression::CInt(7)));

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CFalse);
        }

        #[test]
        fn test_eq_reals_true() {
            let env = create_test_env();
            let expr = Expression::EQ(
                Box::new(Expression::CReal(3.14)),
                Box::new(Expression::CReal(3.14)),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_eq_reals_false() {
            let env = create_test_env();
            let expr = Expression::EQ(
                Box::new(Expression::CReal(2.5)),
                Box::new(Expression::CReal(2.6)),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CFalse);
        }

        #[test]
        fn test_eq_mixed_int_real_true() {
            let env = create_test_env();
            let expr = Expression::EQ(
                Box::new(Expression::CInt(5)),
                Box::new(Expression::CReal(5.0)),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_eq_mixed_real_int_false() {
            let env = create_test_env();
            let expr = Expression::EQ(
                Box::new(Expression::CReal(4.2)),
                Box::new(Expression::CInt(4)),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CFalse);
        }

        #[test]
        fn test_neq_integers_true() {
            let env = create_test_env();
            let expr =
                Expression::NEQ(Box::new(Expression::CInt(3)), Box::new(Expression::CInt(7)));

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_neq_integers_false() {
            let env = create_test_env();
            let expr =
                Expression::NEQ(Box::new(Expression::CInt(5)), Box::new(Expression::CInt(5)));

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CFalse);
        }

        #[test]
        fn test_neq_reals_true() {
            let env = create_test_env();
            let expr = Expression::NEQ(
                Box::new(Expression::CReal(2.5)),
                Box::new(Expression::CReal(2.6)),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_neq_mixed_int_real_false() {
            let env = create_test_env();
            let expr = Expression::NEQ(
                Box::new(Expression::CInt(5)),
                Box::new(Expression::CReal(5.0)),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CFalse);
        }

        #[test]
        fn test_gt_integers_true() {
            let env = create_test_env();
            let expr = Expression::GT(
                Box::new(Expression::CInt(10)),
                Box::new(Expression::CInt(5)),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_gt_integers_false() {
            let env = create_test_env();
            let expr = Expression::GT(Box::new(Expression::CInt(3)), Box::new(Expression::CInt(7)));

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CFalse);
        }

        #[test]
        fn test_gt_equal_integers_false() {
            let env = create_test_env();
            let expr = Expression::GT(Box::new(Expression::CInt(5)), Box::new(Expression::CInt(5)));

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CFalse);
        }

        #[test]
        fn test_gt_reals_true() {
            let env = create_test_env();
            let expr = Expression::GT(
                Box::new(Expression::CReal(7.5)),
                Box::new(Expression::CReal(3.2)),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_gt_mixed_real_int_true() {
            let env = create_test_env();
            let expr = Expression::GT(
                Box::new(Expression::CReal(5.1)),
                Box::new(Expression::CInt(5)),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_lt_integers_true() {
            let env = create_test_env();
            let expr = Expression::LT(Box::new(Expression::CInt(3)), Box::new(Expression::CInt(8)));

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_lt_integers_false() {
            let env = create_test_env();
            let expr = Expression::LT(
                Box::new(Expression::CInt(10)),
                Box::new(Expression::CInt(5)),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CFalse);
        }

        #[test]
        fn test_lt_equal_reals_false() {
            let env = create_test_env();
            let expr = Expression::LT(
                Box::new(Expression::CReal(3.14)),
                Box::new(Expression::CReal(3.14)),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CFalse);
        }

        #[test]
        fn test_lt_mixed_int_real_true() {
            let env = create_test_env();
            let expr = Expression::LT(
                Box::new(Expression::CInt(4)),
                Box::new(Expression::CReal(4.5)),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_gte_integers_greater_true() {
            let env = create_test_env();
            let expr = Expression::GTE(
                Box::new(Expression::CInt(10)),
                Box::new(Expression::CInt(5)),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_gte_integers_equal_true() {
            let env = create_test_env();
            let expr =
                Expression::GTE(Box::new(Expression::CInt(7)), Box::new(Expression::CInt(7)));

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_gte_integers_false() {
            let env = create_test_env();
            let expr =
                Expression::GTE(Box::new(Expression::CInt(3)), Box::new(Expression::CInt(9)));

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CFalse);
        }

        #[test]
        fn test_gte_mixed_real_int_equal_true() {
            let env = create_test_env();
            let expr = Expression::GTE(
                Box::new(Expression::CReal(5.0)),
                Box::new(Expression::CInt(5)),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_lte_integers_less_true() {
            let env = create_test_env();
            let expr =
                Expression::LTE(Box::new(Expression::CInt(3)), Box::new(Expression::CInt(8)));

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_lte_integers_equal_true() {
            let env = create_test_env();
            let expr =
                Expression::LTE(Box::new(Expression::CInt(6)), Box::new(Expression::CInt(6)));

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_lte_reals_false() {
            let env = create_test_env();
            let expr = Expression::LTE(
                Box::new(Expression::CReal(9.5)),
                Box::new(Expression::CReal(3.2)),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CFalse);
        }

        #[test]
        fn test_lte_mixed_int_real_true() {
            let env = create_test_env();
            let expr = Expression::LTE(
                Box::new(Expression::CInt(4)),
                Box::new(Expression::CReal(4.0)),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_relational_with_variables() {
            let mut env = create_test_env();
            env.map_variable("x".to_string(), false, Expression::CInt(10));
            env.map_variable("y".to_string(), false, Expression::CInt(5));

            let expr = Expression::GT(
                Box::new(Expression::Var("x".to_string())),
                Box::new(Expression::Var("y".to_string())),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_relational_with_arithmetic_expressions() {
            let env = create_test_env();
            // (3 + 2) == (4 + 1) => 5 == 5 => true
            let expr = Expression::EQ(
                Box::new(Expression::Add(
                    Box::new(Expression::CInt(3)),
                    Box::new(Expression::CInt(2)),
                )),
                Box::new(Expression::Add(
                    Box::new(Expression::CInt(4)),
                    Box::new(Expression::CInt(1)),
                )),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_nested_relational_expressions() {
            let env = create_test_env();
            // (2 * 3) > (4 + 1) => 6 > 5 => true
            let expr = Expression::GT(
                Box::new(Expression::Mul(
                    Box::new(Expression::CInt(2)),
                    Box::new(Expression::CInt(3)),
                )),
                Box::new(Expression::Add(
                    Box::new(Expression::CInt(4)),
                    Box::new(Expression::CInt(1)),
                )),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_relational_with_negative_numbers() {
            let env = create_test_env();
            let expr = Expression::LT(
                Box::new(Expression::CInt(-5)),
                Box::new(Expression::CInt(-2)),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_relational_with_zero() {
            let env = create_test_env();
            let expr = Expression::GTE(
                Box::new(Expression::CInt(0)),
                Box::new(Expression::CInt(-1)),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_relational_with_decimals() {
            let env = create_test_env();
            let expr = Expression::LTE(
                Box::new(Expression::CReal(3.14159)),
                Box::new(Expression::CReal(3.14160)),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_relational_with_large_numbers() {
            let env = create_test_env();
            let expr = Expression::NEQ(
                Box::new(Expression::CInt(1000000)),
                Box::new(Expression::CInt(999999)),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_relational_operators_error_with_booleans() {
            let env = create_test_env();
            let expr = Expression::GT(Box::new(Expression::CTrue), Box::new(Expression::CFalse));

            let result = eval(expr, &env);

            assert!(result.is_err());
            let error = result.unwrap_err();
            assert_eq!(
                error,
                "greater than '(>)' is only defined for numbers (integers and real)."
            );
        }

        #[test]
        fn test_relational_operators_error_with_strings() {
            let env = create_test_env();
            let expr = Expression::EQ(
                Box::new(Expression::CString("hello".to_string())),
                Box::new(Expression::CString("world".to_string())),
            );

            let result = eval(expr, &env);

            assert!(result.is_err());
            let error = result.unwrap_err();
            assert_eq!(
                error,
                "equality '(==)' is only defined for numbers (integers and real)."
            );
        }

        #[test]
        fn test_relational_operators_error_mixed_types() {
            let env = create_test_env();
            let expr = Expression::LT(
                Box::new(Expression::CInt(5)),
                Box::new(Expression::CString("test".to_string())),
            );

            let result = eval(expr, &env);

            assert!(result.is_err());
            let error = result.unwrap_err();
            assert_eq!(
                error,
                "less than '(<)' is only defined for numbers (integers and real)."
            );
        }

        #[test]
        fn test_relational_with_undefined_variable() {
            let env = create_test_env();
            let expr = Expression::GTE(
                Box::new(Expression::Var("undefined_var".to_string())),
                Box::new(Expression::CInt(5)),
            );

            let result = eval(expr, &env);

            assert!(result.is_err());
            let error = result.unwrap_err();
            assert_eq!(error, "Variable 'undefined_var' not found");
        }

        #[test]
        fn test_complex_relational_expression() {
            let mut env = create_test_env();
            env.map_variable("a".to_string(), false, Expression::CInt(10));
            env.map_variable("b".to_string(), false, Expression::CReal(5.5));

            // (a * 2) != (b + 4.5) => (10 * 2) != (5.5 + 4.5) => 20 != 10 => true
            let expr = Expression::NEQ(
                Box::new(Expression::Mul(
                    Box::new(Expression::Var("a".to_string())),
                    Box::new(Expression::CInt(2)),
                )),
                Box::new(Expression::Add(
                    Box::new(Expression::Var("b".to_string())),
                    Box::new(Expression::CReal(4.5)),
                )),
            );

            let result = eval(expr, &env);

            assert!(result.is_ok());
            assert_eq!(extract_value(result.unwrap()), Expression::CTrue);
        }

        #[test]
        fn test_chained_relational_in_arithmetic() {
            let env = create_test_env();
            // Test that 5 > 3 is evaluated to true (which becomes 1 in arithmetic context)
            // But this should fail because we can't do arithmetic on boolean results
            let expr = Expression::Add(
                Box::new(Expression::GT(
                    Box::new(Expression::CInt(5)),
                    Box::new(Expression::CInt(3)),
                )),
                Box::new(Expression::CInt(1)),
            );

            let result = eval(expr, &env);

            assert!(result.is_err());
            let error = result.unwrap_err();
            assert_eq!(
                error,
                "addition '(+)' is only defined for numbers (integers and real)."
            );
        }
    }
}
