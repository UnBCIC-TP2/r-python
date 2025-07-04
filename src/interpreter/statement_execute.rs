use super::expression_eval::{eval, ExpressionResult};
use crate::environment::environment::Environment;
use crate::environment::environment::TestResult;
use crate::ir::ast::{Expression, Statement};

pub enum Computation {
    Continue(Environment<Expression>),
    Return(Expression, Environment<Expression>),
    PropagateError(Expression, Environment<Expression>),
}

pub fn _execute_with_env_(
    stmt: Statement,
    env: &mut Environment<Expression>,
) -> Result<Environment<Expression>, String> {
    match execute(stmt, &env.clone()) {
        Ok(Computation::Continue(new_env)) => Ok(new_env),
        Ok(Computation::Return(_, new_env)) => Ok(new_env), // For backward compatibility
        Ok(Computation::PropagateError(_, new_env)) => Ok(new_env), // For backward compatibility
        Err(e) => Err(e),
    }
}

pub fn run(
    stmt: Statement,
    env: &Environment<Expression>,
) -> Result<Environment<Expression>, String> {
    match execute(stmt, env) {
        Ok(Computation::Continue(new_env)) => Ok(new_env),
        Ok(Computation::Return(_, new_env)) => Ok(new_env),
        Ok(Computation::PropagateError(_, new_env)) => Ok(new_env),
        Err(e) => Err(e),
    }
}

//TODO: Apresentar RunTests
pub fn run_tests(stmt: &Statement) -> Result<Vec<TestResult>, String> {
    let env = match run(stmt.clone(), &Environment::new()) {
        Ok(env) => env,
        Err(e) => return Err(e),
    };

    let mut results = Vec::new();

    for test in env.get_all_tests() {
        let mut test_env = env.clone();
        test_env.push();

        let stmt = match &test.body {
            Some(body) => *body.clone(),
            None => continue,
        };

        match execute(stmt, &test_env) {
            Ok(Computation::Continue(_)) | Ok(Computation::Return(_, _)) => {
                results.push(TestResult::new(test.name.clone(), true, None));
            }
            Err(e) => {
                results.push(TestResult::new(test.name.clone(), false, Some(e)));
            }
            Ok(Computation::PropagateError(e, _)) => {
                results.push(TestResult::new(
                    test.name.clone(),
                    false,
                    Some(format!("Propagated error: {:?}", e)),
                ));
            }
        }
        test_env.pop();
    }

    Ok(results)
}

pub fn execute(stmt: Statement, env: &Environment<Expression>) -> Result<Computation, String> {
    let mut new_env = env.clone();

    match stmt {
        Statement::VarDeclaration(name, exp) => {
            let value = match eval(*exp, &new_env)? {
                ExpressionResult::Value(expr) => expr,
                ExpressionResult::Propagate(expr) => {
                    return Ok(Computation::PropagateError(expr, new_env))
                }
            };
            new_env.map_variable(name, true, value);
            Ok(Computation::Continue(new_env))
        }
        //TODO: Apresentar Asserts
        Statement::Assert(exp, msg) => {
            let value = match eval(*exp, &new_env)? {
                ExpressionResult::Value(expr) => expr,
                ExpressionResult::Propagate(expr) => {
                    return Ok(Computation::PropagateError(expr, new_env))
                }
            };

            match value {
                Expression::CTrue => Ok(Computation::Continue(new_env)),
                Expression::CFalse => {
                    // Avalia a mensagem
                    let error_msg = match eval(*msg, &new_env)? {
                        ExpressionResult::Value(Expression::CString(s)) => s,
                        ExpressionResult::Propagate(expr) => {
                            return Ok(Computation::PropagateError(expr, new_env))
                        }
                        _ => "Assertion failed".to_string(),
                    };
                    Err(error_msg)
                }
                _ => Err("Condition must evaluate to a boolean".to_string()),
            }
        }

        Statement::AssertTrue(exp, msg) => {
            let value = match eval(*exp, &new_env)? {
                ExpressionResult::Value(expr) => expr,
                ExpressionResult::Propagate(expr) => {
                    return Ok(Computation::PropagateError(expr, new_env))
                }
            };

            match value {
                Expression::CTrue => Ok(Computation::Continue(new_env)),
                Expression::CFalse => {
                    // Avalia a mensagem
                    let error_msg = match eval(*msg, &new_env)? {
                        ExpressionResult::Value(Expression::CString(s)) => s,
                        ExpressionResult::Propagate(expr) => {
                            return Ok(Computation::PropagateError(expr, new_env))
                        }
                        _ => "Assertion failed".to_string(),
                    };
                    Err(error_msg)
                }
                _ => Err("Condition must evaluate to a boolean".to_string()),
            }
        }

        Statement::AssertFalse(exp, msg) => {
            let value = match eval(*exp, &new_env)? {
                ExpressionResult::Value(expr) => expr,
                ExpressionResult::Propagate(expr) => {
                    return Ok(Computation::PropagateError(expr, new_env))
                }
            };

            match value {
                Expression::CFalse => Ok(Computation::Continue(new_env)),
                Expression::CTrue => {
                    // Avalia a mensagem
                    let error_msg = match eval(*msg, &new_env)? {
                        ExpressionResult::Value(Expression::CString(s)) => s,
                        ExpressionResult::Propagate(expr) => {
                            return Ok(Computation::PropagateError(expr, new_env))
                        }
                        _ => "Assertion failed".to_string(),
                    };
                    Err(error_msg)
                }
                _ => Err("Condition must evaluate to a boolean".to_string()),
            }
        }

        Statement::AssertEQ(exp1, exp2, msg) => {
            let value1 = match eval(*exp1, &new_env)? {
                ExpressionResult::Value(expr1) => expr1,
                ExpressionResult::Propagate(expr1) => {
                    return Ok(Computation::PropagateError(expr1, new_env))
                }
            };

            let value2 = match eval(*exp2, &new_env)? {
                ExpressionResult::Value(expr2) => expr2,
                ExpressionResult::Propagate(expr2) => {
                    return Ok(Computation::PropagateError(expr2, new_env))
                }
            };

            let comparator = Expression::EQ(Box::new(value1), Box::new(value2));

            match eval(comparator, &new_env)? {
                ExpressionResult::Value(Expression::CTrue) => Ok(Computation::Continue(new_env)),
                ExpressionResult::Value(Expression::CFalse) => {
                    // Avalia a mensagem
                    let error_msg = match eval(*msg, &new_env)? {
                        ExpressionResult::Value(Expression::CString(s)) => s,
                        ExpressionResult::Propagate(expr) => {
                            return Ok(Computation::PropagateError(expr, new_env))
                        }
                        _ => "Assertion failed".to_string(),
                    };
                    Err(error_msg)
                }
                _ => Err("Condition must evaluate to a boolean".to_string()),
            }
        }

        Statement::AssertNEQ(exp1, exp2, msg) => {
            let value1 = match eval(*exp1, &new_env)? {
                ExpressionResult::Value(expr1) => expr1,
                ExpressionResult::Propagate(expr1) => {
                    return Ok(Computation::PropagateError(expr1, new_env))
                }
            };

            let value2 = match eval(*exp2, &new_env)? {
                ExpressionResult::Value(expr2) => expr2,
                ExpressionResult::Propagate(expr2) => {
                    return Ok(Computation::PropagateError(expr2, new_env))
                }
            };

            let comparator = Expression::NEQ(Box::new(value1), Box::new(value2));

            match eval(comparator, &new_env)? {
                ExpressionResult::Value(Expression::CTrue) => Ok(Computation::Continue(new_env)),
                ExpressionResult::Value(Expression::CFalse) => {
                    // Avalia a mensagem
                    let error_msg = match eval(*msg, &new_env)? {
                        ExpressionResult::Value(Expression::CString(s)) => s,
                        ExpressionResult::Propagate(expr) => {
                            return Ok(Computation::PropagateError(expr, new_env))
                        }
                        _ => "Assertion failed".to_string(),
                    };
                    Err(error_msg)
                }
                _ => Err("Condition must evaluate to a boolean".to_string()),
            }
        }

        Statement::ValDeclaration(name, exp) => {
            let value = match eval(*exp, &new_env)? {
                ExpressionResult::Value(expr) => expr,
                ExpressionResult::Propagate(expr) => {
                    return Ok(Computation::PropagateError(expr, new_env))
                }
            };
            new_env.map_variable(name, false, value);
            Ok(Computation::Continue(new_env))
        }

        Statement::Assignment(name, exp) => {
            let value = match eval(*exp, &new_env)? {
                ExpressionResult::Value(expr) => expr,
                ExpressionResult::Propagate(expr) => {
                    return Ok(Computation::PropagateError(expr, new_env))
                }
            };
            new_env.map_variable(name, true, value);
            Ok(Computation::Continue(new_env))
        }

        Statement::IfThenElse(cond, stmt_then, stmt_else) => {
            let value = match eval(*cond, &new_env)? {
                ExpressionResult::Value(expr) => expr,
                ExpressionResult::Propagate(expr) => {
                    return Ok(Computation::PropagateError(expr, new_env))
                }
            };

            match value {
                Expression::CTrue => match *stmt_then {
                    Statement::Block(stmts) => execute_block(stmts, &new_env),
                    _ => execute(*stmt_then, &new_env),
                },
                Expression::CFalse => match stmt_else {
                    Some(else_stmt) => match *else_stmt {
                        Statement::Block(stmts) => execute_block(stmts, &new_env),
                        _ => execute(*else_stmt, &new_env),
                    },
                    None => Ok(Computation::Continue(new_env)),
                },
                _ => Err("Condition must evaluate to a boolean".to_string()),
            }
        }

        Statement::Block(stmts) => {
            new_env.push();
            let result = execute_block(stmts, &new_env);
            new_env.pop();
            result
        }

        Statement::While(cond, stmt) => {
            let mut value = match eval(*cond.clone(), &new_env)? {
                ExpressionResult::Value(expr) => expr,
                ExpressionResult::Propagate(expr) => {
                    return Ok(Computation::PropagateError(expr, new_env))
                }
            };

            loop {
                match value {
                    Expression::CTrue => {
                        match execute(*stmt.clone(), &new_env)? {
                            Computation::Continue(env) => new_env = env,
                            Computation::Return(expr, env) => {
                                return Ok(Computation::Return(expr, env))
                            }
                            Computation::PropagateError(expr, env) => {
                                return Ok(Computation::PropagateError(expr, env))
                            }
                        }
                        value = match eval(*cond.clone(), &new_env)? {
                            ExpressionResult::Value(expr) => expr,
                            ExpressionResult::Propagate(expr) => {
                                return Ok(Computation::PropagateError(expr, new_env))
                            }
                        };
                    }
                    Expression::CFalse => return Ok(Computation::Continue(new_env)),
                    _ => unreachable!(),
                }
            }
        }

        Statement::For(var, list, stmt) => {
            let values = match eval(*list.clone(), &new_env)? {
                ExpressionResult::Value(expr) => expr,
                ExpressionResult::Propagate(expr) => {
                    return Ok(Computation::PropagateError(expr, new_env))
                }
            };

            match values {
                Expression::ListValue(expressions) => {
                    for exp in expressions {
                        new_env.map_variable(var.clone(), false, exp);
                        match execute(*stmt.clone(), &new_env)? {
                            Computation::Continue(env) => new_env = env,
                            Computation::Return(expr, env) => {
                                return Ok(Computation::Return(expr, env))
                            }
                            Computation::PropagateError(expr, env) => {
                                return Ok(Computation::PropagateError(expr, env))
                            }
                        }
                    }
                    return Ok(Computation::Continue(new_env));
                }
                _ => unreachable!(),
            }
        }

        Statement::Sequence(s1, s2) => {
            match execute(*s1, &new_env)? {
                Computation::Continue(env) => new_env = env,
                Computation::Return(expr, env) => return Ok(Computation::Return(expr, env)),
                Computation::PropagateError(expr, env) => {
                    return Ok(Computation::PropagateError(expr, env))
                }
            }
            execute(*s2, &new_env)
        }

        Statement::FuncDef(func) => {
            new_env.map_function(func.clone());
            Ok(Computation::Continue(new_env))
        }

        //TODO: Apresentar TesteDef
        Statement::TestDef(teste) => {
            new_env.map_test(teste.clone());
            Ok(Computation::Continue(new_env))
        }

        Statement::Return(exp) => {
            let exp_value = match eval(*exp, &new_env)? {
                ExpressionResult::Value(expr) => expr,
                ExpressionResult::Propagate(expr) => {
                    return Ok(Computation::PropagateError(expr, new_env))
                }
            };
            Ok(Computation::Return(exp_value, new_env))
        }

        Statement::TypeDeclaration(name, constructors) => {
            new_env.map_adt(name, constructors);
            Ok(Computation::Continue(new_env))
        }

        _ => Err(String::from("not implemented yet")),
    }
}

pub fn execute_block(
    stmts: Vec<Statement>,
    env: &Environment<Expression>,
) -> Result<Computation, String> {
    let mut current_env = env.clone();

    for stmt in stmts {
        match execute(stmt, &current_env)? {
            Computation::Continue(new_env) => current_env = new_env,
            Computation::Return(expr, env) => return Ok(Computation::Return(expr, env)),
            Computation::PropagateError(expr, env) => {
                return Ok(Computation::PropagateError(expr, env))
            }
        }
    }
    Ok(Computation::Continue(current_env))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::ast::*;

    fn create_test_env() -> Environment<Expression> {
        Environment::new()
    }

    fn extract_env(computation: Computation) -> Environment<Expression> {
        match computation {
            Computation::Continue(env) => env,
            Computation::Return(_, env) => env,
            Computation::PropagateError(_, env) => env,
        }
    }

    mod assignment_tests {
        use super::*;

        #[test]
        fn test_var_declaration_and_assignment() {
            let env = create_test_env();

            // First declare a variable
            let var_decl =
                Statement::VarDeclaration("x".to_string(), Box::new(Expression::CInt(42)));

            let env_with_var = extract_env(execute(var_decl, &env).unwrap());

            // Check that the variable was declared correctly
            let x_value = env_with_var.lookup(&"x".to_string());
            assert!(x_value.is_some());
            let (is_mutable, x_expr) = x_value.unwrap();
            assert_eq!(x_expr, Expression::CInt(42));
            assert!(is_mutable); // Should be mutable

            // Now assign a new value to the variable
            let assignment =
                Statement::Assignment("x".to_string(), Box::new(Expression::CInt(100)));

            let result_env = execute(assignment, &env_with_var);

            assert!(result_env.is_ok());
            let final_env = extract_env(result_env.unwrap());

            let final_x_value = final_env.lookup(&"x".to_string());
            assert!(final_x_value.is_some());
            let (is_still_mutable, final_x_expr) = final_x_value.unwrap();
            assert_eq!(final_x_expr, Expression::CInt(100));
            assert!(is_still_mutable); // Should still be mutable
        }

        #[test]
        fn test_val_declaration_immutable() {
            let env = create_test_env();

            // Declare an immutable value
            let val_decl = Statement::ValDeclaration(
                "message".to_string(),
                Box::new(Expression::CString("Hello, World!".to_string())),
            );

            let result_env = execute(val_decl, &env);

            assert!(result_env.is_ok());
            let final_env = extract_env(result_env.unwrap());

            let message_value = final_env.lookup(&"message".to_string());
            assert!(message_value.is_some());
            let (is_mutable, message_expr) = message_value.unwrap();
            assert_eq!(
                message_expr,
                Expression::CString("Hello, World!".to_string())
            );
            assert!(!is_mutable); // Should be immutable
        }

        #[test]
        fn test_var_declaration_with_reassignment() {
            let env = create_test_env();

            // Declare a mutable boolean variable
            let var_decl =
                Statement::VarDeclaration("flag".to_string(), Box::new(Expression::CTrue));

            let env_with_var = extract_env(execute(var_decl, &env).unwrap());

            let flag_value = env_with_var.lookup(&"flag".to_string());
            assert!(flag_value.is_some());
            let (is_mutable, flag_expr) = flag_value.unwrap();
            assert_eq!(flag_expr, Expression::CTrue);
            assert!(is_mutable); // Should be mutable

            // Test reassignment with false
            let assignment_false =
                Statement::Assignment("flag".to_string(), Box::new(Expression::CFalse));

            let result_env = execute(assignment_false, &env_with_var);
            assert!(result_env.is_ok());
            let final_env = extract_env(result_env.unwrap());

            let flag_value2 = final_env.lookup(&"flag".to_string());
            assert!(flag_value2.is_some());
            let (is_still_mutable, flag_expr2) = flag_value2.unwrap();
            assert_eq!(flag_expr2, Expression::CFalse);
            assert!(is_still_mutable); // Should still be mutable
        }

        #[test]
        fn test_val_declaration_real_number() {
            let env = create_test_env();

            let val_decl =
                Statement::ValDeclaration("pi".to_string(), Box::new(Expression::CReal(3.14159)));

            let result_env = execute(val_decl, &env);

            assert!(result_env.is_ok());
            let final_env = extract_env(result_env.unwrap());

            let pi_value = final_env.lookup(&"pi".to_string());
            assert!(pi_value.is_some());
            let (is_mutable, pi_expr) = pi_value.unwrap();
            assert_eq!(pi_expr, Expression::CReal(3.14159));
            assert!(!is_mutable); // val should be immutable
        }

        #[test]
        fn test_var_declaration_with_arithmetic_expression() {
            let env = create_test_env();

            let var_decl = Statement::VarDeclaration(
                "result".to_string(),
                Box::new(Expression::Add(
                    Box::new(Expression::CInt(10)),
                    Box::new(Expression::CInt(5)),
                )),
            );

            let result_env = execute(var_decl, &env);

            assert!(result_env.is_ok());
            let final_env = extract_env(result_env.unwrap());

            let result_value = final_env.lookup(&"result".to_string());
            assert!(result_value.is_some());
            let (is_mutable, result_expr) = result_value.unwrap();
            assert_eq!(result_expr, Expression::CInt(15));
            assert!(is_mutable); // var should be mutable
        }

        #[test]
        fn test_val_declaration_with_complex_expression() {
            let env = create_test_env();

            // result = (2 + 3) * (4 - 1)
            let val_decl = Statement::ValDeclaration(
                "result".to_string(),
                Box::new(Expression::Mul(
                    Box::new(Expression::Add(
                        Box::new(Expression::CInt(2)),
                        Box::new(Expression::CInt(3)),
                    )),
                    Box::new(Expression::Sub(
                        Box::new(Expression::CInt(4)),
                        Box::new(Expression::CInt(1)),
                    )),
                )),
            );

            let result_env = execute(val_decl, &env);

            assert!(result_env.is_ok());
            let final_env = extract_env(result_env.unwrap());

            let result_value = final_env.lookup(&"result".to_string());
            assert!(result_value.is_some());
            let (is_mutable, result_expr) = result_value.unwrap();
            assert_eq!(result_expr, Expression::CInt(15)); // (2+3) * (4-1) = 5 * 3 = 15
            assert!(!is_mutable); // val should be immutable
        }

        #[test]
        fn test_assignment_using_existing_variable() {
            let env = create_test_env();

            // First declare x as a variable: var x = 10
            let var_decl =
                Statement::VarDeclaration("x".to_string(), Box::new(Expression::CInt(10)));

            let env_with_x = extract_env(execute(var_decl, &env).unwrap());

            // Second declare y using x: var y = x + 5
            let var_decl_y = Statement::VarDeclaration(
                "y".to_string(),
                Box::new(Expression::Add(
                    Box::new(Expression::Var("x".to_string())),
                    Box::new(Expression::CInt(5)),
                )),
            );

            let result_env = execute(var_decl_y, &env_with_x);

            assert!(result_env.is_ok());
            let final_env = extract_env(result_env.unwrap());

            let y_value = final_env.lookup(&"y".to_string());
            assert!(y_value.is_some());
            let (is_mutable, y_expr) = y_value.unwrap();
            assert_eq!(y_expr, Expression::CInt(15));
            assert!(is_mutable); // var should be mutable
        }

        #[test]
        fn test_variable_reassignment() {
            let env = create_test_env();

            // First declare a mutable variable: var counter = 0
            let var_decl =
                Statement::VarDeclaration("counter".to_string(), Box::new(Expression::CInt(0)));

            let env_with_counter = extract_env(execute(var_decl, &env).unwrap());

            // Verify initial value
            let counter_value = env_with_counter.lookup(&"counter".to_string());
            assert!(counter_value.is_some());
            let (is_mutable, counter_expr) = counter_value.unwrap();
            assert_eq!(counter_expr, Expression::CInt(0));
            assert!(is_mutable); // Should be mutable

            // Reassignment: counter = counter + 1
            let reassignment = Statement::Assignment(
                "counter".to_string(),
                Box::new(Expression::Add(
                    Box::new(Expression::Var("counter".to_string())),
                    Box::new(Expression::CInt(1)),
                )),
            );

            let result_env = execute(reassignment, &env_with_counter);

            assert!(result_env.is_ok());
            let final_env = extract_env(result_env.unwrap());

            let final_counter_value = final_env.lookup(&"counter".to_string());
            assert!(final_counter_value.is_some());
            let (is_still_mutable, final_counter_expr) = final_counter_value.unwrap();
            assert_eq!(final_counter_expr, Expression::CInt(1));
            assert!(is_still_mutable); // Should still be mutable
        }

        #[test]
        fn test_multiple_declarations_sequence() {
            let env = create_test_env();

            // Create a sequence of variable declarations
            let program = Statement::Sequence(
                Box::new(Statement::VarDeclaration(
                    "a".to_string(),
                    Box::new(Expression::CInt(5)),
                )),
                Box::new(Statement::Sequence(
                    Box::new(Statement::ValDeclaration(
                        "b".to_string(),
                        Box::new(Expression::CInt(10)),
                    )),
                    Box::new(Statement::VarDeclaration(
                        "c".to_string(),
                        Box::new(Expression::Add(
                            Box::new(Expression::Var("a".to_string())),
                            Box::new(Expression::Var("b".to_string())),
                        )),
                    )),
                )),
            );

            let result_env = execute(program, &env);

            assert!(result_env.is_ok());
            let final_env = extract_env(result_env.unwrap());

            // Check all variables
            let a_value = final_env.lookup(&"a".to_string());
            assert!(a_value.is_some());
            let (a_mutable, a_expr) = a_value.unwrap();
            assert_eq!(a_expr, Expression::CInt(5));
            assert!(a_mutable); // var should be mutable

            let b_value = final_env.lookup(&"b".to_string());
            assert!(b_value.is_some());
            let (b_mutable, b_expr) = b_value.unwrap();
            assert_eq!(b_expr, Expression::CInt(10));
            assert!(!b_mutable); // val should be immutable

            let c_value = final_env.lookup(&"c".to_string());
            assert!(c_value.is_some());
            let (c_mutable, c_expr) = c_value.unwrap();
            assert_eq!(c_expr, Expression::CInt(15));
            assert!(c_mutable); // var should be mutable
        }

        #[test]
        fn test_declarations_in_block() {
            let env = create_test_env();

            let block = Statement::Block(vec![
                Statement::VarDeclaration("local_var".to_string(), Box::new(Expression::CInt(100))),
                Statement::ValDeclaration(
                    "result".to_string(),
                    Box::new(Expression::Mul(
                        Box::new(Expression::Var("local_var".to_string())),
                        Box::new(Expression::CInt(2)),
                    )),
                ),
            ]);

            let result_env = execute(block, &env);

            assert!(result_env.is_ok());
            let final_env = extract_env(result_env.unwrap());

            // Both variables should be accessible after the block
            let local_var_value = final_env.lookup(&"local_var".to_string());
            assert!(local_var_value.is_some());
            let (local_var_mutable, local_var_expr) = local_var_value.unwrap();
            assert_eq!(local_var_expr, Expression::CInt(100));
            assert!(local_var_mutable); // var should be mutable

            let result_value = final_env.lookup(&"result".to_string());
            assert!(result_value.is_some());
            let (result_mutable, result_expr) = result_value.unwrap();
            assert_eq!(result_expr, Expression::CInt(200));
            assert!(!result_mutable); // val should be immutable
        }
    }

    mod for_statement_tests {
        use super::*;

        #[test]
        fn test_for_loop_sum_integers() {
            let env = create_test_env();

            // Create a list of integers: [1, 2, 3, 4, 5]
            let int_list = Expression::ListValue(vec![
                Expression::CInt(1),
                Expression::CInt(2),
                Expression::CInt(3),
                Expression::CInt(4),
                Expression::CInt(5),
            ]);

            // Create the for loop body that adds each element to sum
            // sum = sum + i
            let loop_body = Statement::Assignment(
                "sum".to_string(),
                Box::new(Expression::Add(
                    Box::new(Expression::Var("sum".to_string())),
                    Box::new(Expression::Var("i".to_string())),
                )),
            );

            // Create the for statement: for i in [1,2,3,4,5]: sum = sum + i
            let for_stmt = Statement::For("i".to_string(), Box::new(int_list), Box::new(loop_body));

            // Create a block that initializes sum to 0 and then runs the for loop
            let program = Statement::Block(vec![
                Statement::Assignment("sum".to_string(), Box::new(Expression::CInt(0))),
                for_stmt,
            ]);

            // Execute the program
            let result_env = execute(program, &env);

            // Check that execution was successful
            assert!(result_env.is_ok());
            let final_env = extract_env(result_env.unwrap());

            // Check that sum equals 15 (1+2+3+4+5)
            let sum_value = final_env.lookup(&"sum".to_string());
            assert!(sum_value.is_some());
            let (_, sum_expr) = sum_value.unwrap();
            assert_eq!(sum_expr, Expression::CInt(15));
        }

        #[test]
        fn test_for_loop_empty_list() {
            let env = create_test_env();

            // Create an empty list
            let empty_list = Expression::ListValue(vec![]);

            // Create the for loop body
            let loop_body = Statement::Assignment(
                "count".to_string(),
                Box::new(Expression::Add(
                    Box::new(Expression::Var("count".to_string())),
                    Box::new(Expression::CInt(1)),
                )),
            );

            // Create the for statement
            let for_stmt =
                Statement::For("i".to_string(), Box::new(empty_list), Box::new(loop_body));

            // Create a block that initializes count to 0 and then runs the for loop
            let program = Statement::Block(vec![
                Statement::Assignment("count".to_string(), Box::new(Expression::CInt(0))),
                for_stmt,
            ]);

            // Execute the program
            let result_env = execute(program, &env);

            // Check that execution was successful
            assert!(result_env.is_ok());
            let final_env = extract_env(result_env.unwrap());

            // Check that count is still 0 (loop body never executed)
            let count_value = final_env.lookup(&"count".to_string());
            assert!(count_value.is_some());
            let (_, count_expr) = count_value.unwrap();
            assert_eq!(count_expr, Expression::CInt(0));
        }

        #[test]
        fn test_for_loop_single_element() {
            let env = create_test_env();

            // Create a list with a single element: [42]
            let single_list = Expression::ListValue(vec![Expression::CInt(42)]);

            // Create the for loop body that assigns the element to result
            let loop_body = Statement::Assignment(
                "result".to_string(),
                Box::new(Expression::Var("i".to_string())),
            );

            // Create the for statement
            let for_stmt =
                Statement::For("i".to_string(), Box::new(single_list), Box::new(loop_body));

            // Execute the for loop
            let result_env = execute(for_stmt, &env);

            // Check that execution was successful
            assert!(result_env.is_ok());
            let final_env = extract_env(result_env.unwrap());

            // Check that result equals 42
            let result_value = final_env.lookup(&"result".to_string());
            assert!(result_value.is_some());
            let (_, result_expr) = result_value.unwrap();
            assert_eq!(result_expr, Expression::CInt(42));

            // Check that loop variable i is still accessible with the last value
            let i_value = final_env.lookup(&"i".to_string());
            assert!(i_value.is_some());
            let (_, i_expr) = i_value.unwrap();
            assert_eq!(i_expr, Expression::CInt(42));
        }

        #[test]
        fn test_for_loop_with_expressions() {
            let env = create_test_env();

            // Create a list containing expressions that evaluate to integers
            let expr_list = Expression::ListValue(vec![
                Expression::Add(Box::new(Expression::CInt(1)), Box::new(Expression::CInt(1))), // 2
                Expression::Mul(Box::new(Expression::CInt(2)), Box::new(Expression::CInt(3))), // 6
                Expression::Sub(
                    Box::new(Expression::CInt(10)),
                    Box::new(Expression::CInt(2)),
                ), // 8
            ]);

            // Create the for loop body that multiplies product by each element
            let loop_body = Statement::Assignment(
                "product".to_string(),
                Box::new(Expression::Mul(
                    Box::new(Expression::Var("product".to_string())),
                    Box::new(Expression::Var("i".to_string())),
                )),
            );

            // Create the for statement
            let for_stmt =
                Statement::For("i".to_string(), Box::new(expr_list), Box::new(loop_body));

            // Create a block that initializes product to 1 and then runs the for loop
            let program = Statement::Block(vec![
                Statement::Assignment("product".to_string(), Box::new(Expression::CInt(1))),
                for_stmt,
            ]);

            // Execute the program
            let result_env = execute(program, &env);

            // Check that execution was successful
            assert!(result_env.is_ok());
            let final_env = extract_env(result_env.unwrap());

            // Check that product equals 96 (1 * 2 * 6 * 8)
            let product_value = final_env.lookup(&"product".to_string());
            assert!(product_value.is_some());
            let (_, product_expr) = product_value.unwrap();
            assert_eq!(product_expr, Expression::CInt(96));
        }

        #[test]
        fn test_for_loop_nested_blocks() {
            let env = create_test_env();

            // Create a list: [1, 2, 3]
            let int_list = Expression::ListValue(vec![
                Expression::CInt(1),
                Expression::CInt(2),
                Expression::CInt(3),
            ]);

            // Create a nested block as loop body
            let loop_body = Statement::Block(vec![
                Statement::Assignment(
                    "temp".to_string(),
                    Box::new(Expression::Mul(
                        Box::new(Expression::Var("i".to_string())),
                        Box::new(Expression::CInt(2)),
                    )),
                ),
                Statement::Assignment(
                    "sum".to_string(),
                    Box::new(Expression::Add(
                        Box::new(Expression::Var("sum".to_string())),
                        Box::new(Expression::Var("temp".to_string())),
                    )),
                ),
            ]);

            // Create the for statement
            let for_stmt = Statement::For("i".to_string(), Box::new(int_list), Box::new(loop_body));

            // Create a program that initializes sum and runs the for loop
            let program = Statement::Block(vec![
                Statement::Assignment("sum".to_string(), Box::new(Expression::CInt(0))),
                for_stmt,
            ]);

            // Execute the program
            let result_env = execute(program, &env);

            // Check that execution was successful
            assert!(result_env.is_ok());
            let final_env = extract_env(result_env.unwrap());

            // Check that sum equals 12 (2 + 4 + 6)
            let sum_value = final_env.lookup(&"sum".to_string());
            assert!(sum_value.is_some());
            let (_, sum_expr) = sum_value.unwrap();
            assert_eq!(sum_expr, Expression::CInt(12));
        }
    }

    //TODO: Apresentar Interpretador Asserts (Tests)
    mod assert_statement_tests {
        use super::*;

        #[test]
        fn test_execute_assert_true() {
            let env = create_test_env();
            let stmt = Statement::Assert(
                Box::new(Expression::CTrue),
                Box::new(Expression::CString("ok".to_string())),
            );
            let result = execute(stmt, &env);
            assert!(result.is_ok(), "Assert with true condition should succeed");
        }

        #[test]
        fn test_execute_assert_false() {
            let env = create_test_env();
            let stmt = Statement::Assert(
                Box::new(Expression::CFalse),
                Box::new(Expression::CString("fail msg".to_string())),
            );
            let result = execute(stmt.clone(), &env);
            assert!(result.is_err(), "Assert with false condition should fail");
            //assert_eq!(result.unwrap_err(), "fail msg");
            let computation = match execute(stmt, &env) {
                Ok(Computation::Continue(_)) => "error".to_string(),
                Ok(Computation::Return(_, _)) => "error".to_string(),
                Ok(Computation::PropagateError(_, _)) => "error".to_string(),
                Err(e) => e.to_string(),
            };
            assert_eq!(computation, "fail msg".to_string());
        }

        #[test]
        fn test_execute_asserteq_true() {
            let env = create_test_env();
            let stmt = Statement::AssertEQ(
                Box::new(Expression::CInt(1)),
                Box::new(Expression::CInt(1)),
                Box::new(Expression::CString("should not fail".to_string())),
            );
            let result = execute(stmt, &env);
            assert!(result.is_ok(), "AssertEQ with equal values should succeed");
        }

        #[test]
        fn test_execute_asserteq_false() {
            let env = create_test_env();
            let stmt = Statement::AssertEQ(
                Box::new(Expression::CInt(1)),
                Box::new(Expression::CInt(2)),
                Box::new(Expression::CString("eq fail".to_string())),
            );
            let result = execute(stmt.clone(), &env);
            assert!(
                result.is_err(),
                "AssertEQ with different values should fail"
            );
            //assert_eq!(result.unwrap_err(), "eq fail");

            let computation = match execute(stmt, &env) {
                Ok(Computation::Continue(_)) => "error".to_string(),
                Ok(Computation::Return(_, _)) => "error".to_string(),
                Ok(Computation::PropagateError(_, _)) => "error".to_string(),
                Err(e) => e.to_string(),
            };
            assert_eq!(computation, "eq fail".to_string());
        }

        #[test]
        fn test_execute_assertneq_true() {
            let env = create_test_env();
            let stmt = Statement::AssertNEQ(
                Box::new(Expression::CInt(1)),
                Box::new(Expression::CInt(2)),
                Box::new(Expression::CString("should not fail".to_string())),
            );
            let result = execute(stmt, &env);
            assert!(
                result.is_ok(),
                "AssertNEQ with different values should succeed"
            );
        }

        #[test]
        fn test_execute_assertneq_false() {
            let env = create_test_env();
            let stmt = Statement::AssertNEQ(
                Box::new(Expression::CInt(3)),
                Box::new(Expression::CInt(3)),
                Box::new(Expression::CString("neq fail".to_string())),
            );
            let result = execute(stmt.clone(), &env);
            assert!(result.is_err(), "AssertNEQ with equal values should fail");
            //assert_eq!(result.unwrap_err(), "neq fail");

            let computation = match execute(stmt, &env) {
                Ok(Computation::Continue(_)) => "error".to_string(),
                Ok(Computation::Return(_, _)) => "error".to_string(),
                Ok(Computation::PropagateError(_, _)) => "error".to_string(),
                Err(e) => e.to_string(),
            };
            assert_eq!(computation, "neq fail".to_string());
        }

        #[test]
        fn test_execute_asserttrue_true() {
            let env = create_test_env();
            let stmt = Statement::AssertTrue(
                Box::new(Expression::CTrue),
                Box::new(Expression::CString("ok".to_string())),
            );
            let result = execute(stmt, &env);
            assert!(
                result.is_ok(),
                "AssertTrue with true condition should succeed"
            );
        }

        #[test]
        fn test_execute_asserttrue_false() {
            let env = create_test_env();
            let stmt = Statement::AssertTrue(
                Box::new(Expression::CFalse),
                Box::new(Expression::CString("asserttrue fail".to_string())),
            );
            let result = execute(stmt.clone(), &env);
            assert!(
                result.is_err(),
                "AssertTrue with false condition should fail"
            );
            //assert_eq!(result.unwrap_err(), "asserttrue fail");

            let computation = match execute(stmt, &env) {
                Ok(Computation::Continue(_)) => "error".to_string(),
                Ok(Computation::Return(_, _)) => "error".to_string(),
                Ok(Computation::PropagateError(_, _)) => "error".to_string(),
                Err(e) => e.to_string(),
            };
            assert_eq!(computation, "asserttrue fail".to_string());
        }

        #[test]
        fn test_execute_assertfalse_false() {
            let env = create_test_env();
            let stmt = Statement::AssertFalse(
                Box::new(Expression::CFalse),
                Box::new(Expression::CString("ok".to_string())),
            );
            let result = execute(stmt, &env);
            assert!(
                result.is_ok(),
                "AssertFalse with false condition should succeed"
            );
        }

        #[test]
        fn test_execute_assertfalse_true() {
            let env = create_test_env();
            let stmt = Statement::AssertFalse(
                Box::new(Expression::CTrue),
                Box::new(Expression::CString("assertfalse fail".to_string())),
            );
            let result = execute(stmt.clone(), &env);
            assert!(
                result.is_err(),
                "AssertFalse with true condition should fail"
            );
            //assert_eq!(result.unwrap_err(), "assertfalse fail");
            let computation = match execute(stmt, &env) {
                Ok(Computation::Continue(_)) => "error".to_string(),
                Ok(Computation::Return(_, _)) => "error".to_string(),
                Ok(Computation::PropagateError(_, _)) => "error".to_string(),
                Err(e) => e.to_string(),
            };
            assert_eq!(computation, "assertfalse fail".to_string());
        }
    }

    //TODO: Apresentar Interpretador TestDef (Tests)
    mod testdef_statement_tests {
        use super::*;

        #[test]
        fn test_execute_testdef() {
            let env = create_test_env();
            let test_def = Statement::TestDef(Function {
                name: "test_example".to_string(),
                kind: Type::TVoid,
                params: Vec::new(),
                body: Some(Box::new(Statement::Block(vec![Statement::Assert(
                    Box::new(Expression::CTrue),
                    Box::new(Expression::CString("Test passed".to_string())),
                )]))),
            });
            let programa = Statement::Block(vec![test_def.clone()]);
            match execute(programa, &env) {
                Ok(Computation::Continue(new_env)) => {
                    assert!(new_env.lookup_test(&"test_example".to_string()).is_some());
                }
                _ => panic!("Test definition execution failed"),
            }
        }
    }

    //TODO: Apresentar Interpretador RunTests (Tests)
    mod run_tests_tests {

        use super::*;

        #[test]
        fn test_run_tests() {
            let test_def = Statement::TestDef(Function {
                name: "test_example".to_string(),
                kind: Type::TVoid,
                params: Vec::new(),
                body: Some(Box::new(Statement::Block(vec![Statement::Assert(
                    Box::new(Expression::CTrue),
                    Box::new(Expression::CString("Test passed".to_string())),
                )]))),
            });
            let programa = Statement::Block(vec![test_def.clone()]);
            match run_tests(&programa) {
                Ok(resultados) => {
                    assert_eq!(resultados.len(), 1);
                    assert_eq!(resultados[0].name, "test_example");
                    assert!(resultados[0].result);
                    assert!(resultados[0].error.is_none());
                }
                _ => panic!("Test execution failed"),
            }
        }

        #[test]
        fn test_run_tests_scope() {
            let test_def = Statement::TestDef(Function {
                name: "test_example".to_string(),
                kind: Type::TVoid,
                params: Vec::new(),
                body: Some(Box::new(Statement::Block(vec![Statement::Assert(
                    Box::new(Expression::CTrue),
                    Box::new(Expression::CString("Test passed".to_string())),
                )]))),
            });

            let teste_def2 = Statement::TestDef(Function {
                name: "test_example2".to_string(),
                kind: Type::TVoid,
                params: Vec::new(),
                body: Some(Box::new(Statement::Block(vec![Statement::Assert(
                    Box::new(Expression::CTrue),
                    Box::new(Expression::CString("Test 2 passed".to_string())),
                )]))),
            });

            let assign1 = Statement::Assignment("x".to_string(), Box::new(Expression::CInt(10)));
            let assign2 = Statement::Assignment("y".to_string(), Box::new(Expression::CInt(20)));

            let ifelse = Statement::IfThenElse(
                Box::new(Expression::CTrue),
                Box::new(test_def),
                Some(Box::new(teste_def2)),
            );

            let programa = Statement::Block(vec![assign1, assign2, ifelse]);

            let resultado_final = match run_tests(&programa) {
                Ok(resultados) => resultados,
                Err(e) => panic!("Test execution failed: {}", e),
            };

            assert_eq!(resultado_final.len(), 1);
            assert_eq!(resultado_final[0].name, "test_example");
            assert_eq!(resultado_final[0].result, true);
        }

        #[test]
        fn test_run_tests_with_assert_fail() {
            let teste1 = Statement::TestDef(Function {
                name: "test_fail".to_string(),
                kind: Type::TVoid,
                params: Vec::new(),
                body: Some(Box::new(Statement::Block(vec![Statement::Assert(
                    Box::new(Expression::CFalse),
                    Box::new(Expression::CString("This test should fail".to_string())),
                )]))),
            });
            let programa = Statement::Block(vec![teste1]);
            match run_tests(&programa) {
                Ok(resultados) => {
                    assert_eq!(resultados.len(), 1);
                    assert_eq!(resultados[0].name, "test_fail");
                    assert!(!resultados[0].result);
                    assert_eq!(
                        resultados[0].error,
                        Some("This test should fail".to_string())
                    );
                }
                Err(e) => panic!("Test execution failed: {}", e),
            }
        }

        #[test]
        fn test_run_tests_with_second_assert_fail() {
            let teste1 = Statement::TestDef(Function {
                name: "test_fail".to_string(),
                kind: Type::TVoid,
                params: Vec::new(),
                body: Some(Box::new(Statement::Block(vec![
                    Statement::Assert(
                        Box::new(Expression::CTrue),
                        Box::new(Expression::CString("This test should pass".to_string())),
                    ),
                    Statement::Assert(
                        Box::new(Expression::CFalse),
                        Box::new(Expression::CString(
                            "This second test should fail".to_string(),
                        )),
                    ),
                    Statement::Assert(
                        Box::new(Expression::CTrue),
                        Box::new(Expression::CString(
                            "This test shouldn't run, but should pass".to_string(),
                        )),
                    ),
                ]))),
            });
            let programa = Statement::Block(vec![teste1]);
            match run_tests(&programa) {
                Ok(resultados) => {
                    assert_eq!(resultados.len(), 1);
                    assert_eq!(resultados[0].name, "test_fail");
                    assert!(!resultados[0].result);
                    assert_eq!(
                        resultados[0].error,
                        Some("This second test should fail".to_string())
                    );
                }
                Err(e) => panic!("Test execution failed: {}", e),
            }
        }

        #[test]
        fn test_run_tests_without_asserts() {
            let teste = Statement::TestDef(Function {
                name: "test_no_assert".to_string(),
                kind: Type::TVoid,
                params: Vec::new(),
                body: Some(Box::new(Statement::Block(vec![Statement::VarDeclaration(
                    "x".to_string(),
                    Box::new(Expression::CInt(42)),
                )]))),
            });
            let programa = Statement::Block(vec![teste]);
            match run_tests(&programa) {
                Ok(resultados) => {
                    assert_eq!(resultados.len(), 1);
                    assert_eq!(resultados[0].name, "test_no_assert");
                    assert!(resultados[0].result);
                    assert!(resultados[0].error.is_none());
                }
                Err(e) => panic!("Test execution failed: {}", e),
            }
        }

        #[test]
        fn test_run_tests_with_multiple_tests() {
            let teste1 = Statement::TestDef(Function {
                name: "test_one".to_string(),
                kind: Type::TVoid,
                params: Vec::new(),
                body: Some(Box::new(Statement::Block(vec![Statement::Assert(
                    Box::new(Expression::CTrue),
                    Box::new(Expression::CString("Test one passed".to_string())),
                )]))),
            });
            let teste2 = Statement::TestDef(Function {
                name: "test_two".to_string(),
                kind: Type::TVoid,
                params: Vec::new(),
                body: Some(Box::new(Statement::Block(vec![Statement::Assert(
                    Box::new(Expression::CFalse),
                    Box::new(Expression::CString("Test two failed".to_string())),
                )]))),
            });
            let teste3 = Statement::TestDef(Function {
                name: "test_three".to_string(),
                kind: Type::TVoid,
                params: Vec::new(),
                body: Some(Box::new(Statement::Block(vec![Statement::Assert(
                    Box::new(Expression::CTrue),
                    Box::new(Expression::CString("Test three passed".to_string())),
                )]))),
            });
            let programa = Statement::Block(vec![teste1, teste2, teste3]);

            match run_tests(&programa) {
                Ok(resultados) => {
                    assert_eq!(resultados.len(), 3);
                    assert_eq!(resultados[0].name, "test_one");
                    assert!(resultados[0].result);
                    assert!(resultados[0].error.is_none());

                    assert_eq!(resultados[1].name, "test_two");
                    assert!(!resultados[1].result);
                    assert_eq!(resultados[1].error, Some("Test two failed".to_string()));

                    assert_eq!(resultados[2].name, "test_three");
                    assert!(resultados[2].result);
                    assert!(resultados[2].error.is_none());
                }
                Err(e) => panic!("Test execution failed: {}", e),
            }
        }
        #[test]
        fn test_test_scope_isolation() {
            // test_one: define x = 1, passa se x == 1
            let test_one = Statement::TestDef(Function {
                name: "test_one".to_string(),
                kind: Type::TBool,
                params: vec![],
                body: Some(Box::new(Statement::Block(vec![
                    Statement::VarDeclaration("x".to_string(), Box::new(Expression::CInt(1))),
                    Statement::AssertEQ(
                        Box::new(Expression::Var("x".to_string())),
                        Box::new(Expression::CInt(1)),
                        Box::new(Expression::CString("x should be 1".to_string())),
                    ),
                ]))),
            });

            // test_two: espera que x NÃO exista
            let test_two = Statement::TestDef(Function {
                name: "test_two".to_string(),
                kind: Type::TBool,
                params: vec![],
                body: Some(Box::new(Statement::Block(vec![Statement::AssertFalse(
                    Box::new(Expression::Var("x".to_string())),
                    Box::new(Expression::CString("x should not be visible".to_string())),
                )]))),
            });

            let stmt = Statement::Block(vec![test_one, test_two]);

            let results = run_tests(&stmt).unwrap();

            assert_eq!(results.len(), 2);

            let r1 = &results[0];
            let r2 = &results[1];

            assert_eq!(r1.name, "test_one");
            assert!(r1.result);

            assert_eq!(r2.name, "test_two");
            assert!(!r2.result);
            assert_eq!(r2.error, Some("Variable 'x' not found".to_string())); // Erro é propagado de Expression::Var
        }
    }
}
