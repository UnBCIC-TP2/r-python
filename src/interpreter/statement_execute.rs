use super::expression_eval::{eval, ExpressionResult};
use crate::environment::environment::Environment;
use crate::ir::ast::{Expression, Statement};
use crate::stdlib::standard_library::get_metabuiltins_table;

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
    get_metabuiltins_table();
    match execute(stmt, env) {
        Ok(Computation::Continue(new_env)) => Ok(new_env),
        Ok(Computation::Return(_, new_env)) => Ok(new_env),
        Ok(Computation::PropagateError(_, new_env)) => Ok(new_env),
        Err(e) => Err(e),
    }
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

        Statement::MetaStmt(ref name) => {
            let table = get_metabuiltins_table();
            if let Some(f) = table.get(name) {
                let next_stmt = f(&mut new_env);
                execute(next_stmt, &new_env)
            } else {
                Err(format!("Meta built-in '{}' not found", name))
            }
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
}
