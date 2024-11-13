use std::collections::HashMap;

use crate::ir::ast::Expression;
use crate::ir::ast::Name;
use crate::ir::ast::Statement;

type IntValue = i32;
type ErrorMessage = String;

#[derive(Debug, Clone)]
pub enum EnvValue {
    CInt(i32),
    Func(Vec<Name>, Box<Statement>),
}

type Environment = HashMap<Name, EnvValue>;

pub fn eval(exp: &Expression, env: &Environment) -> Result<IntValue, ErrorMessage> {
    match exp {
        Expression::CInt(v) => Ok(*v),
        Expression::Add(lhs, rhs) => Ok(eval(lhs, env)? + eval(rhs, env)?),
        Expression::Sub(lhs, rhs) => Ok(eval(lhs, env)? - eval(rhs, env)?),
        Expression::Mul(lhs, rhs) => Ok(eval(lhs, env)? * eval(rhs, env)?),
        Expression::Div(lhs, rhs) => Ok(eval(lhs, env)? / eval(rhs, env)?),
        Expression::Var(name) => match env.get(name) {
            Some(EnvValue::CInt(value)) => Ok(*value),
            _ => Err(format!("Variable {} not found", name)),
        },
        Expression::FuncCall(name, args) => {
            match env.get(name) {
                Some(EnvValue::Func(params, stmt)) => {
                    let mut func_env = env.clone();

                    if args.len() != params.len() {
                        return Err(format!("{} requires {} arguments, got {}", name, params.len(), args.len()));
                    }

                    for (param, arg) in params.iter().zip(args.iter()) {
                        let value = eval(arg, env)?;
                        func_env.insert(param.clone(), EnvValue::CInt(value));
                    }

                    let result = execute(stmt, func_env)?;
                    
                    match result.get("result") {
                        Some(EnvValue::CInt(val)) => Ok(*val),
                        _ => Err(format!("{} did not return a valid result", name)),
                    }
                }
                _ => Err(format!("{} is not defined", name)),
            }
        }
    }
}

pub fn execute(stmt: &Statement, env: Environment) -> Result<Environment, ErrorMessage> {
    match stmt {
        Statement::Assignment(name, exp) => {
            let value = eval(exp, &env)?;
            let mut new_env = env;
            new_env.insert(*name.clone(), EnvValue::CInt(value));
            Ok(new_env.clone())
        }
        Statement::IfThenElse(cond, stmt_then, stmt_else) => {
            let value = eval(cond, &env)?;
            if value > 0 {
                execute(stmt_then, env)
            } else {
                execute(stmt_else, env)
            }
        }
        Statement::While(cond, stmt) => {
            let mut value = eval(cond, &env)?;
            let mut new_env = env;
            while value > 0 {
                new_env = execute(stmt, new_env.clone())?;
                value = eval(cond, &new_env.clone())?;
            }
            Ok(new_env)
        }
        Statement::For(var, exp1, exp2, incr, stmt) => {
            let mut new_env = env.clone();
            let srt_value = eval(exp1, &new_env)?;
            let end_value = eval(exp2, &new_env)?;
            let incr_value = eval(incr, &new_env)?;

            new_env.insert(*var.clone(), EnvValue::CInt(srt_value));

            let mut loop_value = srt_value;

            // This could benefit from a refactor...
            match incr_value.signum() {
                -1 => {
                    if srt_value < end_value {
                        Err(String::from("For condition never reached"))
                    } else {
                        while loop_value > end_value {
                            new_env = execute(stmt, new_env.clone())?;

                            let increment = Expression::Add(
                                Box::new(Expression::Var(*var.clone())),
                                Box::new(Expression::CInt(incr_value)),
                            );

                            let new_var_value = eval(&increment, &new_env)?;
                            new_env.insert(*var.clone(), EnvValue::CInt(new_var_value));

                            loop_value = new_var_value;
                        }
                        new_env.remove(&var as &str);

                        Ok(new_env)
                    }
                }
                0 => {
                    Err(String::from("Increment cannot be zero"))
                }
                1 => {
                    if srt_value > end_value {
                        Err(String::from("For condition never reached"))
                    } else {
                        while loop_value < end_value {
                            new_env = execute(stmt, new_env.clone())?;

                            let increment = Expression::Add(
                                Box::new(Expression::Var(*var.clone())),
                                Box::new(Expression::CInt(incr_value)),
                            );

                            let new_var_value = eval(&increment, &new_env)?;
                            new_env.insert(*var.clone(), EnvValue::CInt(new_var_value));

                            loop_value = new_var_value;
                        }
                        new_env.remove(&var as &str);

                        Ok(new_env)
                    }
                }
                _ => Ok(new_env)
            }
        }
        Statement::Func(name, params, stmt) => {
            let mut new_env = env;
            new_env.insert(*name.clone(), EnvValue::Func(params.clone(), stmt.clone()));
            Ok(new_env)
        }
        Statement::Sequence(s1, s2) => execute(s1, env).and_then(|new_env| execute(s2, new_env)),
        _ => Err(String::from("not implemented yet")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn eval_constant() {
        let env = HashMap::new();
        let c10 = Expression::CInt(10);
        let c20 = Expression::CInt(20);

        assert_eq!(eval(&c10, &env), Ok(10));
        assert_eq!(eval(&c20, &env), Ok(20));
    }

    #[test]
    fn eval_add_expression1() {
        let env = HashMap::new();
        let c10 = Expression::CInt(10);
        let c20 = Expression::CInt(20);
        let add1 = Expression::Add(Box::new(c10), Box::new(c20));
        assert_eq!(eval(&add1, &env), Ok(30));
    }

    #[test]
    fn eval_add_expression2() {
        let env = HashMap::new();
        let c10 = Expression::CInt(10);
        let c20 = Expression::CInt(20);
        let c30 = Expression::CInt(30);
        let add1 = Expression::Add(Box::new(c10), Box::new(c20));
        let add2 = Expression::Add(Box::new(add1), Box::new(c30));
        assert_eq!(eval(&add2, &env), Ok(60));
    }

    #[test]
    fn eval_mul_expression() {
        let env = HashMap::new();
        let c10 = Expression::CInt(10);
        let c20 = Expression::CInt(20);
        let mul1 = Expression::Mul(Box::new(c10), Box::new(c20));
        assert_eq!(eval(&mul1, &env), Ok(200));
    }

    #[test]
    fn eval_variable() {
        let env = HashMap::from([(String::from("x"), EnvValue::CInt(10)), (String::from("y"), EnvValue::CInt(20))]);
        let v1 = Expression::Var(String::from("x"));
        let v2 = Expression::Var(String::from("y"));
        assert_eq!(eval(&v1, &env), Ok(10));
        assert_eq!(eval(&v2, &env), Ok(20));
    }

    #[test]
    fn eval_sub_expression1() {
        let env = HashMap::new();
        let c10 = Expression::CInt(10);
        let c20 = Expression::CInt(20);
        let mul1 = Expression::Sub(Box::new(c20), Box::new(c10));
        assert_eq!(eval(&mul1, &env), Ok(10));
    }

    #[test]
    fn eval_sub_expression2() {
        let env = HashMap::new();
        let c10 = Expression::CInt(100);
        let c20 = Expression::CInt(300);
        let mul1 = Expression::Sub(Box::new(c20), Box::new(c10));
        assert_eq!(eval(&mul1, &env), Ok(200));
    }

    #[test]
    fn eval_div_expression1() {
        let env = HashMap::new();
        let c10 = Expression::CInt(10);
        let c20 = Expression::CInt(20);
        let mul1 = Expression::Div(Box::new(c20), Box::new(c10));
        assert_eq!(eval(&mul1, &env), Ok(2));
    }

    #[test]
    fn eval_div_expression2() {
        let env = HashMap::new();
        let c10 = Expression::CInt(3);
        let c20 = Expression::CInt(21);
        let mul1 = Expression::Div(Box::new(c20), Box::new(c10));
        assert_eq!(eval(&mul1, &env), Ok(7));
    }

    #[test]
    fn execute_assignment() {
        let env = HashMap::new(); 
        let assign_stmt = 
            Statement::Assignment(Box::new(String::from("x")), Box::new(Expression::CInt(42)));
    
        match execute(&assign_stmt, env) {
            Ok(new_env) => {
                match new_env.get("x") {
                    Some(EnvValue::CInt(42)) => {},
                    Some(value) => assert!(false, "Expected 42, got {:?}", value),
                    None => assert!(false, "Variable x not found"),
                }
            },
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn eval_expression_with_variables() {
        let env = HashMap::from([(String::from("a"), EnvValue::CInt(5)), (String::from("b"), EnvValue::CInt(3))]);
        let expr = Expression::Mul(
            Box::new(Expression::Var(String::from("a"))),
            Box::new(Expression::Add(
                Box::new(Expression::Var(String::from("b"))),
                Box::new(Expression::CInt(2)),
            )),
        );
        assert_eq!(eval(&expr, &env), Ok(25));
    }

    #[test]
    fn eval_nested_expressions() {
        let env = HashMap::new();
        let expr = Expression::Add(
            Box::new(Expression::Mul(
                Box::new(Expression::CInt(2)),
                Box::new(Expression::CInt(3)),
            )),
            Box::new(Expression::Sub(
                Box::new(Expression::CInt(10)),
                Box::new(Expression::CInt(4)),
            )),
        );
        assert_eq!(eval(&expr, &env), Ok(12));
    }

    #[test]
    fn eval_variable_not_found() {
        let env = HashMap::new();
        let var_expr = Expression::Var(String::from("z"));

        assert_eq!(
            eval(&var_expr, &env),
            Err(String::from("Variable z not found"))
        );
    }

    #[test]
    fn eval_summation() {
        /*
         * (a test case for the following program)
         *
         * > x = 10
         * > y = 0
         * > while x:
         * >   y = y + x
         * >   x = x - 1
         *
         * After executing this program, 'x' must be zero and
         * 'y' must be 55.
         */
        let env = HashMap::new();

        let a1 = Statement::Assignment(Box::new(String::from("x")), Box::new(Expression::CInt(10)));
        let a2 = Statement::Assignment(Box::new(String::from("y")), Box::new(Expression::CInt(0)));
        let a3 = Statement::Assignment(
            Box::new(String::from("y")),
            Box::new(Expression::Add(
                Box::new(Expression::Var(String::from("y"))),
                Box::new(Expression::Var(String::from("x"))),
            )),
        );
        let a4 = Statement::Assignment(
            Box::new(String::from("x")),
            Box::new(Expression::Sub(
                Box::new(Expression::Var(String::from("x"))),
                Box::new(Expression::CInt(1)),
            )),
        );

        let seq1 = Statement::Sequence(Box::new(a3), Box::new(a4));

        let while_statement =
            Statement::While(Box::new(Expression::Var(String::from("x"))), Box::new(seq1));

        let seq2 = Statement::Sequence(Box::new(a2), Box::new(while_statement));
        let program = Statement::Sequence(Box::new(a1), Box::new(seq2));

        match execute(&program, env) {
            Ok(new_env) => {
                match new_env.get("y") {
                    Some(EnvValue::CInt(55)) => {}, 
                    Some(val) => assert!(false, "Expected 55, got {:?}", val),
                    None => assert!(false, "Variable y not found"),
                }
                match new_env.get("x") {
                    Some(EnvValue::CInt(0)) => {}, 
                    Some(val) => assert!(false, "Expected 0, got {:?}", val),
                    None => assert!(false, "Variable x not found"),
                }
            }
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn eval_simple_if_then_else() {
        /*
         * Test for simple if-then-else statement
         *
         * > x = 10
         * > if x > 5:
         * >   y = 1
         * > else:
         * >   y = 0
         *
         * After executing, 'y' should be 1.
         */
        let env = HashMap::new();

        let condition = Expression::Var(String::from("x"));
        let then_stmt =
            Statement::Assignment(Box::new(String::from("y")), Box::new(Expression::CInt(1)));
        let else_stmt =
            Statement::Assignment(Box::new(String::from("y")), Box::new(Expression::CInt(0)));

        let if_statement = Statement::IfThenElse(
            Box::new(condition),
            Box::new(then_stmt),
            Box::new(else_stmt),
        );

        let setup_stmt =
            Statement::Assignment(Box::new(String::from("x")), Box::new(Expression::CInt(10)));
        let program = Statement::Sequence(Box::new(setup_stmt), Box::new(if_statement));

        match execute(&program, env) {
            Ok(new_env) => {
                match new_env.get("y") {
                    Some(EnvValue::CInt(1)) => {},
                    Some(val) => assert!(false, "Expected 1, got {:?}", val),
                    None => assert!(false, "Variable y not found"),
                }
            }
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn eval_while_loop_decrement() {
        /*
         * Test for while loop that decrements a variable
         *
         * > x = 3
         * > y = 10
         * > while x:
         * >   y = y - 1
         * >   x = x - 1
         *
         * After executing, 'y' should be 7 and 'x' should be 0.
         */
        let env = HashMap::new();

        let a1 = Statement::Assignment(Box::new(String::from("x")), Box::new(Expression::CInt(3)));
        let a2 = Statement::Assignment(Box::new(String::from("y")), Box::new(Expression::CInt(10)));
        let a3 = Statement::Assignment(
            Box::new(String::from("y")),
            Box::new(Expression::Sub(
                Box::new(Expression::Var(String::from("y"))),
                Box::new(Expression::CInt(1)),
            )),
        );
        let a4 = Statement::Assignment(
            Box::new(String::from("x")),
            Box::new(Expression::Sub(
                Box::new(Expression::Var(String::from("x"))),
                Box::new(Expression::CInt(1)),
            )),
        );

        let seq1 = Statement::Sequence(Box::new(a3), Box::new(a4));
        let while_statement =
            Statement::While(Box::new(Expression::Var(String::from("x"))), Box::new(seq1));
        let program = Statement::Sequence(
            Box::new(a1),
            Box::new(Statement::Sequence(Box::new(a2), Box::new(while_statement))),
        );

        match execute(&program, env) {
            Ok(new_env) => {
                match new_env.get("y") {
                    Some(EnvValue::CInt(7)) => {}, 
                    Some(val) => assert!(false, "Expected 7, got {:?}", val),
                    None => assert!(false, "Variable y not found"),
                }
                match new_env.get("x") {
                    Some(EnvValue::CInt(0)) => {},
                    Some(val) => assert!(false, "Expected 0, got {:?}", val),
                    None => assert!(false, "Variable x not found"),
                }
            }
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn eval_for_loop_increment() {
        /*
        * For loop test for variable increment
        *
        * > y = 0
        * 
        * > for i in range(0, 5, 2):
        * >    y = y + i
        * 
        * After executing, 'y' should be 6 and 'i' should not be accessible.
        */
        let env = HashMap::new();

        let a1 = Statement::Assignment(Box::new(String::from("y")), Box::new(Expression::CInt(0)));
        let for_exec = Statement::Assignment(
            Box::new(String::from("y")),
            Box::new(Expression::Add(
                Box::new(Expression::Var(String::from("y"))),
                Box::new(Expression::Var(String::from("i"))),
            )),
        );

        let for_stmt = Statement::For(
            Box::new(String::from("i")), 
            Box::new(Expression::CInt(0)), 
            Box::new(Expression::CInt(5)), 
            Box::new(Expression::CInt(2)),
            Box::new(for_exec),
        );

        let program = Statement::Sequence(
            Box::new(a1),
            Box::new(for_stmt),
        );

        match execute(&program, env) {
            Ok(new_env) => {
                match new_env.get("y") {
                    Some(EnvValue::CInt(6)) => {},
                    Some(val) => assert!(false, "Expected 6, got {:?}", val),
                    None => assert!(false, "Variable y not found"),
                }
                match new_env.get("i") {
                    None => {},
                    Some(val) => assert!(false, "Expected None, got {:?}", val),
                }
            }
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn eval_for_loop_decrement() {
        /*
        * For loop test for variable decrement
        *
        * > y = 0
        * 
        * > for i in range(10, 3, -1):
        * >    y = y + i
        * 
        * After executing, 'y' should be 49 and 'i' should not be accessible.
        */
        let env = HashMap::new();

        let a1 = Statement::Assignment(Box::new(String::from("y")), Box::new(Expression::CInt(0)));
        let for_exec = Statement::Assignment(
            Box::new(String::from("y")),
            Box::new(Expression::Add(
                Box::new(Expression::Var(String::from("y"))),
                Box::new(Expression::Var(String::from("i"))),
            )),
        );

        let for_stmt = Statement::For(
            Box::new(String::from("i")), 
            Box::new(Expression::CInt(10)), 
            Box::new(Expression::CInt(3)), 
            Box::new(Expression::CInt(-1)),
            Box::new(for_exec),
        );

        let program = Statement::Sequence(
            Box::new(a1),
            Box::new(for_stmt),
        );

        match execute(&program, env) {
            Ok(new_env) => {
                match new_env.get("y") {
                    Some(EnvValue::CInt(49)) => {},
                    Some(val) => assert!(false, "Expected 49, got {:?}", val),
                    None => assert!(false, "Variable y not found"),
                }
                match new_env.get("i") {
                    None => {},
                    Some(val) => assert!(false, "Expected None, got {:?}", val),
                }
            }
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn eval_for_loop_error() {
        /*
        * For loop test for condition never reached
        *
        * > y = 0
        * 
        * > for i in range(0, 1, -1):
        * >    y = y + i
        * 
        */
        let env = HashMap::new();

        let a1 = Statement::Assignment(Box::new(String::from("y")), Box::new(Expression::CInt(0)));
        let for_exec = Statement::Assignment(
            Box::new(String::from("y")),
            Box::new(Expression::Add(
                Box::new(Expression::Var(String::from("y"))),
                Box::new(Expression::Var(String::from("i"))),
            )),
        );

        let for_stmt = Statement::For(
            Box::new(String::from("i")), 
            Box::new(Expression::CInt(0)), 
            Box::new(Expression::CInt(1)), 
            Box::new(Expression::CInt(-1)),
            Box::new(for_exec),
        );

        let program = Statement::Sequence(
            Box::new(a1),
            Box::new(for_stmt),
        );

        match execute(&program, env) {
            Ok(_new_env) => {
                assert!(false);
            }
            Err(s) => assert_eq!(s, "For condition never reached"),
        }
    }

    #[test]
    fn eval_nested_if_statements() {
        /*
         * Test for nested if-then-else statements
         *
         * > x = 10
         * > if x > 5:
         * >   if x > 8:
         * >     y = 1
         * >   else:
         * >     y = 2
         * > else:
         * >   y = 0
         *
         * After executing, 'y' should be 1.
         */
        let env = HashMap::new();

        let inner_then_stmt =
            Statement::Assignment(Box::new(String::from("y")), Box::new(Expression::CInt(1)));
        let inner_else_stmt =
            Statement::Assignment(Box::new(String::from("y")), Box::new(Expression::CInt(2)));
        let inner_if_statement = Statement::IfThenElse(
            Box::new(Expression::Var(String::from("x"))),
            Box::new(inner_then_stmt),
            Box::new(inner_else_stmt),
        );

        let outer_else_stmt =
            Statement::Assignment(Box::new(String::from("y")), Box::new(Expression::CInt(0)));
        let outer_if_statement = Statement::IfThenElse(
            Box::new(Expression::Var(String::from("x"))),
            Box::new(inner_if_statement),
            Box::new(outer_else_stmt),
        );

        let setup_stmt =
            Statement::Assignment(Box::new(String::from("x")), Box::new(Expression::CInt(10)));
        let program = Statement::Sequence(Box::new(setup_stmt), Box::new(outer_if_statement));

        match execute(&program, env) {
            Ok(new_env) => {
                match new_env.get("y") {
                    Some(EnvValue::CInt(1)) => {},
                    Some(val) => assert!(false, "Expected 1, got {:?}", val),
                    None => assert!(false, "Variable y not found"),
                }
            }
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn eval_complex_sequence() {
        /*
         * Sequence with multiple assignments and expressions
         *
         * > x = 5
         * > y = 0
         * > z = 2 * x + 3
         *
         * After executing, 'x' should be 5, 'y' should be 0, and 'z' should be 13.
         */
        let env = HashMap::new();

        let a1 = Statement::Assignment(Box::new(String::from("x")), Box::new(Expression::CInt(5)));
        let a2 = Statement::Assignment(Box::new(String::from("y")), Box::new(Expression::CInt(0)));
        let a3 = Statement::Assignment(
            Box::new(String::from("z")),
            Box::new(Expression::Add(
                Box::new(Expression::Mul(
                    Box::new(Expression::CInt(2)),
                    Box::new(Expression::Var(String::from("x"))),
                )),
                Box::new(Expression::CInt(3)),
            )),
        );

        let program = Statement::Sequence(
            Box::new(a1),
            Box::new(Statement::Sequence(Box::new(a2), Box::new(a3))),
        );

        match execute(&program, env) {
            Ok(new_env) => {
                match new_env.get("x") {
                    Some(EnvValue::CInt(5)) => {},
                    Some(val) => assert!(false, "Expected 5, got {:?}", val),
                    None => assert!(false, "Variable x not found"),
                }
                match new_env.get("y") {
                    Some(EnvValue::CInt(0)) => {}, 
                    Some(val) => assert!(false, "Expected 0, got {:?}", val),
                    None => assert!(false, "Variable y not found"),
                }
                match new_env.get("z") {
                    Some(EnvValue::CInt(13)) => {}, 
                    Some(val) => assert!(false, "Expected 13, got {:?}", val),
                    None => assert!(false, "Variable z not found"),
                }
            }
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn func_decl_call() {
        /*
         * Test for declaration and call of a function
         *
         * > def add(a,b):
         * >    result = a + b
         * >
         * > sum = add(5, 7)
         *
         * After executing, 'sum' should be 12.
         */
        let env = Environment::new();

        let program = Statement::Sequence(
            Box::new(Statement::Func(
                Box::new(String::from("add")),
                vec![String::from("a"), String::from("b")],
                Box::new(Statement::Assignment(
                    Box::new(String::from("result")),
                    Box::new(Expression::Add(
                        Box::new(Expression::Var(String::from("a"))),
                        Box::new(Expression::Var(String::from("b"))),
                    )),
                )),
            )),
            Box::new(Statement::Assignment(
                Box::new(String::from("sum")),
                Box::new(Expression::FuncCall(
                    String::from("add"),
                    vec![Expression::CInt(5), Expression::CInt(7)],
                )),
            )),
        );

        match execute(&program, env) {
            Ok(new_env) => {
                match new_env.get("sum") {
                    Some(EnvValue::CInt(12)) => {}, 
                    Some(val) => assert!(false, "Expected 12, got {:?}", val),
                    None => assert!(false, "Variable sum not found"),
                }
            }
            Err(s) => assert!(false, "{}", s),
        }
    }
}
