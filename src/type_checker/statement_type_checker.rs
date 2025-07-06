use crate::environment::environment::Environment;
use crate::ir::ast::{Expression, Function, Name, Statement, Type};
use crate::type_checker::expression_type_checker::check_expr;
use std::collections::HashMap;

type ErrorMessage = String;

pub fn check_stmt(
    stmt: Statement,
    env: &Environment<Type>,
) -> Result<Environment<Type>, ErrorMessage> {
    match stmt {
        Statement::VarDeclaration(var, expr) => check_var_declaration_stmt(var, expr, env),
        Statement::ValDeclaration(var, expr) => check_val_declaration_stmt(var, expr, env),
        Statement::Sequence(stmt1, stmt2) => check_squence_stmt(stmt1, stmt2, env),
        Statement::Assignment(name, exp) => check_assignment_stmt(name, exp, env),
        Statement::IfThenElse(cond, stmt_then, stmt_else_opt) => {
            check_if_then_else_stmt(cond, stmt_then, stmt_else_opt, env)
        }
        Statement::While(cond, stmt) => check_while_stmt(cond, stmt, env),
        Statement::For(var, expr, stmt) => check_for_stmt(var, expr, stmt, env),
        Statement::FuncDef(function) => check_func_def_stmt(function, env),
        Statement::TypeDeclaration(name, cons) => check_adt_declarations_stmt(name, cons, env),
        Statement::Return(exp) => check_return_stmt(exp, env),
        _ => Err("Not implemented yet".to_string()),
    }
}

fn check_squence_stmt(
    stmt1: Box<Statement>,
    stmt2: Box<Statement>,
    env: &Environment<Type>,
) -> Result<Environment<Type>, ErrorMessage> {
    let new_env = check_stmt(*stmt1, &env)?;
    check_stmt(*stmt2, &new_env)
}

fn check_assignment_stmt(
    name: Name,
    exp: Box<Expression>,
    env: &Environment<Type>,
) -> Result<Environment<Type>, ErrorMessage> {
    let mut new_env = env.clone();
    let exp_type = check_expr(&*exp, &new_env)?;

    match new_env.lookup(&name) {
        Some((mutable, var_type)) => {
            if !mutable {
                return Err(format!(
                    "[Type Error] Cannot assign to immutable variable '{}'.",
                    name
                ));
            }
            if var_type != exp_type && var_type != Type::TAny && exp_type != Type::TAny {
                return Err(format!(
                    "[Type Error] Cannot assign value of type '{:?}' to variable '{}' of type '{:?}'.",
                    exp_type, name, var_type
                ));
            }
            new_env.map_variable(name, mutable, exp_type);
            Ok(new_env)
        }
        None => Err(format!("[Name Error] Variable '{}' is not defined.", name)),
    }
}

fn check_var_declaration_stmt(
    var: Name,
    expr: Box<Expression>,
    env: &Environment<Type>,
) -> Result<Environment<Type>, ErrorMessage> {
    let mut new_env = env.clone();
    let exp_type = check_expr(&*expr, &new_env)?;
    new_env.map_variable(var, true, exp_type);
    Ok(new_env)
}

fn check_val_declaration_stmt(
    var: Name,
    expr: Box<Expression>,
    env: &Environment<Type>,
) -> Result<Environment<Type>, ErrorMessage> {
    let mut new_env = env.clone();
    let exp_type = check_expr(&*expr, &new_env)?;
    new_env.map_variable(var, false, exp_type);
    Ok(new_env)
}

fn check_if_then_else_stmt(
    cond: Box<Expression>,
    stmt_then: Box<Statement>,
    stmt_else_opt: Option<Box<Statement>>,
    env: &Environment<Type>,
) -> Result<Environment<Type>, ErrorMessage> {
    let mut new_env = env.clone();
    let cond_type = check_expr(&*cond, &new_env)?;

    if cond_type != Type::TBool {
        return Err(String::from("[Type Error] Condition must be a boolean."));
    }

    let then_env = check_stmt(*stmt_then, &new_env)?;

    match stmt_else_opt {
        Some(stmt_else) => {
            let else_env = check_stmt(*stmt_else, &new_env)?;
            merge_environments(&then_env, &else_env)
        }
        None => Ok(then_env),
    }
}

fn check_while_stmt(
    cond: Box<Expression>,
    stmt: Box<Statement>,
    env: &Environment<Type>,
) -> Result<Environment<Type>, ErrorMessage> {
    let mut new_env = env.clone();
    let cond_type = check_expr(&*cond, &new_env)?;

    if cond_type != Type::TBool {
        return Err(String::from("[Type Error] Condition must be a boolean."));
    }

    check_stmt(*stmt, &new_env)
}

fn check_for_stmt(
    var: Name,
    expr: Box<Expression>,
    stmt: Box<Statement>,
    env: &Environment<Type>,
) -> Result<Environment<Type>, ErrorMessage> {
    let mut new_env = env.clone();
    let expr_type = check_expr(&*expr, &new_env)?;

    // For now, we'll assume the expression should be a list
    // This could be made more flexible in the future
    if !matches!(expr_type, Type::TList(_)) {
        return Err(String::from("[Type Error] For loop expression must be a list."));
    }

    // Extract the element type from the list type
    let element_type = match expr_type {
        Type::TList(element_type) => *element_type,
        _ => Type::TAny, // This should never happen due to the check above
    };

    // Add the loop variable to the environment with the correct element type
    new_env.map_variable(var, true, element_type);

    check_stmt(*stmt, &new_env)
}

fn check_func_def_stmt(
    function: Function,
    env: &Environment<Type>,
) -> Result<Environment<Type>, ErrorMessage> {
    let mut new_env = env.clone();
    new_env.push();

    for formal_arg in function.params.iter() {
        new_env.map_variable(
            formal_arg.argument_name.clone(),
            false,
            formal_arg.argument_type.clone(),
        );
    }

    if let Some(body) = function.body.clone() {
        new_env = check_stmt(*body, &new_env)?;
    }
    new_env.pop();
    new_env.map_function(function);

    Ok(new_env)
}

fn check_adt_declarations_stmt(
    name: Name,
    cons: HashMap<Name, Vec<Type>>,
    env: &Environment<Type>,
) -> Result<Environment<Type>, ErrorMessage> {
    let mut new_env = env.clone();
    new_env.map_adt(name.clone(), cons);
    Ok(new_env)
}

fn check_return_stmt(
    exp: Box<Expression>,
    env: &Environment<Type>,
) -> Result<Environment<Type>, ErrorMessage> {
    let mut new_env = env.clone();

    assert!(new_env.scoped_function());

    let ret_type = check_expr(&*exp, &new_env)?;

    match new_env.lookup(&"return".to_string()) {
        Some(_) => Ok(new_env),
        None => {
            new_env.map_variable("return".to_string(), false, ret_type);
            Ok(new_env)
        }
    }
}

fn merge_environments(
    env1: &Environment<Type>,
    env2: &Environment<Type>,
) -> Result<Environment<Type>, ErrorMessage> {
    let mut merged = env1.clone();

    // Get all variables defined in either environment
    for (name, (mutable2, type2)) in env2.get_all_variables() {
        match env1.lookup(&name) {
            Some((mutable1, type1)) => {
                // Variable exists in both branches
                // Check mutability first - if either is constant, result must be constant
                let final_mutable = mutable1 && mutable2;

                // Then check types
                if type1 == Type::TAny {
                    // If type1 is TAny, use type2
                    merged.map_variable(name.clone(), final_mutable, type2.clone());
                } else if type2 == Type::TAny {
                    // If type2 is TAny, keep type1
                    merged.map_variable(name.clone(), final_mutable, type1.clone());
                } else if type1 != type2 {
                    return Err(format!(
                        "[Type Error] Variable '{}' has inconsistent types in different branches: '{:?}' and '{:?}'",
                        name, type1, type2
                    ));
                } else {
                    // Types match, update with combined mutability
                    merged.map_variable(name.clone(), final_mutable, type1.clone());
                }
            }
            None => {
                // Variable only exists in else branch - it's conditionally defined
                merged.map_variable(name.clone(), mutable2, type2.clone());
            }
        }
    }

    //TODO: should we merge ADTs and functions?

    Ok(merged)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::environment::environment::Environment;
    use crate::ir::ast::Expression::*;
    use crate::ir::ast::FormalArgument;
    use crate::ir::ast::Function;
    use crate::ir::ast::Statement::*;
    use crate::ir::ast::Type;

    #[test]
    fn check_assignment() {
        let env: Environment<Type> = Environment::new();
        // Declare variable 'a' first
        let env = check_stmt(
            Statement::VarDeclaration("a".to_string(), Box::new(CTrue)),
            &env,
        )
        .unwrap();
        let assignment = Assignment("a".to_string(), Box::new(CTrue));

        match check_stmt(assignment, &env) {
            Ok(_) => assert!(true),
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn check_assignment_error2() {
        let env: Environment<Type> = Environment::new();
        // Declare variable 'a' first
        let env = check_stmt(
            Statement::VarDeclaration("a".to_string(), Box::new(CTrue)),
            &env,
        )
        .unwrap();
        let assignment1 = Assignment("a".to_string(), Box::new(CTrue));
        let assignment2 = Assignment("a".to_string(), Box::new(CInt(1)));
        let program = Sequence(Box::new(assignment1), Box::new(assignment2));

        assert!(
            matches!(check_stmt(program, &env), Err(_)),
            "[Type Error on '__main__()'] 'a' has mismatched types: expected 'TBool', found 'TInteger'."
        );
    }

    #[test]
    fn check_if_then_else_error() {
        let env: Environment<Type> = Environment::new();

        let stmt = IfThenElse(
            Box::new(CInt(1)),
            Box::new(Assignment("a".to_string(), Box::new(CInt(1)))),
            Some(Box::new(Assignment("b".to_string(), Box::new(CReal(2.0))))),
        );

        assert!(
            matches!(check_stmt(stmt, &env), Err(_)),
            "[Type Error on '__main__()'] if expression must be boolean."
        );
    }

    #[test]
    fn check_while_error() {
        let env: Environment<Type> = Environment::new();

        let assignment1 = Assignment("a".to_string(), Box::new(CInt(3)));
        let assignment2 = Assignment("b".to_string(), Box::new(CInt(0)));
        let stmt = While(
            Box::new(CInt(1)),
            Box::new(Assignment(
                "b".to_string(),
                Box::new(Add(Box::new(Var("b".to_string())), Box::new(CInt(1)))),
            )),
        );
        let program = Sequence(
            Box::new(assignment1),
            Box::new(Sequence(Box::new(assignment2), Box::new(stmt))),
        );

        assert!(
            matches!(check_stmt(program, &env), Err(_)),
            "[Type Error on '__main__()'] while expression must be boolean."
        );
    }

    #[test]
    #[ignore = "not yet implemented"]
    fn check_func_def() {
        let env: Environment<Type> = Environment::new();

        let func = FuncDef(Function {
            name: "add".to_string(),
            kind: Type::TInteger,
            params: vec![
                FormalArgument::new("a".to_string(), Type::TInteger),
                FormalArgument::new("b".to_string(), Type::TInteger),
            ],
            body: Some(Box::new(Return(Box::new(Add(
                Box::new(Var("a".to_string())),
                Box::new(Var("b".to_string())),
            ))))),
        });
        match check_stmt(func, &env) {
            Ok(_) => assert!(true),
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn test_if_else_consistent_types() {
        let env = Environment::new();
        // Declare variable 'x' first
        let env = check_stmt(
            Statement::VarDeclaration("x".to_string(), Box::new(Expression::CInt(0))),
            &env,
        )
        .unwrap();
        let stmt = Statement::IfThenElse(
            Box::new(Expression::CTrue),
            Box::new(Statement::Assignment(
                "x".to_string(),
                Box::new(Expression::CInt(1)),
            )),
            Some(Box::new(Statement::Assignment(
                "x".to_string(),
                Box::new(Expression::CInt(2)),
            ))),
        );

        // Should succeed - x is consistently an integer in both branches
        assert!(check_stmt(stmt, &env).is_ok());
    }

    #[test]
    fn test_if_else_inconsistent_types() {
        let env = Environment::new();
        let stmt = Statement::IfThenElse(
            Box::new(Expression::CTrue),
            Box::new(Statement::Assignment(
                "x".to_string(),
                Box::new(Expression::CInt(1)),
            )),
            Some(Box::new(Statement::Assignment(
                "x".to_string(),
                Box::new(Expression::CString("hello".to_string())),
            ))),
        );

        // Should fail - x has different types in different branches
        assert!(check_stmt(stmt, &env).is_err());
    }

    #[test]
    fn test_if_else_partial_definition() {
        let env = Environment::new();
        // Declare variable 'x' first
        let env = check_stmt(
            Statement::VarDeclaration("x".to_string(), Box::new(Expression::CInt(0))),
            &env,
        )
        .unwrap();
        let stmt = Statement::Sequence(
            Box::new(Statement::IfThenElse(
                Box::new(Expression::CTrue),
                Box::new(Statement::Assignment(
                    "x".to_string(),
                    Box::new(Expression::CInt(1)),
                )),
                None,
            )),
            Box::new(Statement::Assignment(
                "x".to_string(),
                Box::new(Expression::CInt(2)),
            )),
        );

        // Should succeed - x is conditionally defined in then branch
        // and later used consistently as an integer
        assert!(check_stmt(stmt, &env).is_ok());
    }

    #[test]
    fn test_variable_assignment() {
        let env = Environment::new();
        // Declare variable 'x' first
        let env = check_stmt(
            Statement::VarDeclaration("x".to_string(), Box::new(Expression::CInt(0))),
            &env,
        )
        .unwrap();
        let stmt = Statement::Assignment("x".to_string(), Box::new(Expression::CInt(42)));

        // Should succeed and add x:integer to environment
        let new_env = check_stmt(stmt, &env).unwrap();
        assert_eq!(
            new_env.lookup(&"x".to_string()),
            Some((true, Type::TInteger))
        );
    }

    #[test]
    fn test_variable_reassignment_same_type() {
        let mut env = Environment::new();
        env.map_variable("x".to_string(), true, Type::TInteger);

        let stmt = Statement::Assignment("x".to_string(), Box::new(Expression::CInt(100)));

        // Should succeed - reassigning same type
        assert!(check_stmt(stmt, &env).is_ok());
    }

    #[test]
    fn test_variable_reassignment_different_type() {
        let mut env = Environment::new();
        env.map_variable("x".to_string(), true, Type::TInteger);

        let stmt = Statement::Assignment(
            "x".to_string(),
            Box::new(Expression::CString("hello".to_string())),
        );

        // Should fail - trying to reassign different type
        assert!(check_stmt(stmt, &env).is_err());
    }

    #[test]
    fn test_function_scoping() {
        let mut env: Environment<i32> = Environment::new();

        let global_func = Function {
            name: "global".to_string(),
            kind: Type::TVoid,
            params: Vec::new(),
            body: None,
        };

        let _local_func = Function {
            name: "local".to_string(),
            kind: Type::TVoid,
            params: Vec::new(),
            body: None,
        };

        // Test function scoping
        env.map_function(global_func.clone());
        assert!(env.lookup_function(&"global".to_string()).is_some());
    }

    #[test]
    fn test_for_valid_integer_list() {
        let mut env = Environment::new();
        env.map_variable("sum".to_string(), true, Type::TInteger);
        let stmt = Statement::For(
            "x".to_string(),
            Box::new(Expression::ListValue(vec![
                Expression::CInt(1),
                Expression::CInt(2),
                Expression::CInt(3),
            ])),
            Box::new(Statement::Assignment(
                "sum".to_string(),
                Box::new(Expression::Add(
                    Box::new(Expression::Var("sum".to_string())),
                    Box::new(Expression::Var("x".to_string())),
                )),
            )),
        );
        let result = check_stmt(stmt, &env);
        if let Err(e) = &result {
            println!("Error: {}", e);
        }
        assert!(result.is_ok());
    }

    #[test]
    fn test_for_mixed_type_list() {
        let env = Environment::new();
        let stmt = Statement::For(
            "x".to_string(),
            Box::new(Expression::ListValue(vec![
                Expression::CInt(1),
                Expression::CString("hello".to_string()),
                Expression::CInt(3),
            ])),
            Box::new(Statement::Assignment(
                "x".to_string(),
                Box::new(Expression::CInt(1)),
            )),
        );
        // Should fail - list contains mixed types (integers and strings)
        assert!(check_stmt(stmt, &env).is_err());
    }

    #[test]
    fn test_for_empty_list() {
        let env = Environment::new();
        // Declare variable 'x' first
        let env = check_stmt(
            Statement::VarDeclaration("x".to_string(), Box::new(Expression::CInt(0))),
            &env,
        )
        .unwrap();
        let stmt = Statement::For(
            "x".to_string(),
            Box::new(Expression::ListValue(vec![])),
            Box::new(Statement::Assignment(
                "x".to_string(),
                Box::new(Expression::CInt(1)),
            )),
        );
        // Should succeed - empty list is valid, though no iterations will occur
        assert!(check_stmt(stmt, &env).is_ok());
    }

    #[test]
    fn test_for_iterator_variable_reassignment() {
        let env = Environment::new();
        let stmt = Statement::For(
            "x".to_string(),
            Box::new(Expression::ListValue(vec![
                Expression::CInt(1),
                Expression::CInt(2),
            ])),
            Box::new(Statement::Assignment(
                "x".to_string(),
                Box::new(Expression::CString("invalid".to_string())),
            )),
        );
        // Should fail - trying to assign string to iterator variable when iterating over integers
        assert!(check_stmt(stmt, &env).is_err());
    }

    #[test]
    fn test_for_nested_loops() {
        let env = Environment::new();
        // Declare variable 'sum' first
        let env = check_stmt(
            Statement::VarDeclaration("sum".to_string(), Box::new(Expression::CInt(0))),
            &env,
        )
        .unwrap();
        let stmt = Statement::For(
            "i".to_string(),
            Box::new(Expression::ListValue(vec![
                Expression::CInt(1),
                Expression::CInt(2),
            ])),
            Box::new(Statement::For(
                "j".to_string(),
                Box::new(Expression::ListValue(vec![
                    Expression::CInt(3),
                    Expression::CInt(4),
                ])),
                Box::new(Statement::Assignment(
                    "sum".to_string(),
                    Box::new(Expression::Add(
                        Box::new(Expression::Var("i".to_string())),
                        Box::new(Expression::Var("j".to_string())),
                    )),
                )),
            )),
        );

        // Should succeed - nested loops with proper variable usage
        assert!(check_stmt(stmt, &env).is_ok());
    }

    #[test]
    fn test_for_variable_scope() {
        let mut env = Environment::new();
        env.map_variable("x".to_string(), true, Type::TString); // x is defined as string in outer scope

        let stmt = Statement::For(
            "x".to_string(), // reusing name x as iterator
            Box::new(Expression::ListValue(vec![
                Expression::CInt(1),
                Expression::CInt(2),
            ])),
            Box::new(Statement::Assignment(
                "y".to_string(),
                Box::new(Expression::Var("x".to_string())),
            )),
        );

        // Should not succeed - for loop creates new scope, x is temporarily an integer
        // TODO: Let discuss this case here next class.
        assert!(check_stmt(stmt, &env).is_err());
    }
}
