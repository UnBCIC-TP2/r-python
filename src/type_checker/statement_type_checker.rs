use crate::environment::environment::Environment;
use crate::ir::ast::{Expression, Statement, Type};          // Não remover Expression
use crate::type_checker::expression_type_checker::check_expr;
use crate::ir::typed_ast::{TypedStatement, TypedFunction};

type ErrorMessage = String;

pub fn check_stmt(
    stmt: Statement,
    env: &Environment<Type>,
) -> Result<(TypedStatement,Environment<Type>), ErrorMessage> {
    match stmt {
        Statement::VarDeclaration(var, expr) => {
            let mut new_env = env.clone();
            
            let typed_expr = check_expr(*expr, &new_env)?;
            
            let var_type = typed_expr.type_info().clone();

            if new_env.lookup(&var).is_none() {
                new_env.map_variable(var.clone(), true, var_type);
                
                let typed_stmt = TypedStatement::VarDeclaration {
                    name: var,
                    value: Box::new(typed_expr),
                };

                Ok((typed_stmt, new_env))
            } else {
                Err(format!("[Type Error] variable '{:?}' already declared", var))
            }
        },
        Statement::ValDeclaration(var, expr) => {
            let mut new_env = env.clone();
            let typed_expr = check_expr(*expr, &new_env)?;
            let var_type = typed_expr.type_info().clone();

            if new_env.lookup(&var).is_none() {
                new_env.map_variable(var.clone(), false, var_type); 
                let typed_stmt = TypedStatement::ValDeclaration {
                    name: var,
                    value: Box::new(typed_expr),
                };
                Ok((typed_stmt, new_env))
            } else {
                Err(format!("[Type Error] variable '{:?}' already declared", var))
            }
        },

        Statement::Sequence(stmt1, stmt2) => {
            let (typed_stmt1, env1) = check_stmt(*stmt1, env)?;
            let (typed_stmt2, env2) = check_stmt(*stmt2, &env1)?;
            
            let typed_sequence = TypedStatement::Sequence {
                first: Box::new(typed_stmt1),
                second: Box::new(typed_stmt2),
            };
            Ok((typed_sequence, env2))
        },

        Statement::Assignment(name, exp) => {
            let mut new_env = env.clone();
            let typed_exp = check_expr(*exp, &new_env)?;
            let exp_type = typed_exp.type_info();

            match new_env.lookup(&name) {
                Some((mutable, var_type)) => {
                    if !mutable {
                        return Err(format!("[Type Error] cannot reassign '{:?}' variable, since it was declared as a constant value.", name));
                    }
                    if var_type == Type::TAny || &var_type == exp_type {
                        let final_type = if var_type == Type::TAny { exp_type.clone() } else { var_type };
                        new_env.map_variable(name.clone(), true, final_type);
                        let typed_stmt = TypedStatement::Assignment { name, value: Box::new(typed_exp) };
                        Ok((typed_stmt, new_env))
                    } else {
                        Err(format!("[Type Error] expected '{:?}', found '{:?}'.", var_type, exp_type))
                    }
                }
                None => Err(format!("[Type Error] variable '{:?}' not declared.", name)),
            }
        },

        Statement::IfThenElse(cond, stmt_then, stmt_else_opt) => {
            let typed_cond = check_expr(*cond, env)?;
            if typed_cond.type_info() != &Type::TBool {
                return Err("[Type Error] 'if' condition must be boolean.".to_string());
            }

            let (typed_then_stmt, then_env) = check_stmt(*stmt_then, env)?;

            if let Some(stmt_else) = stmt_else_opt {
                let (typed_else_stmt, else_env) = check_stmt(*stmt_else, env)?;
                let final_env = merge_environments(&then_env, &else_env)?; 
                let typed_stmt = TypedStatement::IfThenElse {
                    cond: Box::new(typed_cond),
                    stmt_then: Box::new(typed_then_stmt),
                    stmt_else_opt: Some(Box::new(typed_else_stmt)),
                };
                Ok((typed_stmt, final_env))
            } else {
                let final_env = merge_environments(env, &then_env)?;
                let typed_stmt = TypedStatement::IfThenElse {
                    cond: Box::new(typed_cond),
                    stmt_then: Box::new(typed_then_stmt),
                    stmt_else_opt: None,
                };
                Ok((typed_stmt, final_env))
            }
        },

        Statement::While(cond, stmt) => {
            let typed_cond = check_expr(*cond, env)?;
            if typed_cond.type_info() != &Type::TBool {
                return Err("[Type Error] 'while' condition must be boolean.".to_string());
            }

            let (typed_body, final_env) = check_stmt(*stmt, env)?;
            let typed_stmt = TypedStatement::While {
                cond: Box::new(typed_cond),
                body: Box::new(typed_body),
            };
            Ok((typed_stmt, final_env))
        },

        Statement::For(var, expr, stmt) => {
            let typed_iterable = check_expr(*expr, env)?;
            
            if let Type::TList(base_type) = typed_iterable.type_info() {
                let mut loop_env = env.clone();
                loop_env.push();
                loop_env.map_variable(var.clone(), false, *base_type.clone());
                
                let (typed_body, _) = check_stmt(*stmt, &loop_env)?;
                loop_env.pop(); 
                
                let typed_stmt = TypedStatement::For {
                    name: var,
                    iterable: Box::new(typed_iterable),
                    body: Box::new(typed_body),
                };

                Ok((typed_stmt, env.clone()))
            } else {
                Err(format!("[TypeError] Expecting a List type in 'for' loop, but found a {:?}.", typed_iterable.type_info()))
            }
        },
        
        Statement::FuncDef(function) => {
            let mut new_env = env.clone();

            new_env.map_function(function.clone());


            let typed_func = TypedFunction {
                name: function.name,
                kind: function.kind,
                params: function.params,
                body: None, 
            };
            
            let typed_stmt = TypedStatement::FuncDef(typed_func);
            Ok((typed_stmt, new_env))
        },

        Statement::TypeDeclaration(name, cons) => {
            let mut new_env = env.clone();
            new_env.map_adt(name.clone(), cons.clone());
            let typed_stmt = TypedStatement::TypeDeclaration(name, cons);
            Ok((typed_stmt, new_env))
        },

        Statement::Return(exp) => {
            let typed_exp = check_expr(*exp, env)?;
            let mut new_env = env.clone();
            
            new_env.map_variable("return".to_string(), false, typed_exp.type_info().clone());
            
            let typed_stmt = TypedStatement::Return(Box::new(typed_exp));
            Ok((typed_stmt, new_env))
        },

        _ => Err("Not implemented yet".to_string()),
    }
}



fn merge_environments(
    env1: &Environment<Type>,
    env2: &Environment<Type>,
) -> Result<Environment<Type>, ErrorMessage> {
    let mut merged = env1.clone();

    for (name, (mutable2, type2)) in env2.get_all_variables() {
        match env1.lookup(&name) {
            Some((mutable1, type1)) => {
                let final_mutable = mutable1 && mutable2;

                if type1 == Type::TAny {
                    merged.map_variable(name.clone(), final_mutable, type2.clone());
                } else if type2 == Type::TAny {
                    merged.map_variable(name.clone(), final_mutable, type1.clone());
                } else if type1 != type2 {
                    return Err(format!(
                        "[Type Error] Variable '{}' has inconsistent types in different branches: '{:?}' and '{:?}'",
                        name, type1, type2
                    ));
                } else {
                    merged.map_variable(name.clone(), final_mutable, type1.clone());
                }
            }
            None => {
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
    use crate::ir::typed_ast::{TypedStatement};


    #[test]
    fn check_assignment() {
        let env = Environment::new();
        let (_, env_after_decl) = check_stmt(
            Statement::VarDeclaration("a".to_string(), Box::new(Expression::CTrue)),
            &env,
        ).unwrap();

        let assignment_stmt = Statement::Assignment("a".to_string(), Box::new(Expression::CFalse));
        
        let result = check_stmt(assignment_stmt, &env_after_decl);

        assert!(result.is_ok());
        let (typed_stmt, _final_env) = result.unwrap();

        assert!(matches!(typed_stmt, TypedStatement::Assignment { .. }));
    }

    #[test]
    fn check_assignment_error2() {
        let env: Environment<Type> = Environment::new();
        
        let (_, env_after_decl) = check_stmt(
            Statement::VarDeclaration("a".to_string(), Box::new(CTrue)),
            &env,
        )
        .unwrap();
        
        let assignment1 = Assignment("a".to_string(), Box::new(CTrue));
        let assignment2 = Assignment("a".to_string(), Box::new(CInt(1)));
        let program = Sequence(Box::new(assignment1), Box::new(assignment2));

        assert!(check_stmt(program, &env_after_decl).is_err());
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

        let result = check_stmt(func, &env);

        assert!(result.is_ok());
        let (typed_stmt, new_env) = result.unwrap();

        assert!(matches!(typed_stmt, TypedStatement::FuncDef(..)));

        assert!(new_env.lookup_function(&"add".to_string()).is_some());
    }

    #[test]
    fn test_if_else_consistent_types() {
        let env = Environment::new();
        
        let (_, env_after_decl) = check_stmt(
            Statement::VarDeclaration("x".to_string(), Box::new(Expression::CInt(0))),
            &env,
        )
        .unwrap();

        let if_stmt = Statement::IfThenElse(
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

        let result = check_stmt(if_stmt, &env_after_decl);

        assert!(result.is_ok());
        let (typed_stmt, final_env) = result.unwrap();

        assert!(matches!(typed_stmt, TypedStatement::IfThenElse { .. }));

        assert_eq!(
            final_env.lookup(&"x".to_string()),
            Some((true, Type::TInteger))      
        );
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

        assert!(check_stmt(stmt, &env).is_err());
    }

   #[test]
    fn test_if_else_partial_definition() {
        let env = Environment::new();
        
        let (_, env_after_decl) = check_stmt(
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

        let result = check_stmt(stmt, &env_after_decl);

        assert!(result.is_ok());
        let (_typed_stmt, final_env) = result.unwrap();

        assert_eq!(
            final_env.lookup(&"x".to_string()),
            Some((true, Type::TInteger))
        );
    }

    #[test]
    fn test_variable_assignment() {
        let env = Environment::new();
        
        let (_, env_after_decl) = check_stmt(
            Statement::VarDeclaration("x".to_string(), Box::new(Expression::CInt(0))),
            &env,
        )
        .unwrap();

        let stmt = Statement::Assignment("x".to_string(), Box::new(Expression::CInt(42)));

        let result = check_stmt(stmt, &env_after_decl);

        assert!(result.is_ok());

        let (typed_stmt, final_env) = result.unwrap();

        assert!(matches!(typed_stmt, TypedStatement::Assignment { .. }));

        assert_eq!(
            final_env.lookup(&"x".to_string()),
            Some((true, Type::TInteger))
        );
    }

    #[test]
    fn test_variable_reassignment_same_type() {
        let mut env = Environment::new();
        env.map_variable("x".to_string(), true, Type::TInteger);
        let stmt = Statement::Assignment("x".to_string(), Box::new(Expression::CInt(100)));

        let result = check_stmt(stmt, &env);

        assert!(result.is_ok());

        let (typed_stmt, _new_env) = result.unwrap();
        assert!(matches!(typed_stmt, TypedStatement::Assignment { .. }));
    }

    #[test]
    fn test_variable_reassignment_different_type() {
        let mut env = Environment::new();
        env.map_variable("x".to_string(), true, Type::TInteger);

        let stmt = Statement::Assignment(
            "x".to_string(),
            Box::new(Expression::CString("hello".to_string())),
        );

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

        assert!(result.is_ok());

        let (typed_stmt, _final_env) = result.unwrap();
        assert!(matches!(typed_stmt, TypedStatement::For { .. }));
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
        assert!(check_stmt(stmt, &env).is_err());
    }

    #[test]
    fn test_for_empty_list() {
        let env = Environment::new();
        let (_, env_after_decl) = check_stmt(
            Statement::VarDeclaration("x".to_string(), Box::new(Expression::CInt(0))),
            &env,
        )
        .unwrap();

        let stmt = Statement::For(
            "x".to_string(),
            Box::new(Expression::ListValue(vec![])),
            Box::new(Statement::VarDeclaration(
                "y".to_string(),
                Box::new(Expression::Var("x".to_string())),
            )),
        );

        let result = check_stmt(stmt, &env_after_decl);

        assert!(result.is_ok());
        
        let (typed_stmt, _final_env) = result.unwrap();
        if let TypedStatement::For { iterable, .. } = typed_stmt {
            assert_eq!(
                iterable.type_info(),
                &Type::TList(Box::new(Type::TAny))
            );
        } else {
            panic!("Expected a TypedStatement::For node.");
        }
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
        assert!(check_stmt(stmt, &env).is_err());
    }

    #[test]
    fn test_for_nested_loops() {
        let env = Environment::new();
        
        let (_, env_after_decl) = check_stmt(
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

        let result = check_stmt(stmt, &env_after_decl);

        assert!(result.is_ok());
        let (typed_stmt, final_env) = result.unwrap();

        assert!(matches!(typed_stmt, TypedStatement::For { .. }));

        assert_eq!(
        final_env.lookup(&"sum".to_string()),
        Some((true, Type::TInteger))
);
    }

    #[test]
    fn test_for_variable_scope() {
        let mut env = Environment::new();
        env.map_variable("x".to_string(), true, Type::TString); 

        let stmt = Statement::For(
            "x".to_string(), 
            Box::new(Expression::ListValue(vec![
                Expression::CInt(1),
                Expression::CInt(2),
            ])),
            Box::new(Statement::Assignment(
                "y".to_string(),
                Box::new(Expression::Var("x".to_string())),
            )),
        );

        // TODO: Let discuss this case here next class.
        assert!(check_stmt(stmt, &env).is_err());
    }
}
