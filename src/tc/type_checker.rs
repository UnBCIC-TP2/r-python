use crate::ir::ast::{EnvValue, Environment, Expression, Name, Statement, Type};

type ErrorMessage = String;

pub enum ControlType {
    Continue(Environment),
    Return(Type),
}

pub fn check_exp(exp: Expression, env: &Environment) -> Result<Type, ErrorMessage> {
    match exp {
        Expression::CTrue => Ok(Type::TBool),
        Expression::CFalse => Ok(Type::TBool),
        Expression::CInt(_) => Ok(Type::TInteger),
        Expression::CReal(_) => Ok(Type::TReal),
        Expression::CString(_) => Ok(Type::TString),
        Expression::Add(l, r) => check_bin_arithmetic_expression(*l, *r, env),
        Expression::Sub(l, r) => check_bin_arithmetic_expression(*l, *r, env),
        Expression::Mul(l, r) => check_bin_arithmetic_expression(*l, *r, env),
        Expression::Div(l, r) => check_bin_arithmetic_expression(*l, *r, env),
        Expression::And(l, r) => check_bin_boolean_expression(*l, *r, env),
        Expression::Or(l, r) => check_bin_boolean_expression(*l, *r, env),
        Expression::Not(e) => check_not_expression(*e, env),
        Expression::EQ(l, r) => check_bin_relational_expression(*l, *r, env),
        Expression::GT(l, r) => check_bin_relational_expression(*l, *r, env),
        Expression::LT(l, r) => check_bin_relational_expression(*l, *r, env),
        Expression::GTE(l, r) => check_bin_relational_expression(*l, *r, env),
        Expression::LTE(l, r) => check_bin_relational_expression(*l, *r, env),
        Expression::Var(name) => check_var_name(name, env),
        Expression::FuncCall(name, args) => check_func_call(name, args, env),
    }
}

pub fn check_stmt(
    stmt: Statement,
    env: &Environment,
    exptd_type: Option<Type>,
) -> Result<ControlType, ErrorMessage> {
    match stmt {
        Statement::Assignment(name, exp, kind) => {
            let mut new_env = env.clone();
            let exp_type = check_exp(*exp, &new_env)?;

            if let Some(def_type) = kind {
                if exp_type != def_type {
                    return Err(format!(
                        "[Type Error] cannot assign type '{:?}' to '{:?}' variable.",
                        exp_type, def_type
                    ));
                }
                new_env.insert(name, (None, exp_type));
            } else {
                let result_type = check_var_name(name.clone(), &new_env)?;
                if exp_type != result_type {
                    return Err(format!(
                        "[Type Error] variable '{}' is already defined as type '{:?}'.",
                        name.clone(),
                        result_type
                    ));
                }
                new_env.entry(name).and_modify(|e| e.1 = exp_type);
            }

            Ok(ControlType::Continue(new_env))
        }
        Statement::IfThenElse(exp, stmt_then, option) => {
            let new_env = env.clone();
            let exp_type = check_exp(*exp, &new_env)?;

            if exp_type != Type::TBool {
                return Err(format!("[Type Error] if expression must be boolean."));
            }

            let (new_env, stmt_then_type) =
                match check_stmt(*stmt_then, &new_env, exptd_type.clone())? {
                    ControlType::Continue(control_env) => (control_env, None),
                    ControlType::Return(control_type) => (new_env, Some(control_type)),
                };
            let (new_env, stmt_else_type) = match option {
                Some(stmt_else) => match check_stmt(*stmt_else, &new_env, exptd_type)? {
                    ControlType::Continue(control_env) => (control_env, None),
                    ControlType::Return(control_type) => (new_env, Some(control_type)),
                },
                None => (new_env, None),
            };

            match stmt_then_type.or(stmt_else_type) {
                Some(kind) => Ok(ControlType::Return(kind)),
                None => Ok(ControlType::Continue(new_env)),
            }
        }
        Statement::While(exp, stmt_while) => {
            let new_env = env.clone();
            let exp_type = check_exp(*exp, &new_env)?;

            if exp_type != Type::TBool {
                return Err(format!("[Type Error] while expression must be boolean."));
            }

            check_stmt(*stmt_while, &new_env, exptd_type)
        }
        Statement::Sequence(stmt1, stmt2) => {
            let new_env = env.clone();

            let (new_env, stmt1_type) = match check_stmt(*stmt1, &new_env, exptd_type.clone())? {
                ControlType::Continue(control_env) => (control_env, None),
                ControlType::Return(control_type) => (new_env, Some(control_type)),
            };
            let (new_env, stmt2_type) = match check_stmt(*stmt2, &new_env, exptd_type.clone())? {
                ControlType::Continue(control_env) => (control_env, None),
                ControlType::Return(control_type) => (new_env, Some(control_type)),
            };

            match stmt1_type.or(stmt2_type) {
                Some(kind) => Ok(ControlType::Return(kind)),
                None => Ok(ControlType::Continue(new_env)),
            }
        }
        Statement::FuncDef(name, func) => {
            let mut new_env = env.clone();
            new_env.insert(
                name.clone(),
                (Some(EnvValue::Func(func.clone())), Type::TFunction),
            );

            if let Some(params) = func.clone().params {
                for (param_name, param_type) in params {
                    new_env.insert(param_name, (None, param_type));
                }
            }

            match check_stmt(*func.clone().body, &new_env, Some(func.clone().kind))? {
                ControlType::Return(_) => Ok(ControlType::Continue(new_env)),
                ControlType::Continue(_) => Err(format!(
                    "[Type Error] '{}()' does not have a result statement.",
                    name
                )),
            }
        }
        Statement::Return(exp) => {
            let new_env = env.clone();
            let exp_type = check_exp(*exp, &new_env)?;

            if exptd_type == None {
                return Err(format!(
                    "[Type Error] return statement outside function body."
                ));
            }
            if exp_type != exptd_type.unwrap() {
                return Err(format!(
                    "[Type Error] return expression does not match function's return type."
                ));
            }

            Ok(ControlType::Return(exp_type))
        }
        Statement::MetaStmt(_, _) =>{
            Ok(ControlType::Continue(env.clone()))
        }
        _ => Err(String::from("not implemented yet")),
    }
}

fn check_func_call(
    name: String,
    args: Vec<Expression>,
    env: &Environment,
) -> Result<Type, ErrorMessage> {
    match env.get(&name) {
        Some((Some(EnvValue::Func(func)), _)) => {
            if let Some(params) = &func.params {
                if params.len() != args.len() {
                    return Err(format!(
                        "[Type Error] '{}()' expected {} argument(s) but got {}",
                        name,
                        params.len(),
                        args.len()
                    ));
                }

                for ((param, param_type), exp) in params.iter().zip(args) {
                    let exp_type = check_exp(exp, env)?;
                    if exp_type != *param_type {
                        return Err(format!(
                            "[Type Error] incorret type for argument '{}' in '{}()'.",
                            param, name
                        ));
                    }
                }
            }
            Ok(func.kind.clone())
        }
        Some(_) => Err(format!(
            "[Type Error] cannot call non-function object '{}'.",
            name
        )),
        None => Err(format!("[Type Error] '{}()' is not defined.", name)),
    }
}

fn check_var_name(name: Name, env: &Environment) -> Result<Type, ErrorMessage> {
    match env.get(&name) {
        Some(value) => Ok(value.clone().1),
        None => Err(format!("[Type Error] '{}' is not defined.", name)),
    }
}

fn check_bin_arithmetic_expression(
    left: Expression,
    right: Expression,
    env: &Environment,
) -> Result<Type, ErrorMessage> {
    let left_type = check_exp(left, env)?;
    let right_type = check_exp(right, env)?;

    match (left_type, right_type) {
        (Type::TInteger, Type::TInteger) => Ok(Type::TInteger),
        (Type::TInteger, Type::TReal) => Ok(Type::TReal),
        (Type::TReal, Type::TInteger) => Ok(Type::TReal),
        (Type::TReal, Type::TReal) => Ok(Type::TReal),
        _ => Err(String::from("[Type Error] expecting numeric type values.")),
    }
}

fn check_bin_boolean_expression(
    left: Expression,
    right: Expression,
    env: &Environment,
) -> Result<Type, ErrorMessage> {
    let left_type = check_exp(left, env)?;
    let right_type = check_exp(right, env)?;
    match (left_type, right_type) {
        (Type::TBool, Type::TBool) => Ok(Type::TBool),
        _ => Err(String::from("[Type Error] expecting boolean type values.")),
    }
}

fn check_not_expression(exp: Expression, env: &Environment) -> Result<Type, ErrorMessage> {
    let exp_type = check_exp(exp, env)?;

    match exp_type {
        Type::TBool => Ok(Type::TBool),
        _ => Err(String::from("[Type Error] expecting a boolean type value.")),
    }
}

fn check_bin_relational_expression(
    left: Expression,
    right: Expression,
    env: &Environment,
) -> Result<Type, ErrorMessage> {
    let left_type = check_exp(left, env)?;
    let right_type = check_exp(right, env)?;

    match (left_type, right_type) {
        (Type::TInteger, Type::TInteger) => Ok(Type::TBool),
        (Type::TInteger, Type::TReal) => Ok(Type::TBool),
        (Type::TReal, Type::TInteger) => Ok(Type::TBool),
        (Type::TReal, Type::TReal) => Ok(Type::TBool),
        _ => Err(String::from("[Type Error] expecting numeric type values.")),
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;

    use crate::ir::ast::Expression::*;
    use crate::ir::ast::Function;
    use crate::ir::ast::Statement::*;
    use crate::ir::ast::Type::*;

    #[test]
    fn check_tlist_comparison() {
        let t_list1 = TList(Box::new(TInteger));
        let t_list2 = TList(Box::new(TInteger));

        assert_eq!(t_list1 == t_list2, true);
    }

    #[test]
    fn check_tlist_comparison_different_types() {
        let t_list1 = TList(Box::new(TInteger));
        let t_list2 = TList(Box::new(TBool));

        assert_eq!(t_list1 == t_list2, false);
    }

    #[test]
    fn check_ttuple_comparison() {
        let t_tuple1 = TTuple(vec![TInteger, TBool]);
        let t_tuple2 = TTuple(vec![TInteger, TBool]);

        assert_eq!(t_tuple1 == t_tuple2, true);
    }

    #[test]
    fn check_ttuple_comparison_different_types() {
        let t_tuple1 = TTuple(vec![TInteger, TBool]);
        let t_tuple2 = TTuple(vec![TBool, TInteger]);

        assert_eq!(t_tuple1 == t_tuple2, false);
    }

    #[test]
    fn check_constant() {
        let env = HashMap::new();
        let c10 = CInt(10);
        assert_eq!(check_exp(c10, &env), Ok(TInteger));
    }

    #[test]
    fn check_add_integers() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let c20 = CInt(20);
        let add = Add(Box::new(c10), Box::new(c20));

        assert_eq!(check_exp(add, &env), Ok(TInteger));
    }

    #[test]
    fn check_add_reals() {
        let env = HashMap::new();
        let c10 = CReal(10.5);
        let c20 = CReal(20.3);
        let add = Add(Box::new(c10), Box::new(c20));

        assert_eq!(check_exp(add, &env), Ok(TReal));
    }

    #[test]
    fn check_add_real_and_integer() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let c20 = CReal(20.3);
        let add = Add(Box::new(c10), Box::new(c20));

        assert_eq!(check_exp(add, &env), Ok(TReal));
    }

    #[test]
    fn check_add_integer_and_real() {
        let env = HashMap::new();
        let c10 = CReal(10.5);
        let c20 = CInt(20);
        let add = Add(Box::new(c10), Box::new(c20));

        assert_eq!(check_exp(add, &env), Ok(TReal));
    }

    #[test]
    fn check_type_error_arithmetic_expression() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let bool = CFalse;
        let add = Add(Box::new(c10), Box::new(bool));

        assert_eq!(
            check_exp(add, &env),
            Err(String::from("[Type Error] expecting numeric type values."))
        );
    }

    #[test]
    fn check_type_error_not_expression() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let not = Not(Box::new(c10));

        assert_eq!(
            check_exp(not, &env),
            Err(String::from("[Type Error] expecting a boolean type value."))
        );
    }

    #[test]
    fn check_type_error_and_expression() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let bool = CTrue;
        let and = And(Box::new(c10), Box::new(bool));

        assert_eq!(
            check_exp(and, &env),
            Err(String::from("[Type Error] expecting boolean type values."))
        );
    }

    #[test]
    fn check_type_error_or_expression() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let bool = CTrue;
        let or = Or(Box::new(c10), Box::new(bool));

        assert_eq!(
            check_exp(or, &env),
            Err(String::from("[Type Error] expecting boolean type values."))
        );
    }

    #[test]
    fn check_assignment() {
        let env = Environment::new();
        let assignment = Assignment("a".to_string(), Box::new(CTrue), Some(TBool));

        match check_stmt(assignment, &env, None) {
            Ok(ControlType::Continue(new_env)) => {
                assert_eq!(new_env.get("a"), Some((None, TBool)).as_ref());
            }
            Ok(_) => assert!(false),
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn check_assignment_error1() {
        let env = Environment::new();
        let assignment = Assignment("a".to_string(), Box::new(CTrue), Some(TInteger));

        match check_stmt(assignment, &env, None) {
            Ok(_) => assert!(false),
            Err(s) => assert_eq!(
                s,
                "[Type Error] cannot assign type 'TBool' to 'TInteger' variable."
            ),
        }
    }

    #[test]
    fn check_assignment_error2() {
        let env = Environment::new();
        let assignment1 = Assignment("a".to_string(), Box::new(CTrue), Some(TBool));

        let assignment2 = Assignment("a".to_string(), Box::new(CInt(1)), None);

        let program = Sequence(Box::new(assignment1), Box::new(assignment2));

        match check_stmt(program, &env, None) {
            Ok(_) => assert!(false),
            Err(s) => assert_eq!(
                s,
                "[Type Error] variable 'a' is already defined as type 'TBool'."
            ),
        }
    }

    #[test]
    fn check_if_then_else() {
        let env = Environment::new();
        let ifthenelse = IfThenElse(
            Box::new(CTrue),
            Box::new(Assignment(
                "a".to_string(),
                Box::new(CInt(1)),
                Some(TInteger),
            )),
            Some(Box::new(Assignment(
                "b".to_string(),
                Box::new(CReal(2.0)),
                Some(TReal),
            ))),
        );

        match check_stmt(ifthenelse, &env, None) {
            Ok(ControlType::Continue(new_env)) => {
                assert_eq!(new_env.get("a"), Some((None, TInteger)).as_ref());
                assert_eq!(new_env.get("b"), Some((None, TReal)).as_ref());
            }
            Ok(_) => assert!(false),
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn check_if_then_else_error() {
        let env = Environment::new();
        let ifthenelse = IfThenElse(
            Box::new(CInt(1)),
            Box::new(Assignment(
                "a".to_string(),
                Box::new(CInt(1)),
                Some(TInteger),
            )),
            Some(Box::new(Assignment(
                "b".to_string(),
                Box::new(CReal(2.0)),
                Some(TReal),
            ))),
        );

        match check_stmt(ifthenelse, &env, None) {
            Ok(_) => assert!(false),
            Err(s) => assert_eq!(s, "[Type Error] if expression must be boolean."),
        }
    }

    #[test]
    fn check_while() {
        let env = Environment::new();
        let assignment1 = Assignment("a".to_string(), Box::new(CInt(3)), Some(TInteger));
        let assignment2 = Assignment("b".to_string(), Box::new(CInt(0)), Some(TInteger));
        let while_stmt = While(
            Box::new(GT(Box::new(Var("a".to_string())), Box::new(CInt(0)))),
            Box::new(Assignment(
                "b".to_string(),
                Box::new(Add(Box::new(Var("b".to_string())), Box::new(CInt(1)))),
                None,
            )),
        );
        let program = Sequence(
            Box::new(assignment1),
            Box::new(Sequence(Box::new(assignment2), Box::new(while_stmt))),
        );

        match check_stmt(program, &env, None) {
            Ok(ControlType::Continue(new_env)) => {
                assert_eq!(new_env.get("a"), Some((None, TInteger)).as_ref());
                assert_eq!(new_env.get("b"), Some((None, TInteger)).as_ref());
            }
            Ok(_) => assert!(false),
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn check_while_error() {
        let env = Environment::new();
        let assignment1 = Assignment("a".to_string(), Box::new(CInt(3)), Some(TInteger));
        let assignment2 = Assignment("b".to_string(), Box::new(CInt(0)), Some(TInteger));
        let while_stmt = While(
            Box::new(CInt(1)),
            Box::new(Assignment(
                "b".to_string(),
                Box::new(Add(Box::new(Var("b".to_string())), Box::new(CInt(1)))),
                None,
            )),
        );
        let program = Sequence(
            Box::new(assignment1),
            Box::new(Sequence(Box::new(assignment2), Box::new(while_stmt))),
        );

        match check_stmt(program, &env, None) {
            Ok(_) => assert!(false),
            Err(s) => assert_eq!(s, "[Type Error] while expression must be boolean."),
        }
    }

    #[test]
    fn check_func_def() {
        let env = Environment::new();
        let func = FuncDef(
            "add".to_string(),
            Function {
                kind: TInteger,
                params: Some(vec![
                    ("a".to_string(), TInteger),
                    ("b".to_string(), TInteger),
                ]),
                body: Box::new(Return(Box::new(Add(
                    Box::new(Var("a".to_string())),
                    Box::new(Var("b".to_string())),
                )))),
            },
        );

        match check_stmt(func, &env, None) {
            Ok(ControlType::Continue(new_env)) => {
                assert_eq!(
                    new_env.get("add"),
                    Some((
                        Some(EnvValue::Func(Function {
                            kind: TInteger,
                            params: Some(vec![
                                ("a".to_string(), TInteger),
                                ("b".to_string(), TInteger)
                            ]),
                            body: Box::new(Return(Box::new(Add(
                                Box::new(Var("a".to_string())),
                                Box::new(Var("b".to_string()))
                            ))))
                        })),
                        TFunction
                    ))
                    .as_ref()
                );
            }
            Ok(_) => assert!(false),
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn check_func_def_error1() {
        let env = Environment::new();
        let func = FuncDef(
            "add".to_string(),
            Function {
                kind: TInteger,
                params: Some(vec![
                    ("a".to_string(), TInteger),
                    ("b".to_string(), TInteger),
                ]),
                body: Box::new(Assignment(
                    "a".to_string(),
                    Box::new(CInt(1)),
                    Some(TInteger),
                )),
            },
        );

        match check_stmt(func, &env, None) {
            Ok(_) => assert!(false),
            Err(s) => assert_eq!(s, "[Type Error] 'add()' does not have a result statement."),
        }
    }

    #[test]
    fn check_func_def_error2() {
        let env = Environment::new();
        let func = FuncDef(
            "add".to_string(),
            Function {
                kind: TInteger,
                params: Some(vec![
                    ("a".to_string(), TInteger),
                    ("b".to_string(), TInteger),
                ]),
                body: Box::new(Return(Box::new(CTrue))),
            },
        );

        match check_stmt(func, &env, None) {
            Ok(_) => assert!(false),
            Err(s) => assert_eq!(
                s,
                "[Type Error] return expression does not match function's return type."
            ),
        }
    }

    #[test]
    fn check_return_outside_function() {
        let env = Environment::new();
        let retrn = Return(Box::new(CInt(1)));

        match check_stmt(retrn, &env, None) {
            Ok(_) => assert!(false),
            Err(s) => assert_eq!(s, "[Type Error] return statement outside function body."),
        }
    }

    #[test]
    fn check_function_call_wrong_args() {
        let env = Environment::new();

        let func = FuncDef(
            "add".to_string(),
            Function {
                kind: TInteger,
                params: Some(vec![
                    ("a".to_string(), TInteger),
                    ("b".to_string(), TInteger),
                ]),
                body: Box::new(Sequence(
                    Box::new(Assignment(
                        "c".to_string(),
                        Box::new(Add(
                            Box::new(Var("a".to_string())),
                            Box::new(Var("b".to_string())),
                        )),
                        Some(TInteger),
                    )),
                    Box::new(Return(Box::new(Var("c".to_string())))),
                )),
            },
        );

        let program1 = Sequence(
            Box::new(func.clone()),
            Box::new(Assignment(
                "var".to_string(),
                Box::new(FuncCall("add".to_string(), vec![CInt(1)])),
                Some(TInteger),
            )),
        );

        let program2 = Sequence(
            Box::new(func),
            Box::new(Assignment(
                "var".to_string(),
                Box::new(FuncCall("add".to_string(), vec![CInt(1), CInt(2), CInt(3)])),
                Some(TInteger),
            )),
        );

        match check_stmt(program1, &env.clone(), None) {
            Ok(_) => assert!(false),
            Err(s) => assert_eq!(s, "[Type Error] 'add()' expected 2 argument(s) but got 1"),
        }

        match check_stmt(program2, &env, None) {
            Ok(_) => assert!(false),
            Err(s) => assert_eq!(s, "[Type Error] 'add()' expected 2 argument(s) but got 3"),
        }
    }

    #[test]
    fn check_function_call_wrong_type() {
        let env = Environment::new();

        let func = FuncDef(
            "add".to_string(),
            Function {
                kind: TInteger,
                params: Some(vec![
                    ("a".to_string(), TInteger),
                    ("b".to_string(), TInteger),
                ]),
                body: Box::new(Sequence(
                    Box::new(Assignment(
                        "c".to_string(),
                        Box::new(Add(
                            Box::new(Var("a".to_string())),
                            Box::new(Var("b".to_string())),
                        )),
                        Some(TInteger),
                    )),
                    Box::new(Return(Box::new(Var("c".to_string())))),
                )),
            },
        );

        let program = Sequence(
            Box::new(func.clone()),
            Box::new(Assignment(
                "var".to_string(),
                Box::new(FuncCall("add".to_string(), vec![CInt(1), CTrue])),
                Some(TInteger),
            )),
        );

        match check_stmt(program, &env.clone(), None) {
            Ok(_) => assert!(false),
            Err(s) => assert_eq!(s, "[Type Error] incorret type for argument 'b' in 'add()'."),
        }
    }

    #[test]
    fn check_function_call_non_function() {
        let env = Environment::new();
        let program = Sequence(
            Box::new(Assignment(
                "a".to_string(),
                Box::new(CInt(1)),
                Some(TInteger),
            )),
            Box::new(Assignment(
                "b".to_string(),
                Box::new(FuncCall("a".to_string(), vec![])),
                Some(TInteger),
            )),
        );

        match check_stmt(program, &env.clone(), None) {
            Ok(_) => assert!(false),
            Err(s) => assert_eq!(s, "[Type Error] cannot call non-function object 'a'."),
        }
    }

    #[test]
    fn check_function_call_undefined() {
        let env = Environment::new();
        let program = Assignment(
            "a".to_string(),
            Box::new(FuncCall("func".to_string(), vec![])),
            Some(TInteger),
        );

        match check_stmt(program, &env.clone(), None) {
            Ok(_) => assert!(false),
            Err(s) => assert_eq!(s, "[Type Error] 'func()' is not defined."),
        }
    }
}
