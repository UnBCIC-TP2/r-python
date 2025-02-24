use crate::ir::ast::{Environment, Expression, Name, Statement, Type};

type ErrorMessage = String;

pub enum ControlType {
    Continue(Environment<Type>),
    Return(Type),
}

pub fn check_exp(exp: Expression, env: &Environment<Type>) -> Result<Type, ErrorMessage> {
    match exp {
        Expression::CTrue => Ok(Type::TBool),
        Expression::CFalse => Ok(Type::TBool),
        Expression::CVoid => Ok(Type::TVoid),
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
        Expression::Var(name) => check_var_name(name, env, false),

        Expression::COk(e) => check_result_ok(*e, env),
        Expression::CErr(e) => check_result_err(*e, env),
        Expression::CJust(e) => check_maybe_just(*e, env),
        Expression::CNothing => Ok(Type::TMaybe(Box::new(Type::TAny))),
        Expression::IsError(e) => check_iserror_type(*e, env),
        Expression::IsNothing(e) => check_isnothing_type(*e, env),
        Expression::Unwrap(e) => check_unwrap_type(*e, env),
        Expression::Propagate(e) => check_propagate_type(*e, env),
        Expression::FuncCall(name, args) => check_func_call(name, args, env),
        //_ => Err(String::from("not implemented yet")),
    }
}

pub fn check_stmt(stmt: Statement, env: &Environment<Type>) -> Result<ControlType, ErrorMessage> {
    let mut new_env = env.clone();

    match stmt {
        Statement::Assignment(name, exp, kind) => {
            let exp_type = check_exp(*exp, &new_env)?;
            if let Ok(stated_type) = check_var_name(name.clone(), &new_env, true) {
                if exp_type != stated_type {
                    return Err(format!("[Type Error on '{}()'] '{}' has mismatched types: expected '{:?}', found '{:?}'.", new_env.scope_name(), name, stated_type, exp_type));
                }
            } else {
                new_env.insert_variable(name, exp_type);
            }
            Ok(ControlType::Continue(new_env))
        }
        Statement::IfThenElse(exp, stmt_then, option) => {
            let exp_type = check_exp(*exp, &new_env)?;

            if exp_type != Type::TBool {
                return Err(format!(
                    "[Type Error on '{}()'] if expression must be boolean.",
                    new_env.scope_name()
                ));
            }

            let stmt_then_result = check_stmt(*stmt_then, &new_env)?;
            let stmt_else_result = match option {
                Some(stmt_else) => check_stmt(*stmt_else, &new_env)?,
                None => return Ok(ControlType::Continue(new_env)),
            };

            match (stmt_then_result, stmt_else_result) {
                (ControlType::Return(kind), ControlType::Continue(_)) => {
                    Ok(ControlType::Return(kind))
                }
                (ControlType::Continue(_), ControlType::Return(kind)) => {
                    Ok(ControlType::Return(kind))
                }
                (ControlType::Return(kind1), ControlType::Return(_)) => {
                    Ok(ControlType::Return(kind1))
                }
                _ => Ok(ControlType::Continue(new_env)),
            }
        }
        Statement::While(exp, stmt_while) => {
            let exp_type = check_exp(*exp, &new_env)?;

            if exp_type != Type::TBool {
                return Err(format!(
                    "[Type Error on '{}()'] while expression must be boolean.",
                    new_env.scope_name()
                ));
            }

            match check_stmt(*stmt_while, &new_env)? {
                ControlType::Continue(_) => Ok(ControlType::Continue(new_env)),
                ControlType::Return(kind) => Ok(ControlType::Return(kind)),
            }
        }

        Statement::Block(stmts) => {
            for stmt in stmts {
                let (check_env, stmt_type) = match check_stmt(stmt, &new_env)? {
                    ControlType::Continue(control_env) => (control_env, None),
                    ControlType::Return(control_type) => (new_env, Some(control_type)),
                };
                if let Some(kind) = stmt_type {
                    return Ok(ControlType::Return(kind));
                } else {
                    new_env = check_env;
                }
            }
            Ok(ControlType::Continue(new_env))
        }

        Statement::Sequence(stmt1, stmt2) => {
            if let ControlType::Continue(control_env) = check_stmt(*stmt1, &new_env)? {
                new_env = control_env;
            }
            check_stmt(*stmt2, &new_env)
        }
        Statement::FuncDef(func) => {
            new_env.insert_frame(func.clone());

            let mut type_vec = vec![];

            if let Some(params) = func.params.clone() {
                // Adicionamos a verificação de parâmetros duplicados
                check_duplicate_params(&params)?;

                for (param_name, param_kind) in params {
                    new_env.insert_variable(param_name, param_kind.clone());
                    type_vec.push(param_kind);
                }
            }

            let func_type = Type::TFunction(Box::new(func.kind), type_vec);

            if let None = new_env.search_frame(func.name.clone()) {
                new_env.insert_variable(func.name.clone(), func_type.clone());
            }

            match check_stmt(*func.body.unwrap(), &new_env)? {
                ControlType::Continue(_) => Err(format!(
                    "[Syntax Error] '{}()' does not have a return statement.",
                    func.name
                )),
                ControlType::Return(_) => {
                    new_env.remove_frame();
                    new_env.insert_variable(func.name, func_type);
                    Ok(ControlType::Continue(new_env))
                }
            }
        }
        Statement::Return(exp) => {
            let exp_type = check_exp(*exp, &new_env)?;

            if let Some(Type::TFunction(func_type, _)) = new_env.scope_return() {
                if exp_type != func_type.clone().unwrap() {
                    return Err(format!(
                        "[Type Error] '{}()' has mismatched types: expected '{:?}', found '{:?}'.",
                        new_env.scope_name(),
                        func_type.clone().unwrap(),
                        exp_type
                    ));
                }

                Ok(ControlType::Return(exp_type))
            } else {
                Err(format!("[Syntax Error] return statement outside function."))
            }
        }
        _ => Err(String::from("not implemented yet.")),
    }
}

fn check_func_call(
    name: String,
    args: Vec<Expression>,
    env: &Environment<Type>,
) -> Result<Type, ErrorMessage> {
    match check_var_name(name.clone(), env, false) {
        Ok(Type::TFunction(kind, type_vec)) => {
            if args.len() != type_vec.len() {
                return Err(format!(
                    "[Type Error on '{}()'] '{}()' expected {} arguments, found {}.",
                    env.scope_name(),
                    name,
                    type_vec.len(),
                    args.len()
                ));
            }

            for (arg, param_type) in args.iter().zip(type_vec) {
                let arg_type = check_exp(arg.clone(), env)?;
                if arg_type != param_type {
                    return Err(format!("[Type Error on '{}()'] '{}()' has mismatched arguments: expected '{:?}', found '{:?}'.", env.scope_name(), name, param_type, arg_type));
                }
            }

            Ok(kind.unwrap())
        }
        _ => Err(format!(
            "[Name Error on '{}()'] '{}()' is not defined.",
            env.scope_name(),
            name
        )),
    }
}

fn check_duplicate_params(params: &Vec<(Name, Type)>) -> Result<(), ErrorMessage> {
    let mut seen_params = std::collections::HashSet::new();

    for (name, _) in params {
        if !seen_params.insert(name.clone()) {
            return Err(format!(
                "[Parameter Error] Duplicate parameter name '{}'",
                name
            ));
        }
    }

    Ok(())
}

fn check_var_name(name: Name, env: &Environment<Type>, scoped: bool) -> Result<Type, ErrorMessage> {
    let mut curr_scope = env.scope_key();

    loop {
        let frame = env.get_frame(curr_scope.clone());

        match frame.variables.get(&name) {
            Some(kind) => {
                if scoped && curr_scope != env.scope_key() {
                    return Err(format!(
                        "[Local Name Error on '{}'] cannot access local variable '{}'.",
                        env.scope_name(),
                        name
                    ));
                } else {
                    return Ok(kind.clone());
                }
            }
            None => match &frame.parent_key {
                Some(parent) => curr_scope = parent.clone(),
                None => {
                    return Err(format!(
                        "[Name Error on '{}'] '{}' is not defined.",
                        env.scope_name(),
                        name
                    ))
                }
            },
        }
    }
}

fn check_bin_arithmetic_expression(
    left: Expression,
    right: Expression,
    env: &Environment<Type>,
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
    env: &Environment<Type>,
) -> Result<Type, ErrorMessage> {
    let left_type = check_exp(left, env)?;
    let right_type = check_exp(right, env)?;
    match (left_type, right_type) {
        (Type::TBool, Type::TBool) => Ok(Type::TBool),
        _ => Err(String::from("[Type Error] expecting boolean type values.")),
    }
}

fn check_not_expression(exp: Expression, env: &Environment<Type>) -> Result<Type, ErrorMessage> {
    let exp_type = check_exp(exp, env)?;

    match exp_type {
        Type::TBool => Ok(Type::TBool),
        _ => Err(String::from("[Type Error] expecting a boolean type value.")),
    }
}

fn check_bin_relational_expression(
    left: Expression,
    right: Expression,
    env: &Environment<Type>,
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

fn check_result_ok(exp: Expression, env: &Environment<Type>) -> Result<Type, ErrorMessage> {
    let exp_type = check_exp(exp, env)?;
    return Ok(Type::TResult(Box::new(exp_type), Box::new(Type::TAny)));
}

fn check_result_err(exp: Expression, env: &Environment<Type>) -> Result<Type, ErrorMessage> {
    let exp_type = check_exp(exp, env)?;
    return Ok(Type::TResult(Box::new(Type::TAny), Box::new(exp_type)));
}

fn check_unwrap_type(exp: Expression, env: &Environment<Type>) -> Result<Type, ErrorMessage> {
    let exp_type = check_exp(exp, env)?;

    match exp_type {
        Type::TMaybe(t) => Ok(*t),
        Type::TResult(tl, _) => Ok(*tl),
        _ => Err(String::from(
            "[Type Error] expecting a maybe or result type value.",
        )),
    }
}

fn check_propagate_type(exp: Expression, env: &Environment<Type>) -> Result<Type, ErrorMessage> {
    let exp_type = check_exp(exp, env)?;

    match exp_type {
        Type::TMaybe(t) => Ok(*t),
        Type::TResult(tl, _) => Ok(*tl),
        _ => Err(String::from(
            "[Type Error] expecting a maybe or result type value.",
        )),
    }
}

fn check_maybe_just(exp: Expression, env: &Environment<Type>) -> Result<Type, ErrorMessage> {
    let exp_type = check_exp(exp, env)?;
    Ok(Type::TMaybe(Box::new(exp_type)))
}

fn check_iserror_type(exp: Expression, env: &Environment<Type>) -> Result<Type, ErrorMessage> {
    let v = check_exp(exp, env)?;

    match v {
        Type::TResult(_, _) => Ok(Type::TBool),
        _ => Err(String::from("[Type Error] expecting a result type value.")),
    }
}

fn check_isnothing_type(exp: Expression, env: &Environment<Type>) -> Result<Type, ErrorMessage> {
    let exp_type = check_exp(exp, env)?;

    match exp_type {
        Type::TMaybe(_) => Ok(Type::TBool),
        _ => Err(String::from("[Type Error] expecting a maybe type value.")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::ir::ast::Environment;
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
        let env = Environment::new();

        let c10 = CInt(10);

        assert_eq!(check_exp(c10, &env), Ok(TInteger));
    }

    #[test]
    fn check_add_integers() {
        let env = Environment::new();

        let c10 = CInt(10);
        let c20 = CInt(20);
        let add = Add(Box::new(c10), Box::new(c20));

        assert_eq!(check_exp(add, &env), Ok(TInteger));
    }

    #[test]
    fn check_add_reals() {
        let env = Environment::new();

        let c10 = CReal(10.5);
        let c20 = CReal(20.3);
        let add = Add(Box::new(c10), Box::new(c20));

        assert_eq!(check_exp(add, &env), Ok(TReal));
    }

    #[test]
    fn check_add_real_and_integer() {
        let env = Environment::new();

        let c10 = CInt(10);
        let c20 = CReal(20.3);
        let add = Add(Box::new(c10), Box::new(c20));

        assert_eq!(check_exp(add, &env), Ok(TReal));
    }

    #[test]
    fn check_add_integer_and_real() {
        let env = Environment::new();

        let c10 = CReal(10.5);
        let c20 = CInt(20);
        let add = Add(Box::new(c10), Box::new(c20));

        assert_eq!(check_exp(add, &env), Ok(TReal));
    }

    #[test]
    fn check_type_error_arithmetic_expression() {
        let env = Environment::new();

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
        let env = Environment::new();

        let c10 = CInt(10);
        let not = Not(Box::new(c10));

        assert_eq!(
            check_exp(not, &env),
            Err(String::from("[Type Error] expecting a boolean type value."))
        );
    }

    #[test]
    fn check_type_error_and_expression() {
        let env = Environment::new();

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
        let env = Environment::new();

        let c10 = CInt(10);
        let bool = CTrue;
        let or = Or(Box::new(c10), Box::new(bool));

        assert_eq!(
            check_exp(or, &env),
            Err(String::from("[Type Error] expecting boolean type values."))
        );
    }

    #[test]
    fn check_ok_result() {
        let env = Environment::new();
        let f10 = CReal(10.0);
        let ok = COk(Box::new(f10));

        assert_eq!(
            check_exp(ok, &env),
            Ok(TResult(Box::new(TReal), Box::new(TAny)))
        );
    }

    #[test]
    fn check_err_result() {
        let env = Environment::new();
        let ecode = CInt(1);
        let err = CErr(Box::new(ecode));

        assert_eq!(
            check_exp(err, &env),
            Ok(TResult(Box::new(TAny), Box::new(TInteger)))
        );
    }

    #[test]
    fn check_just_integer() {
        let env = Environment::new();
        let c5 = CInt(5);
        let just = CJust(Box::new(c5));

        assert_eq!(check_exp(just, &env), Ok(TMaybe(Box::new(TInteger))))
    }

    #[test]
    fn check_is_error_result_positive() {
        let env = Environment::new();
        let bool = CTrue;
        let ok = COk(Box::new(bool));
        let ie = IsError(Box::new(ok));

        assert_eq!(check_exp(ie, &env), Ok(TBool));
    }

    #[test]
    fn check_is_error_result_error() {
        let env = Environment::new();
        let bool = CTrue;
        let ie = IsError(Box::new(bool));

        assert_eq!(
            check_exp(ie, &env),
            Err(String::from("[Type Error] expecting a result type value."))
        );
    }

    #[test]
    fn check_nothing() {
        let env = Environment::new();

        assert_eq!(check_exp(CNothing, &env), Ok(TMaybe(Box::new(TAny))));
    }

    #[test]
    fn check_is_nothing_on_maybe() {
        let env = Environment::new();
        let c5 = CInt(5);
        let just = CJust(Box::new(c5));
        let is_nothing = IsNothing(Box::new(just));

        assert_eq!(check_exp(is_nothing, &env), Ok(TBool));
    }

    #[test]
    fn check_is_nothing_type_error() {
        let env = Environment::new();
        let c5 = CInt(5);
        let is_nothing = IsNothing(Box::new(c5));

        assert_eq!(
            check_exp(is_nothing, &env),
            Err(String::from("[Type Error] expecting a maybe type value."))
        );
    }

    #[test]
    fn check_unwrap_maybe() {
        let env = Environment::new();
        let c5 = CInt(5);
        let some = CJust(Box::new(c5));
        let u = Unwrap(Box::new(some));

        assert_eq!(check_exp(u, &env), Ok(TInteger));
    }

    #[test]
    fn check_unwrap_maybe_type_error() {
        let env = Environment::new();
        let c5 = CInt(5);
        let u = Unwrap(Box::new(c5));

        assert_eq!(
            check_exp(u, &env),
            Err(String::from(
                "[Type Error] expecting a maybe or result type value."
            ))
        );
    }

    #[test]
    fn check_unwrap_result() {
        let env = Environment::new();
        let bool = CTrue;
        let ok = COk(Box::new(bool));
        let u = Unwrap(Box::new(ok));

        assert_eq!(check_exp(u, &env), Ok(TBool));
    }

    #[test]
    fn check_propagate_maybe() {
        let env = Environment::new();
        let c5 = CInt(5);
        let some = CJust(Box::new(c5));
        let u = Propagate(Box::new(some));

        assert_eq!(check_exp(u, &env), Ok(TInteger));
    }

    #[test]
    fn check_propagate_maybe_type_error() {
        let env = Environment::new();
        let c5 = CInt(5);
        let u = Propagate(Box::new(c5));

        assert_eq!(
            check_exp(u, &env),
            Err(String::from(
                "[Type Error] expecting a maybe or result type value."
            ))
        );
    }

    #[test]
    fn check_propagate_result() {
        let env = Environment::new();
        let bool = CTrue;
        let ok = COk(Box::new(bool));
        let u = Propagate(Box::new(ok));

        assert_eq!(check_exp(u, &env), Ok(TBool));
    }

    #[test]
    fn check_assignment() {
        let env: Environment<Type> = Environment::new();

        let assignment = Assignment("a".to_string(), Box::new(CTrue), Some(TAny));

        match check_stmt(assignment, &env) {
            Ok(ControlType::Continue(new_env)) => {
                assert_eq!(new_env.search_frame("a".to_string()), Some(TBool).as_ref());
            }
            Ok(_) => assert!(false),
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn check_assignment_error1() {
        let env: Environment<Type> = Environment::new();
        let assignment1 = Assignment("a".to_string(), Box::new(CInt(10)), Some(TAny));
        let assignment2 = Assignment("a".to_string(), Box::new(CTrue), Some(TAny));
        let program = Sequence(Box::new(assignment1), Box::new(assignment2));

        match check_stmt(program, &env) {
            Ok(_) => assert!(false),
            Err(s) => assert_eq!(
                s,
                "[Type Error on '__main__()'] 'a' has mismatched types: expected 'TInteger', found 'TBool'."
            ),
        }
    }

    #[test]
    fn check_assignment_error2() {
        let env: Environment<Type> = Environment::new();

        let assignment1 = Assignment("a".to_string(), Box::new(CTrue), Some(TAny));
        let assignment2 = Assignment("a".to_string(), Box::new(CInt(1)), Some(TAny));
        let program = Sequence(Box::new(assignment1), Box::new(assignment2));

        match check_stmt(program, &env) {
            Ok(_) => assert!(false),
            Err(s) => assert_eq!(
                s,
                "[Type Error on '__main__()'] 'a' has mismatched types: expected 'TBool', found 'TInteger'."
            ),
        }
    }

    #[test]
    fn check_if_then_else_error() {
        let env: Environment<Type> = Environment::new();

        let ifthenelse = IfThenElse(
            Box::new(CInt(1)),
            Box::new(Assignment("a".to_string(), Box::new(CInt(1)), Some(TAny))),
            Some(Box::new(Assignment(
                "b".to_string(),
                Box::new(CReal(2.0)),
                Some(TAny),
            ))),
        );

        match check_stmt(ifthenelse, &env) {
            Ok(_) => assert!(false),
            Err(s) => assert_eq!(
                s,
                "[Type Error on '__main__()'] if expression must be boolean."
            ),
        }
    }

    #[test]
    fn check_while_error() {
        let env: Environment<Type> = Environment::new();

        let assignment1 = Assignment("a".to_string(), Box::new(CInt(3)), Some(TAny));
        let assignment2 = Assignment("b".to_string(), Box::new(CInt(0)), Some(TAny));
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

        match check_stmt(program, &env) {
            Ok(_) => assert!(false),
            Err(s) => assert_eq!(
                s,
                "[Type Error on '__main__()'] while expression must be boolean."
            ),
        }
    }

    #[test]
    fn check_func_def() {
        let env: Environment<Type> = Environment::new();

        let func = FuncDef(Function {
            name: "add".to_string(),
            kind: Some(TInteger),
            params: Some(vec![
                ("a".to_string(), TInteger),
                ("b".to_string(), TInteger),
            ]),
            body: Some(Box::new(Return(Box::new(Add(
                Box::new(Var("a".to_string())),
                Box::new(Var("b".to_string())),
            ))))),
        });

        match check_stmt(func, &env) {
            Ok(ControlType::Continue(new_env)) => {
                assert_eq!(
                    new_env.search_frame("add".to_string()),
                    Some(TFunction(
                        Box::new(Some(TInteger)),
                        vec![TInteger, TInteger]
                    ))
                    .as_ref()
                );
            }
            Ok(_) => assert!(false),
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn check_func_def_error() {
        let env: Environment<Type> = Environment::new();

        let func = FuncDef(Function {
            name: "add".to_string(),
            kind: Some(TInteger),
            params: Some(vec![
                ("a".to_string(), TInteger),
                ("b".to_string(), TInteger),
            ]),
            body: Some(Box::new(Return(Box::new(CTrue)))),
        });

        match check_stmt(func, &env) {
            Ok(_) => assert!(false),
            Err(s) => assert_eq!(
                s,
                "[Type Error] 'add()' has mismatched types: expected 'TInteger', found 'TBool'."
            ),
        }
    }

    #[test]
    fn check_return_outside_function() {
        let env: Environment<Type> = Environment::new();

        let retrn = Return(Box::new(CInt(1)));

        match check_stmt(retrn, &env) {
            Ok(_) => assert!(false),
            Err(s) => assert_eq!(s, "[Syntax Error] return statement outside function."),
        }
    }

    #[test]
    fn check_function_call_wrong_args() {
        let env: Environment<Type> = Environment::new();

        let func = FuncDef(Function {
            name: "add".to_string(),
            kind: Some(TInteger),
            params: Some(vec![
                ("a".to_string(), TInteger),
                ("b".to_string(), TInteger),
            ]),
            body: Some(Box::new(Sequence(
                Box::new(Assignment(
                    "c".to_string(),
                    Box::new(Add(
                        Box::new(Var("a".to_string())),
                        Box::new(Var("b".to_string())),
                    )),
                    Some(TInteger),
                )),
                Box::new(Return(Box::new(Var("c".to_string())))),
            ))),
        });
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

        match check_stmt(program1, &env.clone()) {
            Ok(_) => assert!(false),
            Err(s) => assert_eq!(
                s,
                "[Type Error on '__main__()'] 'add()' expected 2 arguments, found 1."
            ),
        }
        match check_stmt(program2, &env) {
            Ok(_) => assert!(false),
            Err(s) => assert_eq!(
                s,
                "[Type Error on '__main__()'] 'add()' expected 2 arguments, found 3."
            ),
        }
    }

    #[test]
    fn check_function_call_wrong_type() {
        let env: Environment<Type> = Environment::new();

        let func = FuncDef(Function {
            name: "add".to_string(),
            kind: Some(TInteger),
            params: Some(vec![
                ("a".to_string(), TInteger),
                ("b".to_string(), TInteger),
            ]),
            body: Some(Box::new(Sequence(
                Box::new(Assignment(
                    "c".to_string(),
                    Box::new(Add(
                        Box::new(Var("a".to_string())),
                        Box::new(Var("b".to_string())),
                    )),
                    Some(TInteger),
                )),
                Box::new(Return(Box::new(Var("c".to_string())))),
            ))),
        });
        let program = Sequence(
            Box::new(func.clone()),
            Box::new(Assignment(
                "var".to_string(),
                Box::new(FuncCall("add".to_string(), vec![CInt(1), CTrue])),
                Some(TInteger),
            )),
        );

        match check_stmt(program, &env.clone()) {
            Ok(_) => assert!(false),
            Err(s) => assert_eq!(s, "[Type Error on '__main__()'] 'add()' has mismatched arguments: expected 'TInteger', found 'TBool'."),
        }
    }

    #[test]
    fn check_function_call_non_function() {
        let env: Environment<Type> = Environment::new();

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

        match check_stmt(program, &env.clone()) {
            Ok(_) => assert!(false),
            Err(s) => assert_eq!(s, "[Name Error on '__main__()'] 'a()' is not defined."),
        }
    }

    #[test]
    fn check_function_call_undefined() {
        let env: Environment<Type> = Environment::new();

        let program = Assignment(
            "a".to_string(),
            Box::new(FuncCall("func".to_string(), vec![])),
            Some(TInteger),
        );

        match check_stmt(program, &env.clone()) {
            Ok(_) => assert!(false),
            Err(s) => assert_eq!(s, "[Name Error on '__main__()'] 'func()' is not defined."),
        }
    }
    #[test]
    fn check_recursive_function() {
        let env: Environment<Type> = Environment::new();

        // Definição de função fatorial recursiva
        let factorial = FuncDef(Function {
            name: "factorial".to_string(),
            kind: Some(TInteger),
            params: Some(vec![("n".to_string(), TInteger)]),
            body: Some(Box::new(IfThenElse(
                Box::new(EQ(Box::new(Var("n".to_string())), Box::new(CInt(0)))),
                Box::new(Return(Box::new(CInt(1)))),
                Some(Box::new(Return(Box::new(Mul(
                    Box::new(Var("n".to_string())),
                    Box::new(FuncCall(
                        "factorial".to_string(),
                        vec![Sub(Box::new(Var("n".to_string())), Box::new(CInt(1)))],
                    )),
                ))))),
            ))),
        });

        match check_stmt(factorial, &env) {
            Ok(ControlType::Continue(new_env)) => {
                assert_eq!(
                    new_env.search_frame("factorial".to_string()),
                    Some(TFunction(Box::new(Some(TInteger)), vec![TInteger])).as_ref()
                );
            }
            _ => assert!(false, "Recursive function definition failed"),
        }
    }

    #[test]
    fn check_function_multiple_return_paths() {
        let env: Environment<Type> = Environment::new();

        // Função com múltiplos caminhos de retorno
        let func = FuncDef(Function {
            name: "max".to_string(),
            kind: Some(TInteger),
            params: Some(vec![
                ("a".to_string(), TInteger),
                ("b".to_string(), TInteger),
            ]),
            body: Some(Box::new(IfThenElse(
                Box::new(GT(
                    Box::new(Var("a".to_string())),
                    Box::new(Var("b".to_string())),
                )),
                Box::new(Return(Box::new(Var("a".to_string())))),
                Some(Box::new(Return(Box::new(Var("b".to_string()))))),
            ))),
        });

        match check_stmt(func, &env) {
            Ok(ControlType::Continue(_)) => assert!(true),
            _ => assert!(false, "Multiple return paths function failed"),
        }
    }

    #[test]
    fn test_function_wrong_return_type() {
        let env: Environment<Type> = Environment::new();

        let func = FuncDef(Function {
            name: "wrong_return".to_string(),
            kind: Some(TInteger),
            params: None,
            body: Some(Box::new(Return(Box::new(CReal(1.0))))),
        });

        match check_stmt(func, &env) {
            Ok(_) => assert!(false, "Should fail due to wrong return type"),
            Err(msg) => assert_eq!(
                msg,
                "[Type Error] 'wrong_return()' has mismatched types: expected 'TInteger', found 'TReal'."
            ),
        }
    }

    #[test]
    fn test_function_parameter_shadowing() {
        let env: Environment<Type> = Environment::new();

        let func = FuncDef(Function {
            name: "shadow_test".to_string(),
            kind: Some(TInteger),
            params: Some(vec![
                ("x".to_string(), TInteger),
                ("x".to_string(), TInteger), // Mesmo nome de parâmetro
            ]),
            body: Some(Box::new(Return(Box::new(Var("x".to_string()))))),
        });

        match check_stmt(func, &env) {
            Ok(_) => panic!("Should not accept duplicate parameter names"),
            Err(msg) => assert_eq!(msg, "[Parameter Error] Duplicate parameter name 'x'"),
        }
    }
}
