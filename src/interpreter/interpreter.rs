use std::collections::HashSet;

use crate::ir::ast::{Environment, Expression, Function, Name, Statement, TestEnvironment};

type ErrorMessage = (String, Option<Expression>);

#[derive(Clone, Debug, PartialEq)]
pub enum EnvValue {
    Exp(Expression),
    Func(Function),
    TestEnvironment(TestEnvironment<EnvValue>),
}

pub enum ControlFlow {
    Continue(Environment<EnvValue>),
    Return(EnvValue),
}

pub fn eval(exp: Expression, env: &Environment<EnvValue>) -> Result<EnvValue, ErrorMessage> {
    match exp {
        Expression::Add(lhs, rhs) => add(*lhs, *rhs, env),
        Expression::Sub(lhs, rhs) => sub(*lhs, *rhs, env),
        Expression::Mul(lhs, rhs) => mul(*lhs, *rhs, env),
        Expression::Div(lhs, rhs) => div(*lhs, *rhs, env),
        Expression::And(lhs, rhs) => and(*lhs, *rhs, env),
        Expression::Or(lhs, rhs) => or(*lhs, *rhs, env),
        Expression::Not(lhs) => not(*lhs, env),
        Expression::EQ(lhs, rhs) => eq(*lhs, *rhs, env),
        Expression::GT(lhs, rhs) => gt(*lhs, *rhs, env),
        Expression::LT(lhs, rhs) => lt(*lhs, *rhs, env),
        Expression::GTE(lhs, rhs) => gte(*lhs, *rhs, env),
        Expression::LTE(lhs, rhs) => lte(*lhs, *rhs, env),
        Expression::Var(name) => lookup(name, env),
        Expression::COk(e) => eval_ok(*e, env),
        Expression::CErr(e) => eval_err(*e, env),
        Expression::CJust(e) => eval_just(*e, env),
        Expression::Unwrap(e) => eval_unwrap_expression(*e, env),
        Expression::Propagate(e) => eval_propagate_expression(*e, env),
        Expression::IsError(e) => eval_iserror_expression(*e, env),
        Expression::IsNothing(e) => eval_isnothing_expression(*e, env),
        Expression::FuncCall(name, args) => call(name, args, env),
        Expression::ReadFile(file_path_exp) => read_file(*file_path_exp, env),
        Expression::ReadString => read_string(env),
        Expression::ReadInt => read_int(env),
        Expression::ReadFloat => read_float(env),

        _ if is_constant(exp.clone()) => Ok(EnvValue::Exp(exp)),
        _ => Err((String::from("Not implemented yet."), None)),
    }
}

pub fn run(stmt: Statement, env: &Environment<EnvValue>) -> Result<ControlFlow, String> {
    match execute(stmt, env) {
        Ok(e) => Ok(e),
        Err((s, _)) => Err(s),
    }
}

fn execute(stmt: Statement, env: &Environment<EnvValue>) -> Result<ControlFlow, ErrorMessage> {
    let mut new_env = env.clone();

    let result = match stmt {
        Statement::Assignment(name, exp, _) => {
            let value = eval(*exp, &new_env)?;
            new_env.insert_variable(name, value); // Remove the tuple
            Ok(ControlFlow::Continue(new_env))
        }

        Statement::IfThenElse(cond, stmt_then, stmt_else) => {
            let value = eval(*cond, &new_env)?;

            match value {
                EnvValue::Exp(Expression::CTrue) => match *stmt_then {
                    Statement::Block(stmts) => execute_block(stmts, &new_env),
                    _ => execute(*stmt_then, &new_env),
                },
                EnvValue::Exp(Expression::CFalse) => match stmt_else {
                    Some(else_stmt) => match *else_stmt {
                        Statement::Block(stmts) => execute_block(stmts, &new_env),
                        _ => execute(*else_stmt, &new_env),
                    },
                    None => Ok(ControlFlow::Continue(new_env)),
                },
                _ => Err(("Condition must evaluate to a boolean".to_string(), None)),
            }
        }

        Statement::Block(stmts) => execute_block(stmts, &new_env),

        Statement::While(cond, stmt) => {
            let mut value = eval(*cond.clone(), &new_env)?;

            loop {
                match value {
                    EnvValue::Exp(Expression::CTrue) => match execute(*stmt.clone(), &new_env)? {
                        ControlFlow::Continue(control_env) => {
                            new_env = control_env;
                            value = eval(*cond.clone(), &new_env)?;
                        }
                        ControlFlow::Return(value) => return Ok(ControlFlow::Return(value)),
                    },
                    EnvValue::Exp(Expression::CFalse) => return Ok(ControlFlow::Continue(new_env)),
                    _ => unreachable!(),
                }
            }
        }
        Statement::AssertTrue(cond, error) => {
            let value = eval(*cond, &env)?;
            match value {
                EnvValue::Exp(Expression::CTrue) => Ok(ControlFlow::Continue(env.clone())),
                EnvValue::Exp(Expression::CFalse) => Err((error, None)),
                _ => Err((String::from("expecting a boolean value."), None)),
            }
        }

        Statement::AssertFalse(cond, error) => {
            let value = eval(*cond, &env)?;
            match value {
                EnvValue::Exp(Expression::CFalse) => Ok(ControlFlow::Continue(env.clone())),
                EnvValue::Exp(Expression::CTrue) => Err((error, None)),
                _ => Err((String::from("expecting a boolean value."), None)),
            }
        }

        Statement::AssertEQ(value1, value2, error) => {
            match execute(
                Statement::AssertTrue(
                    Box::new(match eq(*value1, *value2, &env)? {
                        EnvValue::Exp(Expression::CTrue) => Expression::CTrue,
                        EnvValue::Exp(Expression::CFalse) => Expression::CFalse,
                        _ => return Err((String::from(""), None)),
                    }),
                    error,
                ),
                env,
            ) {
                Ok(ControlFlow::Continue(new_env)) => Ok(ControlFlow::Continue(new_env)),
                Err(err) => Err(err),
                _ => Err((String::from("arguments are not of the same type"), None)),
            }
        }

        Statement::AssertNEQ(value1, value2, error) => {
            match execute(
                Statement::AssertFalse(
                    Box::new(match eq(*value1, *value2, &env)? {
                        EnvValue::Exp(Expression::CTrue) => Expression::CTrue,
                        EnvValue::Exp(Expression::CFalse) => Expression::CFalse,
                        _ => return Err((String::from(""), None)),
                    }),
                    error,
                ),
                env,
            ) {
                Ok(ControlFlow::Continue(new_env)) => Ok(ControlFlow::Continue(new_env)),
                Err(err) => Err(err),
                _ => Err((String::from("arguments are not of the same type"), None)),
            }
        }

        Statement::AssertFails(error) => Err((error, None)),

        Statement::TestDef(mut test) => {
            test.body = Some(Box::new(Statement::Sequence(
                test.body.unwrap(),
                Box::new(Statement::Return(Box::new(Expression::CVoid))),
            )));

            new_env.insert_test(test.name.clone(), test);
            Ok(ControlFlow::Continue(new_env))
        }

        Statement::ModTestDef(name, stmt) => {
            let mut mod_test: TestEnvironment<EnvValue> = TestEnvironment::new();

            let new_mod_test_env;

            mod_test.env = new_env.clone();

            match execute(*stmt, &mod_test.env) {
                Ok(ControlFlow::Continue(new_env)) => new_mod_test_env = new_env,
                Ok(ControlFlow::Return(value)) => return Ok(ControlFlow::Return(value)),
                Err(e) => return Err(e),
            }

            mod_test.env = new_mod_test_env;

            new_env.insert_variable(name, EnvValue::TestEnvironment(mod_test));

            Ok(ControlFlow::Continue(new_env))
        }
        Statement::Sequence(s1, s2) => match execute(*s1, &new_env)? {
            ControlFlow::Continue(control_env) => {
                new_env = control_env;
                execute(*s2, &new_env)
            }
            ControlFlow::Return(value) => return Ok(ControlFlow::Return(value)),
        },
        Statement::FuncDef(func) => {
            new_env.insert_variable(func.name.clone(), EnvValue::Func(func.clone()));
            Ok(ControlFlow::Continue(new_env))
        }

        Statement::Return(exp) => {
            let exp_value = eval(*exp, &new_env)?;
            Ok(ControlFlow::Return(exp_value))
        }

        Statement::WriteToFile(file_path_exp, content_exp) => {
            let file_path_value = eval(*file_path_exp, &new_env)?;
            let content_value = eval(*content_exp, &new_env)?;

            if let (EnvValue::Exp(Expression::CString(file_path)), EnvValue::Exp(Expression::CString(content))) = (file_path_value, content_value) {
                std::fs::write(file_path, content).map_err(|e| (e.to_string(), None))?;
                Ok(ControlFlow::Continue(new_env))
            } else {
                Err((String::from("write_to_file expects two string arguments"), None))
            }
        }

        Statement::Print(exp) => {
            let value = eval(*exp, &new_env)?;

            match value {
                EnvValue::Exp(Expression::CInt(i)) => println!("{}", i),
                EnvValue::Exp(Expression::CReal(r)) => println!("{}", r),
                EnvValue::Exp(Expression::CString(s)) => println!("{}", s),
                EnvValue::Exp(Expression::CTrue) => println!("true"),
                EnvValue::Exp(Expression::CFalse) => println!("false"),
                _ => return Err((String::from("Cannot print this type of value"), None)),
            }

            Ok(ControlFlow::Continue(new_env))
        }
        _ => Err((String::from("not implemented yet"), None)),
    };

    match result {
        Ok(v) => Ok(v),
        Err((s, opt)) => {
            if s != "Propagate".to_string() {
                return Err((s, None));
            } else {
                return propagate_error(opt.unwrap(), env);
            }
        }
    }
}

//helper function for executing blocks
fn execute_block(
    stmts: Vec<Statement>,
    env: &Environment<EnvValue>,
) -> Result<ControlFlow, ErrorMessage> {
    let mut current_env = env.clone();

    for stmt in stmts {
        match execute(stmt, &current_env)? {
            ControlFlow::Continue(new_env) => current_env = new_env,
            ControlFlow::Return(value) => return Ok(ControlFlow::Return(value)),
        }
    }
    Ok(ControlFlow::Continue(current_env))
}

fn call(
    name: Name,
    args: Vec<Expression>,
    env: &Environment<EnvValue>,
) -> Result<EnvValue, ErrorMessage> {
    // Use search_frame instead of get
    match env.search_frame(name.clone()) {
        Some(EnvValue::Func(func)) => {
            let mut new_env = Environment::new();

            // Copy global functions
            let mut curr_scope = env.scope_key();
            loop {
                let frame = env.get_frame(curr_scope.clone());
                for (name, value) in &frame.variables {
                    if let EnvValue::Func(_) = value {
                        new_env.insert_variable(name.clone(), value.clone());
                    }
                }
                match &frame.parent_key {
                    Some(parent) => curr_scope = parent.clone(),
                    None => break,
                }
            }

            // Bind arguments
            if let Some(params) = &func.params {
                for (param, arg) in params.iter().zip(args) {
                    let arg_value = eval(arg, env)?;
                    new_env.insert_variable(param.0.clone(), arg_value);
                }
            }

            // Execute function
            match execute(*func.body.as_ref().unwrap().clone(), &new_env)? {
                ControlFlow::Return(value) => Ok(value),
                ControlFlow::Continue(_) => {
                    Err(("Function did not return a value".to_string(), None))
                }
            }
        }
        _ => Err((format!("Function {} not found", name), None)),
    }
}

/* Error propagation functions:
    -> extract_error_value
    -> propagate_error
*/
fn extract_error_value(
    exp: Expression,
    env: &Environment<EnvValue>,
) -> Result<String, ErrorMessage> {
    // Gets expression and returns the value inside (works with constants and Error types)
    match exp {
        Expression::COk(e) => extract_error_value(*e, env),
        Expression::CErr(e) => extract_error_value(*e, env),
        Expression::CJust(e) => extract_error_value(*e, env),
        Expression::CTrue => Ok("True".to_string()),
        Expression::CFalse => Ok("False".to_string()),
        Expression::CInt(value) => Ok(value.to_string()),
        Expression::CReal(value) => Ok(value.to_string()),
        Expression::CString(value) => Ok(value.to_string()),
        Expression::CNothing => Ok("Nothing".to_string()),
        _ => Err((String::from("Nothing to extract from."), None)),
    }
}

fn propagate_error(
    exp: Expression,
    env: &Environment<EnvValue>,
) -> Result<ControlFlow, ErrorMessage> {
    // Checks error value and propagates it (terminates code if on highest level function)
    if env.scope_key().1 == 0 {
        match eval(exp, &env) {
            Ok(EnvValue::Exp(new_value)) => match extract_error_value(new_value, &env) {
                Ok(s) => Err((
                    String::from(format!("Program terminated with errors: {}", s)),
                    None,
                )),
                _ => Err(("Program terminated with errors".to_string(), None)),
            },
            _ => Err((
                "Program panicked and trying to terminate with errors".to_string(),
                None,
            )),
        }
    } else {
        return Ok(ControlFlow::Return(EnvValue::Exp(Expression::CErr(
            Box::new(exp),
        ))));
    }
}
fn read_file(file_path_exp: Expression, env: &Environment<EnvValue>) -> Result<EnvValue, ErrorMessage> {
    let file_path_value = eval(file_path_exp, env)?;
    if let EnvValue::Exp(Expression::CString(file_path)) = file_path_value {
        let content = std::fs::read_to_string(file_path).map_err(|e| (e.to_string(), None))?;
        Ok(EnvValue::Exp(Expression::CString(content)))
    } else {
        Err((String::from("read_file expects a string as the file path"), None))
    }
}


// IO helper functions
fn read_string(_: &Environment<EnvValue>) -> Result<EnvValue, ErrorMessage> {
    let mut input = String::new();
    std::io::stdin().read_line(&mut input).map_err(|e| (e.to_string(), None))?;

    let input = input.trim().to_string();
    Ok(EnvValue::Exp(Expression::CString(input)))
}

fn read_int(_: &Environment<EnvValue>) -> Result<EnvValue, ErrorMessage> {
    let mut input = String::new();
    std::io::stdin().read_line(&mut input).map_err(|e| (e.to_string(), None))?;

    let input = input.trim().parse::<i32>().map_err(|e| (e.to_string(), None))?;
    Ok(EnvValue::Exp(Expression::CInt(input)))
}

fn read_float(_: &Environment<EnvValue>) -> Result<EnvValue, ErrorMessage> {
    let mut input = String::new();
    std::io::stdin().read_line(&mut input).map_err(|e| (e.to_string(), None))?;

    let input = input.trim().parse::<f64>().map_err(|e| (e.to_string(), None))?;
    Ok(EnvValue::Exp(Expression::CReal(input)))
}

fn execute_tests(
    tests_set: Vec<(String, Option<String>)>,
    env: &Environment<EnvValue>,
) -> Result<HashSet<(String, String, Option<String>)>, String> {
    let mut results = HashSet::new();
    let cur_scope = env.scope_key();
    let frame: crate::ir::ast::Frame<EnvValue> = env.get_frame(cur_scope.clone()).clone();

    for (mod_test, test) in tests_set {
        match frame.variables.get(&mod_test) {
            Some(EnvValue::TestEnvironment(test_module)) => {
                let mut test_env = test_module.env.clone();
                let mod_test_scope = test_env.scope_key();
                let test_frame = test_env.get_frame(mod_test_scope);

                if test != None {
                    test_env = match run(
                        Statement::FuncDef(
                            match test_frame.clone().tests.get(&test.clone().unwrap()) {
                                Some(real_test) => real_test.clone(),
                                None => {
                                    return Err(format!(
                                        "{teste} is not a test",
                                        teste = &test.clone().unwrap()
                                    ))
                                }
                            },
                        ),
                        &test_env,
                    ) {
                        Ok(ControlFlow::Continue(new_env)) => new_env,
                        Err(e) => return Err(e),
                        Ok(ControlFlow::Return(_)) => return Ok(results),
                    };

                    let result = match eval(
                        Expression::FuncCall(test.clone().unwrap(), Vec::<Expression>::new()),
                        &test_env,
                    ) {
                        Ok(_) => ("Passou".to_string(), None),
                        Err((e, _)) => ("Falhou".to_string(), Some(format!("Erro: {}", e))),
                    };

                    results.insert((
                        format!(
                            "{modulo}::{teste}",
                            teste = test.clone().unwrap(),
                            modulo = mod_test.clone()
                        ),
                        result.0,
                        result.1,
                    ));
                    continue;
                }

                for (test, real_test) in test_frame.clone().tests.into_iter() {
                    test_env = match run(Statement::FuncDef(real_test), &test_env) {
                        Ok(ControlFlow::Continue(new_env)) => new_env,
                        Err(e) => return Err(e),
                        Ok(ControlFlow::Return(_)) => return Ok(results),
                    };

                    let result = match eval(
                        Expression::FuncCall(test.clone(), Vec::<Expression>::new()),
                        &test_env,
                    ) {
                        Ok(_) => ("Passou".to_string(), None),
                        Err((e, _)) => ("Falhou".to_string(), Some(format!("Erro: {}", e))),
                    };

                    results.insert((
                        format!(
                            "{modulo}::{teste}",
                            teste = test.clone(),
                            modulo = mod_test.clone()
                        ),
                        result.0,
                        result.1,
                    ));
                }
            }
            _ => {
                return Err(format!(
                    "{modulo} is not a ModTest",
                    modulo = mod_test.clone()
                ))
            }
        }
    }
    Ok(results)
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

fn lookup(name: String, env: &Environment<EnvValue>) -> Result<EnvValue, ErrorMessage> {
    let mut curr_scope = env.scope_key();

    loop {
        let frame = env.get_frame(curr_scope.clone());

        match frame.variables.get(&name) {
            Some(value) => return Ok(value.clone()),
            None => curr_scope = frame.parent_key.clone().unwrap(),
        }
    }
}

/* Arithmetic Operations */
fn eval_binary_arith_op<F>(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<EnvValue>,
    op: F,
    error_msg: &str,
) -> Result<EnvValue, ErrorMessage>
where
    F: Fn(f64, f64) -> f64,
{
    let v1 = eval(lhs, env)?;
    let v2 = eval(rhs, env)?;
    //// checar aqui se o status de erro é vdd, se for, retornar o valor de erro "Ok(EnvValue::Exp(Cerr q tem no env))"   --> fzr teste
    match (v1, v2) {
        (EnvValue::Exp(Expression::CInt(v1)), EnvValue::Exp(Expression::CInt(v2))) => Ok(
            EnvValue::Exp(Expression::CInt(op(v1 as f64, v2 as f64) as i32)),
        ),
        (EnvValue::Exp(Expression::CInt(v1)), EnvValue::Exp(Expression::CReal(v2))) => {
            Ok(EnvValue::Exp(Expression::CReal(op(v1 as f64, v2))))
        }
        (EnvValue::Exp(Expression::CReal(v1)), EnvValue::Exp(Expression::CInt(v2))) => {
            Ok(EnvValue::Exp(Expression::CReal(op(v1, v2 as f64))))
        }
        (EnvValue::Exp(Expression::CReal(v1)), EnvValue::Exp(Expression::CReal(v2))) => {
            Ok(EnvValue::Exp(Expression::CReal(op(v1, v2))))
        }
        _ => Err((error_msg.to_string(), None)),
    }
}

fn add(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<EnvValue>,
) -> Result<EnvValue, ErrorMessage> {
    eval_binary_arith_op(
        lhs,
        rhs,
        env,
        |a, b| a + b,
        "addition '(+)' is only defined for numbers (integers and real).",
    )
}

fn sub(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<EnvValue>,
) -> Result<EnvValue, ErrorMessage> {
    eval_binary_arith_op(
        lhs,
        rhs,
        env,
        |a, b| a - b,
        "subtraction '(-)' is only defined for numbers (integers and real).",
    )
}

fn mul(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<EnvValue>,
) -> Result<EnvValue, ErrorMessage> {
    eval_binary_arith_op(
        lhs,
        rhs,
        env,
        |a, b| a * b,
        "multiplication '(*)' is only defined for numbers (integers and real).",
    )
}

fn div(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<EnvValue>,
) -> Result<EnvValue, ErrorMessage> {
    eval_binary_arith_op(
        lhs,
        rhs,
        env,
        |a, b| a / b,
        "division '(/)' is only defined for numbers (integers and real).",
    )
}

/* Boolean Expressions */
fn eval_binary_boolean_op<F>(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<EnvValue>,
    op: F,
    error_msg: &str,
) -> Result<EnvValue, ErrorMessage>
where
    F: Fn(bool, bool) -> Expression,
{
    let v1 = eval(lhs, env)?;
    let v2 = eval(rhs, env)?;
    //// checar aqui se o status de erro é vdd, se for, retornar o valor de erro "Ok(EnvValue::Exp(Cerr q tem no env))"   --> fzr teste
    match (v1, v2) {
        (EnvValue::Exp(Expression::CTrue), EnvValue::Exp(Expression::CTrue)) => {
            Ok(EnvValue::Exp(op(true, true)))
        }
        (EnvValue::Exp(Expression::CTrue), EnvValue::Exp(Expression::CFalse)) => {
            Ok(EnvValue::Exp(op(true, false)))
        }
        (EnvValue::Exp(Expression::CFalse), EnvValue::Exp(Expression::CTrue)) => {
            Ok(EnvValue::Exp(op(false, true)))
        }
        (EnvValue::Exp(Expression::CFalse), EnvValue::Exp(Expression::CFalse)) => {
            Ok(EnvValue::Exp(op(false, false)))
        }
        _ => Err((error_msg.to_string(), None)),
    }
}

fn and(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<EnvValue>,
) -> Result<EnvValue, ErrorMessage> {
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

fn or(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<EnvValue>,
) -> Result<EnvValue, ErrorMessage> {
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

fn not(lhs: Expression, env: &Environment<EnvValue>) -> Result<EnvValue, ErrorMessage> {
    let v = eval(lhs, env)?;
    match v {
        EnvValue::Exp(Expression::CTrue) => Ok(EnvValue::Exp(Expression::CFalse)),
        EnvValue::Exp(Expression::CFalse) => Ok(EnvValue::Exp(Expression::CTrue)),
        _ => Err((String::from("'not' is only defined for booleans."), None)),
    }
}

/* Relational Operations */
fn eval_binary_rel_op<F>(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<EnvValue>,
    op: F,
    error_msg: &str,
) -> Result<EnvValue, ErrorMessage>
where
    F: Fn(f64, f64) -> Expression,
{
    let v1 = eval(lhs, env)?;
    let v2 = eval(rhs, env)?;
    //// checar aqui se o status de erro é vdd, se for, retornar o valor de erro "Ok(EnvValue::Exp(Cerr q tem no env))"   --> fzr teste
    match (v1, v2) {
        (EnvValue::Exp(Expression::CInt(v1)), EnvValue::Exp(Expression::CInt(v2))) => {
            Ok(EnvValue::Exp(op(v1 as f64, v2 as f64)))
        }
        (EnvValue::Exp(Expression::CInt(v1)), EnvValue::Exp(Expression::CReal(v2))) => {
            Ok(EnvValue::Exp(op(v1 as f64, v2)))
        }
        (EnvValue::Exp(Expression::CReal(v1)), EnvValue::Exp(Expression::CInt(v2))) => {
            Ok(EnvValue::Exp(op(v1, v2 as f64)))
        }
        (EnvValue::Exp(Expression::CReal(v1)), EnvValue::Exp(Expression::CReal(v2))) => {
            Ok(EnvValue::Exp(op(v1, v2)))
        }
        _ => Err((error_msg.to_string(), None)),
    }
}

fn eq(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<EnvValue>,
) -> Result<EnvValue, ErrorMessage> {
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
        "(==) is only defined for numbers (integers and real).",
    )
}

fn gt(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<EnvValue>,
) -> Result<EnvValue, ErrorMessage> {
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
        "(>) is only defined for numbers (integers and real).",
    )
}

fn lt(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<EnvValue>,
) -> Result<EnvValue, ErrorMessage> {
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
        "(<) is only defined for numbers (integers and real).",
    )
}

fn gte(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<EnvValue>,
) -> Result<EnvValue, ErrorMessage> {
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
        "(>=) is only defined for numbers (integers and real).",
    )
}

fn lte(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<EnvValue>,
) -> Result<EnvValue, ErrorMessage> {
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
        "(<=) is only defined for numbers (integers and real).",
    )
}

fn eval_unwrap_expression(
    exp: Expression,
    env: &Environment<EnvValue>,
) -> Result<EnvValue, ErrorMessage> {
    ////QUATRO/ FAz uteste também
    let v = eval(exp, env)?;
    match v {
        EnvValue::Exp(Expression::CJust(e)) => Ok(EnvValue::Exp(*e)),
        EnvValue::Exp(Expression::COk(e)) => Ok(EnvValue::Exp(*e)),
        _ => Err((String::from("Program panicked trying to unwrap."), None)),
    }
}

fn eval_propagate_expression(
    exp: Expression,
    env: &Environment<EnvValue>,
) -> Result<EnvValue, ErrorMessage> {
    ////QUATRO Fazer teste com recursão pls :D
    let v = eval(exp, env)?;
    //let mut *new_env = env.clone();
    match v {
        EnvValue::Exp(Expression::CJust(e)) => Ok(EnvValue::Exp(*e)),
        EnvValue::Exp(Expression::COk(e)) => Ok(EnvValue::Exp(*e)),
        EnvValue::Exp(Expression::CErr(e)) => Err(("Propagate".to_string(), Some(*e))),
        EnvValue::Exp(Expression::CNothing) => Err((
            "Propagate".to_string(),
            Some(Expression::CString("Couldn't unwrap Nothing".to_string())),
        )),
        _ => Err((String::from("'propagate' is expects a Just or Ok."), None)),
    }
}

fn eval_isnothing_expression(
    exp: Expression,
    env: &Environment<EnvValue>,
) -> Result<EnvValue, ErrorMessage> {
    let v = eval(exp, env)?;
    match v {
        EnvValue::Exp(Expression::CNothing) => Ok(EnvValue::Exp(Expression::CTrue)),
        _ => Ok(EnvValue::Exp(Expression::CFalse)),
        //EnvValue::Exp(Expression::CJust(_)) => Ok(EnvValue::Exp(Expression::CFalse)),
        //_ => Err("Expression not recognized.".to_string()),
    }
}

fn eval_iserror_expression(
    exp: Expression,
    env: &Environment<EnvValue>,
) -> Result<EnvValue, ErrorMessage> {
    let v = eval(exp, env)?;
    match v {
        EnvValue::Exp(Expression::CErr(_)) => Ok(EnvValue::Exp(Expression::CTrue)),
        _ => Ok(EnvValue::Exp(Expression::CFalse)),
        //EnvValue::Exp(Expression::COk(_)) => Ok(EnvValue::Exp(Expression::CFalse)),
        //_ => Err(String::from("'is_error' is only defined for Ok and Err.")),
    }
}

fn eval_just(exp: Expression, env: &Environment<EnvValue>) -> Result<EnvValue, ErrorMessage> {
    let v = eval(exp, env)?;
    match v {
        EnvValue::Exp(e) => Ok(EnvValue::Exp(Expression::CJust(Box::new(e)))),
        _ => Err(("Expression not recognized.".to_string(), None)),
    }
}

fn eval_ok(exp: Expression, env: &Environment<EnvValue>) -> Result<EnvValue, ErrorMessage> {
    let v = eval(exp, env)?;
    match v {
        EnvValue::Exp(e) => Ok(EnvValue::Exp(Expression::COk(Box::new(e)))),
        _ => Err(("Expression not recognized.".to_string(), None)),
    }
}

fn eval_err(exp: Expression, env: &Environment<EnvValue>) -> Result<EnvValue, ErrorMessage> {
    let v = eval(exp, env)?;
    match v {
        EnvValue::Exp(e) => Ok(EnvValue::Exp(Expression::CErr(Box::new(e)))),
        _ => Err(("Expression not recognized.".to_string(), None)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::ast::Expression::*;
    use crate::ir::ast::Function;
    use crate::ir::ast::Statement::*;
    use std::collections::HashMap;
    //use crate::ir::ast::Type;
    use crate::ir::ast::Type::*;
    use approx::relative_eq;

    #[test]
    fn eval_constant() {
        let env: Environment<EnvValue> = Environment::new();

        let c10 = CInt(10);
        let c20 = CInt(20);

        assert_eq!(eval(c10, &env), Ok(EnvValue::Exp(CInt(10))));
        assert_eq!(eval(c20, &env), Ok(EnvValue::Exp(CInt(20))));
    }

    #[test]
    fn eval_unwrap_result_ok() {
        let env: Environment<EnvValue> = Environment::new();
        let c10 = CInt(10);
        let ok = COk(Box::new(c10));
        let u = Unwrap(Box::new(ok));

        assert_eq!(eval(u, &env), Ok(EnvValue::Exp(CInt(10))));
    }

    #[test]
    fn eval_unwrap_result_err() {
        let env: Environment<EnvValue> = Environment::new();
        let c1 = CInt(1);
        let err = CErr(Box::new(c1));
        let u = Unwrap(Box::new(err));

        match eval(u, &env) {
            Err(_) => assert!(true),
            _ => assert!(false, "The program was suposed to terminate"),
        }
    }

    #[test]
    fn eval_unwrap_just() {
        let env: Environment<EnvValue> = Environment::new();
        let c5 = CInt(5);
        let maybe = CJust(Box::new(c5));
        let u = Unwrap(Box::new(maybe));

        assert_eq!(eval(u, &env), Ok(EnvValue::Exp(CInt(5))));
    }

    #[test]
    fn eval_unwrap_nothing() {
        let env: Environment<EnvValue> = Environment::new();
        let u = Unwrap(Box::new(CNothing));

        match eval(u, &env) {
            Err(_) => assert!(true),
            _ => assert!(false, "The program was suposed to terminate"),
        }
    }

    #[test]
    fn eval_is_error_result_true() {
        let env: Environment<EnvValue> = Environment::new();
        let aux = CInt(2);
        let e = Expression::CErr(Box::new(aux));
        let ie = IsError(Box::new(e));

        assert_eq!(eval(ie, &env), Ok(EnvValue::Exp(CTrue)));
    }

    #[test]
    fn eval_is_error_result_false() {
        let env: Environment<EnvValue> = Environment::new();
        let aux = CInt(2);
        let r = COk(Box::new(aux));
        let ie = IsError(Box::new(r));

        assert_eq!(eval(ie, &env), Ok(EnvValue::Exp(CFalse)));
    }

    #[test]
    fn eval_is_error_result_error() {
        let env: Environment<EnvValue> = Environment::new();
        let aux = CInt(2);
        let ie = IsError(Box::new(aux));

        assert_eq!(eval(ie, &env), Ok(EnvValue::Exp(CFalse)));
        /*
        assert_eq!(
            eval(ie, &env),
            Err(String::from("'is_error' is only defined for Ok and Err."))
        ); */
    }

    #[test]
    fn eval_is_nothing_with_nothing() {
        let env: Environment<EnvValue> = Environment::new();
        let nothing = CNothing;
        let u = IsNothing(Box::new(nothing));

        assert_eq!(eval(u, &env), Ok(EnvValue::Exp(CTrue)));
    }

    #[test]
    fn eval_is_nothing_with_just() {
        let env: Environment<EnvValue> = Environment::new();
        let c2 = CReal(6.9);
        let just = CJust(Box::new(c2));
        let u = IsNothing(Box::new(just));

        assert_eq!(eval(u, &env), Ok(EnvValue::Exp(CFalse)));
    }

    #[test]
    fn eval_is_nothing_with_int() {
        let env: Environment<EnvValue> = Environment::new();
        let c420 = CInt(420);
        let u = IsNothing(Box::new(c420));

        assert_eq!(eval(u, &env), Ok(EnvValue::Exp(CFalse)));

        //assert_eq!(eval(u, &env), Err("Expression not recognized.".to_string()));
    }

    #[test]
    fn eval_add_expression1() {
        let env: Environment<EnvValue> = Environment::new();

        let c10 = CInt(10);
        let c20 = CInt(20);
        let add1 = Add(Box::new(c10), Box::new(c20));

        assert_eq!(eval(add1, &env), Ok(EnvValue::Exp(CInt(30))));
    }

    #[test]
    fn eval_add_expression2() {
        let env: Environment<EnvValue> = Environment::new();

        let c10 = CInt(10);
        let c20 = CInt(20);
        let c30 = CInt(30);
        let add1 = Add(Box::new(c10), Box::new(c20));
        let add2 = Add(Box::new(add1), Box::new(c30));

        assert_eq!(eval(add2, &env), Ok(EnvValue::Exp(CInt(60))));
    }

    #[test]
    fn eval_add_expression3() {
        let env: Environment<EnvValue> = Environment::new();

        let c10 = CInt(10);
        let c20 = CReal(20.5);
        let add1 = Add(Box::new(c10), Box::new(c20));

        assert_eq!(eval(add1, &env), Ok(EnvValue::Exp(CReal(30.5))));
    }

    #[test]
    fn eval_sub_expression1() {
        let env: Environment<EnvValue> = Environment::new();

        let c10 = CInt(10);
        let c20 = CInt(20);
        let sub1 = Sub(Box::new(c20), Box::new(c10));

        assert_eq!(eval(sub1, &env), Ok(EnvValue::Exp(CInt(10))));
    }

    #[test]
    fn eval_sub_expression2() {
        let env: Environment<EnvValue> = Environment::new();

        let c100 = CInt(100);
        let c200 = CInt(300);
        let sub1 = Sub(Box::new(c200), Box::new(c100));

        assert_eq!(eval(sub1, &env), Ok(EnvValue::Exp(CInt(200))));
    }

    #[test]
    fn eval_sub_expression3() {
        let env: Environment<EnvValue> = Environment::new();

        let c100 = CReal(100.5);
        let c300 = CInt(300);
        let sub1 = Sub(Box::new(c300), Box::new(c100));

        assert_eq!(eval(sub1, &env), Ok(EnvValue::Exp(CReal(199.5))));
    }

    #[test]
    fn eval_mul_expression1() {
        let env: Environment<EnvValue> = Environment::new();

        let c10 = CInt(10);
        let c20 = CInt(20);
        let mul1 = Mul(Box::new(c10), Box::new(c20));

        assert_eq!(eval(mul1, &env), Ok(EnvValue::Exp(CInt(200))));
    }

    #[test]
    fn eval_mul_expression2() {
        let env: Environment<EnvValue> = Environment::new();

        let c10 = CReal(10.5);
        let c20 = CInt(20);
        let mul1 = Mul(Box::new(c10), Box::new(c20));

        assert_eq!(eval(mul1, &env), Ok(EnvValue::Exp(CReal(210.0))));
    }

    #[test]
    fn eval_div_expression1() {
        let env: Environment<EnvValue> = Environment::new();

        let c10 = CInt(10);
        let c20 = CInt(20);
        let div1 = Div(Box::new(c20), Box::new(c10));

        assert_eq!(eval(div1, &env), Ok(EnvValue::Exp(CInt(2))));
    }

    #[test]
    fn eval_div_expression2() {
        let env: Environment<EnvValue> = Environment::new();

        let c10 = CInt(10);
        let c3 = CInt(3);
        let div1 = Div(Box::new(c10), Box::new(c3));

        assert_eq!(eval(div1, &env), Ok(EnvValue::Exp(CInt(3))));
    }

    #[test]
    fn eval_div_expression3() {
        let env: Environment<EnvValue> = Environment::new();

        let c3 = CInt(3);
        let c21 = CInt(21);
        let div1 = Div(Box::new(c21), Box::new(c3));

        assert_eq!(eval(div1, &env), Ok(EnvValue::Exp(CInt(7))));
    }
    #[test]
    fn eval_div_expression4() {
        let env: Environment<EnvValue> = Environment::new();

        let c10 = CInt(10);
        let c3 = CReal(3.0);
        let div1 = Div(Box::new(c10), Box::new(c3));
        let res = eval(div1, &env);

        match res {
            Ok(EnvValue::Exp(Expression::CReal(v))) => {
                assert!(relative_eq!(v, 3.3333333333333335, epsilon = f64::EPSILON))
            }
            Err(msg) => assert!(false, "{:?}", msg),
            _ => assert!(false, "Not expected."),
        }
    }

    #[test]
    fn eval_variable() {
        let mut env = Environment::new();
        env.insert_variable("x".to_string(), EnvValue::Exp(CInt(10)));
        env.insert_variable("y".to_string(), EnvValue::Exp(CInt(20)));

        let v1 = Var(String::from("x"));
        let v2 = Var(String::from("y"));

        assert_eq!(eval(v1, &env), Ok(EnvValue::Exp(CInt(10))));
        assert_eq!(eval(v2, &env), Ok(EnvValue::Exp(CInt(20))));
    }

    #[test]
    fn eval_expression_with_variables() {
        let mut env = Environment::new();
        env.insert_variable("a".to_string(), EnvValue::Exp(CInt(5)));
        env.insert_variable("b".to_string(), EnvValue::Exp(CInt(3)));

        let expr = Mul(
            Box::new(Var(String::from("a"))),
            Box::new(Add(Box::new(Var(String::from("b"))), Box::new(CInt(2)))),
        );

        assert_eq!(eval(expr, &env), Ok(EnvValue::Exp(CInt(25))));
    }

    #[test]
    fn eval_nested_expressions() {
        let env: Environment<EnvValue> = Environment::new();

        let expr = Add(
            Box::new(Mul(Box::new(CInt(2)), Box::new(CInt(3)))),
            Box::new(Sub(Box::new(CInt(10)), Box::new(CInt(4)))),
        );

        assert_eq!(eval(expr, &env), Ok(EnvValue::Exp(CInt(12))));
    }

    #[test]
    fn execute_assignment() {
        let env: Environment<EnvValue> = Environment::new();

        let assign_stmt = Assignment(String::from("x"), Box::new(CInt(42)), Some(TInteger));

        match run(assign_stmt, &env) {
            Ok(ControlFlow::Continue(new_env)) => assert_eq!(
                new_env.search_frame("x".to_string()),
                Some(&EnvValue::Exp(CInt(42)))
            ),
            Ok(ControlFlow::Return(_)) => assert!(false),
            Err(s) => assert!(false, "{:?}", s),
        }
    }

    #[test]
    fn eval_summation() {
        /*
         * (a test case for the following program)
         *
         * > x: TInteger = 10
         * > y: TInteger = 0
         * > while x >= 0:
         * >   y = y + x
         * >   x = x - 1
         *
         * After executing this program, 'x' must be zero and
         * 'y' must be 55.
         */

        let env: Environment<EnvValue> = Environment::new();

        let a1 = Assignment(String::from("x"), Box::new(CInt(10)), Some(TInteger));
        let a2 = Assignment(String::from("y"), Box::new(CInt(0)), Some(TInteger));
        let a3 = Assignment(
            String::from("y"),
            Box::new(Add(
                Box::new(Var(String::from("y"))),
                Box::new(Var(String::from("x"))),
            )),
            None,
        );
        let a4 = Assignment(
            String::from("x"),
            Box::new(Sub(Box::new(Var(String::from("x"))), Box::new(CInt(1)))),
            None,
        );

        let seq1 = Sequence(Box::new(a3), Box::new(a4));

        let while_statement = While(
            Box::new(GT(Box::new(Var(String::from("x"))), Box::new(CInt(0)))),
            Box::new(seq1),
        );

        let seq2 = Sequence(Box::new(a2), Box::new(while_statement));
        let program = Sequence(Box::new(a1), Box::new(seq2));

        match execute(program, &env) {
            Ok(ControlFlow::Continue(new_env)) => {
                assert_eq!(
                    new_env.search_frame("y".to_string()),
                    Some(&EnvValue::Exp(CInt(55)))
                );
                assert_eq!(
                    new_env.search_frame("x".to_string()),
                    Some(&EnvValue::Exp(CInt(0)))
                );
            }
            Ok(ControlFlow::Return(_)) => assert!(false),
            Err(s) => assert!(false, "{:?}", s),
        }
    }

    #[test]
    fn eval_assert_true() {
        //let lb= Box::new (CTrue);
        //let rb= Box::new(CFalse);
        let n1 = Box::new(CInt(4));
        let n2: Box<Expression> = Box::new(CReal(0.54));
        let armt = Box::new(EQ(n1, n2));
        let str_erro: String = String::from("It didn't go");
        let env: Environment<EnvValue> = Environment::new();
        let func_teste = AssertTrue(armt, str_erro.clone());
        match run(func_teste, &env) {
            Ok(_) => {}
            Err(s) => assert_eq!(s, str_erro),
        }
    }

    #[test]
    fn eval_assert_false() {
        let verdade = Box::new(CFalse);
        let str_erro = String::from("Nao foi");
        let func_teste = AssertFalse(verdade, str_erro);
        let env: Environment<EnvValue> = Environment::new();
        match run(func_teste, &env) {
            Ok(_) => {}
            Err(s) => assert!(false, "{}", s),
        }
    }
    #[test]
    fn eval_assert_eq() {
        let n1 = Box::new(CReal(4.0));
        let n2 = Box::new(CInt(4));
        let str_erro: String = String::from("Different values");
        let func_teste = AssertEQ(n1, n2, str_erro);
        let env: Environment<EnvValue> = Environment::new();

        match run(func_teste, &env) {
            Ok(_) => {}
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn eval_fail_assert_eq() {
        let n1 = Box::new(CReal(4.5));
        let n2 = Box::new(CInt(4));
        let str_erro: String = String::from("Different values");
        let func_teste = AssertEQ(n1, n2, str_erro.clone());
        let env: Environment<EnvValue> = Environment::new();

        match run(func_teste, &env) {
            Ok(_) => {}
            Err(s) => assert_eq!(s, str_erro),
        }
    }

    #[test]
    fn eval_assert_neq() {
        let n1 = Box::new(CReal(4.0));
        let n2 = Box::new(CInt(3));
        let str_erro: String = String::from("Equal values");
        let func_teste = AssertNEQ(n1, n2, str_erro.clone());
        let env: Environment<EnvValue> = Environment::new();

        match run(func_teste, &env) {
            Ok(_) => {}
            Err(s) => assert_eq!(s, str_erro),
        }
    }
    #[test]
    fn eval_fails() {
        let env: Environment<EnvValue> = Environment::new();
        let error_msg: String = String::from("Test failed.");
        let test_fn = AssertFails(error_msg.clone());

        match run(test_fn, &env) {
            Ok(_) => {}
            Err(s) => assert_eq!(s, error_msg),
        }
    }
    #[test]
    fn eval_simple_if_then_else() {
        /*
         * Test for simple if-then-else statement
         *
         * > x: TInteger = 10
         * > if x > 5:
         * >   y: TInteger = 1
         * > else:
         * >   y: TInteger = 0
         *
         * After executing, 'y' should be 1.
         */

        let env: Environment<EnvValue> = Environment::new();

        let condition = GT(Box::new(Var(String::from("x"))), Box::new(CInt(5)));
        let then_stmt = Assignment(String::from("y"), Box::new(CInt(1)), Some(TInteger));
        let else_stmt = Assignment(String::from("y"), Box::new(CInt(0)), Some(TInteger));

        let if_statement = IfThenElse(
            Box::new(condition),
            Box::new(then_stmt),
            Some(Box::new(else_stmt)),
        );

        let setup_stmt = Assignment(String::from("x"), Box::new(CInt(10)), Some(TInteger));
        let program = Sequence(Box::new(setup_stmt), Box::new(if_statement));

        match run(program, &env) {
            Ok(ControlFlow::Continue(new_env)) => assert_eq!(
                new_env.search_frame("y".to_string()),
                Some(&EnvValue::Exp(CInt(1)))
            ),
            Ok(ControlFlow::Return(_)) => assert!(false),
            Err(s) => assert!(false, "{:?}", s),
        }
    }

    #[test]
    fn eval_if_then_optional_else() {
        /*
         * Test for simple if-then-else statement
         *
         * > x: TInteger = 1
         * > y: TInteger = 0
         * > if x == y:
         * >   y = 1
         * > else:
         * >    y = 2
         * >    if x < 0:
         * >        y = 5
         *
         * After executing, 'y' should be 2.
         */

        let env: Environment<EnvValue> = Environment::new();

        let second_condition = LT(Box::new(Var(String::from("x"))), Box::new(CInt(0)));
        let second_then_stmt = Assignment(String::from("y"), Box::new(CInt(5)), None);

        let second_if_stmt =
            IfThenElse(Box::new(second_condition), Box::new(second_then_stmt), None);

        let else_setup_stmt = Assignment(String::from("y"), Box::new(CInt(2)), None);
        let else_stmt = Sequence(Box::new(else_setup_stmt), Box::new(second_if_stmt));

        let first_condition = EQ(
            Box::new(Var(String::from("x"))),
            Box::new(Var(String::from("y"))),
        );
        let first_then_stmt = Assignment(String::from("y"), Box::new(CInt(1)), None);

        let first_if_stmt = IfThenElse(
            Box::new(first_condition),
            Box::new(first_then_stmt),
            Some(Box::new(else_stmt)),
        );

        let second_assignment = Assignment(String::from("y"), Box::new(CInt(0)), Some(TInteger));
        let setup_stmt = Sequence(Box::new(second_assignment), Box::new(first_if_stmt));

        let first_assignment = Assignment(String::from("x"), Box::new(CInt(1)), Some(TInteger));
        let program = Sequence(Box::new(first_assignment), Box::new(setup_stmt));

        match run(program, &env) {
            Ok(ControlFlow::Continue(new_env)) => assert_eq!(
                new_env.search_frame("y".to_string()),
                Some(&EnvValue::Exp(CInt(2)))
            ),
            Ok(ControlFlow::Return(_)) => assert!(false),
            Err(s) => assert!(false, "{:?}", s),
        }
    }

    // #[test]
    // fn eval_while_loop_decrement() {
    //     /*
    //      * Test for while loop that decrements a variable
    //      *
    //      * > x = 3
    //      * > y = 10
    //      * > while x:
    //      * >   y = y - 1
    //      * >   x = x - 1
    //      *
    //      * After executing, 'y' should be 7 and 'x' should be 0.
    //      */
    //     let env = HashMap::new();

    //     let a1 = Assignment(String::from("x"), Box::new(CInt(3))); -> corrigido parenteses extras.
    //     let a2 = Assignment(String::from("y")), Box:new(CInt(10)));
    //     let a3 = Assignment(
    //         String::from("y")),
    //         Box::new(Sub(
    //             Box::new(Var(String::from("y"))),
    //             Box::new(CInt(1)),
    //         )),
    //     );
    //     let a4 = Assignment(
    //         String::from("x")),
    //         Box::new(Sub(
    //             Box::new(Var(String::from("x"))),
    //             Box::new(CInt(1)),
    //         )),
    //     );

    //     let seq1 = Sequence(Box::new(a3), Box::new(a4));
    //     let while_statement =
    //         While(Box::new(Var(String::from("x"))), Box::new(seq1));
    //     let program = Sequence(
    //         Box::new(a1),
    //         Box::new(Sequence(Box::new(a2), Box::new(while_statement))),
    //     );

    //     match run(&program, env) {
    //         Ok(new_env) => {
    //             assert_eq!(new_env.get("y"), Some(&7));
    //             assert_eq!(new_env.get("x"), Some(&0));
    //         }
    //         Err(s) => assert!(false, "{}", s),
    //     }
    // }

    // #[test]
    // fn eval_nested_if_statements() {
    //     /*
    //      * Test for nested if-then-else statements
    //      *
    //      * > x = 10
    //      * > if x > 5:
    //      * >   if x > 8:
    //      * >     y = 1
    //      * >   else:
    //      * >     y = 2
    //      * > else:
    //      * >   y = 0
    //      *
    //      * After executing, 'y' should be 1.
    //      */
    //     let env = HashMap::new();

    //     let inner_then_stmt =
    //         Assignment(String::from("y")), Box:new(CInt(1)));
    //     let inner_else_stmt =
    //         Assignment(String::from("y")), Box:new(CInt(2)));
    //     let inner_if_statement = Statement::IfThenElse(
    //         Box::new(Var(String::from("x"))),
    //         Box::new(inner_then_stmt),
    //         Box::new(inner_else_stmt),
    //     );

    //     let outer_else_stmt =
    //         Assignment(String::from("y")), Box:new(CInt(0)));
    //     let outer_if_statement = Statement::IfThenElse(
    //         Box::new(Var(String::from("x"))),
    //         Box::new(inner_if_statement),
    //         Box::new(outer_else_stmt),
    //     );

    //     let setup_stmt =
    //         Assignment(String::from("x")), Box:new(CInt(10)));
    //     let program = Sequence(Box::new(setup_stmt), Box::new(outer_if_statement));

    //     match run(&program, env) {
    //         Ok(new_env) => assert_eq!(new_env.get("y"), Some(&1)),
    //         Err(s) => assert!(false, "{}", s),
    //     }
    // }

    #[test]
    fn eval_complex_sequence() {
        /*
         * Sequence with multiple assignments and expressions
         *
         * > x: TInteger = 5
         * > y: TInteger = 0
         * > z: TInteger = 2 * x + 3
         *
         * After executing, 'x' should be 5, 'y' should be 0, and 'z' should be 13.
         */

        let env: Environment<EnvValue> = Environment::new();

        let a1 = Assignment(String::from("x"), Box::new(CInt(5)), Some(TInteger));
        let a2 = Assignment(String::from("y"), Box::new(CInt(0)), Some(TInteger));
        let a3 = Assignment(
            String::from("z"),
            Box::new(Add(
                Box::new(Mul(Box::new(CInt(2)), Box::new(Var(String::from("x"))))),
                Box::new(CInt(3)),
            )),
            Some(TInteger),
        );

        let program = Sequence(Box::new(a1), Box::new(Sequence(Box::new(a2), Box::new(a3))));

        match run(program, &env) {
            Ok(ControlFlow::Continue(new_env)) => {
                assert_eq!(
                    new_env.search_frame("x".to_string()),
                    Some(&EnvValue::Exp(CInt(5)))
                );
                assert_eq!(
                    new_env.search_frame("y".to_string()),
                    Some(&EnvValue::Exp(CInt(0)))
                );
                assert_eq!(
                    new_env.search_frame("z".to_string()),
                    Some(&EnvValue::Exp(CInt(13)))
                );
            }
            Ok(ControlFlow::Return(_)) => assert!(false),
            Err(s) => assert!(false, "{:?}", s),
        }
    }

    #[test]
    fn recursive_func_def_call() {
        /*
         * Test for a recursive function
         *
         * > def fibonacci(n: TInteger) -> TInteger:
         * >    if n < 1:
         * >        return 0
         * >
         * >    if n <= 2:
         * >        return n - 1
         * >
         * >    return fibonacci(n - 1) + fibonacci(n - 2)
         * >
         * > fib: TInteger = fibonacci(10)
         *
         * After executing, 'fib' should be 34.
         */

        let env: Environment<EnvValue> = Environment::new();

        let func = FuncDef(Function {
            name: "fibonacci".to_string(),
            kind: Some(TInteger),
            params: Some(vec![("n".to_string(), TInteger)]),
            body: Some(Box::new(Sequence(
                Box::new(IfThenElse(
                    Box::new(LT(Box::new(Var("n".to_string())), Box::new(CInt(1)))),
                    Box::new(Return(Box::new(CInt(0)))),
                    None,
                )),
                Box::new(Sequence(
                    Box::new(IfThenElse(
                        Box::new(LTE(Box::new(Var("n".to_string())), Box::new(CInt(2)))),
                        Box::new(Return(Box::new(Sub(
                            Box::new(Var("n".to_string())),
                            Box::new(CInt(1)),
                        )))),
                        None,
                    )),
                    Box::new(Return(Box::new(Add(
                        Box::new(FuncCall(
                            "fibonacci".to_string(),
                            vec![Sub(Box::new(Var("n".to_string())), Box::new(CInt(1)))],
                        )),
                        Box::new(FuncCall(
                            "fibonacci".to_string(),
                            vec![Sub(Box::new(Var("n".to_string())), Box::new(CInt(2)))],
                        )),
                    )))),
                )),
            ))),
        });

        let program = Sequence(
            Box::new(func),
            Box::new(Assignment(
                "fib".to_string(),
                Box::new(FuncCall("fibonacci".to_string(), vec![CInt(10)])),
                Some(TInteger),
            )),
        );

        match run(program, &env) {
            Ok(ControlFlow::Continue(new_env)) => assert_eq!(
                new_env.search_frame("fib".to_string()),
                Some(&EnvValue::Exp(CInt(34)))
            ),
            Ok(ControlFlow::Return(_)) => assert!(false),
            Err(s) => assert!(false, "{:?}", s),
        }
    }

    #[test]
    fn eval_mod_test_def() {
        /*
         * Test for modTest definition
         *
         *
         *   def soma1(a, b):
         *       return a+b
         *   def soma_mut(a, b, m):
         *       return(a+b)*m
         *
         *   modTest teste {
         *
         *       deftest test {
         *
         *           assertEQ(soma1(1, 2), soma_mut(1, 2, 3))
         *       }
         *   }
         */

        let env: Environment<EnvValue> = Environment::new();

        let func_soma1 = FuncDef(Function {
            name: "soma1".to_string(),
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

        let func_soma_mut = FuncDef(Function {
            name: "soma_mut".to_string(),
            kind: Some(TInteger),
            params: Some(vec![
                ("a".to_string(), TInteger),
                ("b".to_string(), TInteger),
                ("m".to_string(), TInteger),
            ]),
            body: Some(Box::new(Return(Box::new(Mul(
                Box::new(Add(
                    Box::new(Var("a".to_string())),
                    Box::new(Var("b".to_string())),
                )),
                Box::new(Var("m".to_string())),
            ))))),
        });

        let body_test = Box::new(AssertEQ(
            Box::new(FuncCall("soma1".to_string(), vec![CInt(1), CInt(2)])),
            Box::new(FuncCall(
                "soma_mut".to_string(),
                vec![CInt(1), CInt(2), CInt(3)],
            )),
            "Somas diferentes".to_string(),
        ));

        let body_mod_test = Box::new(TestDef(Function {
            name: "test".to_string(),
            kind: Some(TVoid),
            params: None,
            body: Some(body_test.clone()),
        }));

        let mod_test_def = Box::new(ModTestDef("teste".to_string(), body_mod_test));

        let program = Box::new(Sequence(
            Box::new(func_soma1),
            Box::new(Sequence(Box::new(func_soma_mut), mod_test_def)),
        ));

        let real_hash: HashMap<String, Function> = HashMap::from([(
            "test".to_string(),
            Function {
                name: "test".to_string(),
                kind: Some(TVoid),
                params: None,
                body: Some(Box::new(Sequence(
                    body_test,
                    Box::new(Return(Box::new(CVoid))),
                ))),
            },
        )]);

        match run(*program, &env) {
            Ok(ControlFlow::Continue(new_env)) => {
                let cur_scope = new_env.scope_key().clone();
                let frame = new_env.get_frame(cur_scope).clone();
                match frame.variables.get("teste") {
                    Some(EnvValue::TestEnvironment(mod_test)) => {
                        let cur_scope1 = mod_test.env.scope_key();
                        let frame1 = mod_test.env.get_frame(cur_scope1);

                        assert_eq!(frame1.tests, real_hash);
                    }
                    _ => assert!(false),
                }
            }
            Ok(ControlFlow::Return(_)) => assert!(false),
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn eval_more_than_one_test() {
        /*
         * Test for more than one function inside modTest definition
         *
         *
         *   def soma1(a, b):
         *       return a+b
         *   def soma_mut(a, b, m):
         *       return(a+b)*m
         *   def sub (a,b):
         *      return a-b
         *   def sub_mut (a,b,m):
         *      return (a-b)*m
         *
         *   modTest teste {
         *
         *       deftest test {
         *
         *           assertEQ(soma1(1, 2), soma_mut(1, 2, 3))
         *           assertNEQ(sub(1,2), submut(1,2,3))
         *       }
         *   }
         */

        let env: Environment<EnvValue> = Environment::new();

        let func_soma1 = FuncDef(Function {
            name: "soma1".to_string(),
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

        let func_soma_mut = FuncDef(Function {
            name: "soma_mut".to_string(),
            kind: Some(TInteger),
            params: Some(vec![
                ("a".to_string(), TInteger),
                ("b".to_string(), TInteger),
                ("m".to_string(), TInteger),
            ]),
            body: Some(Box::new(Return(Box::new(Mul(
                Box::new(Add(
                    Box::new(Var("a".to_string())),
                    Box::new(Var("b".to_string())),
                )),
                Box::new(Var("m".to_string())),
            ))))),
        });

        let func_sub = FuncDef(Function {
            name: "sub".to_string(),
            kind: Some(TInteger),
            params: Some(vec![
                ("a".to_string(), TInteger),
                ("b".to_string(), TInteger),
            ]),
            body: Some(Box::new(Return(Box::new(Sub(
                Box::new(Var("a".to_string())),
                Box::new(Var("b".to_string())),
            ))))),
        });

        let func_sub_mut = FuncDef(Function {
            name: "sub_mut".to_string(),
            kind: Some(TInteger),
            params: Some(vec![
                ("a".to_string(), TInteger),
                ("b".to_string(), TInteger),
                ("m".to_string(), TInteger),
            ]),
            body: Some(Box::new(Return(Box::new(Mul(
                Box::new(Sub(
                    Box::new(Var("a".to_string())),
                    Box::new(Var("b".to_string())),
                )),
                Box::new(Var("m".to_string())),
            ))))),
        });

        let body_test = Box::new(AssertEQ(
            Box::new(FuncCall("soma1".to_string(), vec![CInt(1), CInt(2)])),
            Box::new(FuncCall(
                "soma_mut".to_string(),
                vec![CInt(1), CInt(2), CInt(3)],
            )),
            "Somas diferentes".to_string(),
        ));

        let body_test_1 = Box::new(AssertNEQ(
            Box::new(FuncCall("sub".to_string(), vec![CInt(1), CInt(2)])),
            Box::new(FuncCall(
                "sub_mut".to_string(),
                vec![CInt(1), CInt(2), CInt(3)],
            )),
            "Subtrações diferentes".to_string(),
        ));

        let mut body_mod_test = Box::new(TestDef(Function {
            name: "teste".to_string(),
            kind: Some(TVoid),
            params: None,
            body: Some(body_test.clone()),
        }));

        body_mod_test = Box::new(Sequence(
            body_mod_test.clone(),
            Box::new(TestDef(Function {
                name: "teste_1".to_string(),
                kind: Some(TVoid),
                params: None,
                body: Some(body_test_1.clone()),
            })),
        ));

        let mod_test_def = Box::new(ModTestDef("testes".to_string(), body_mod_test));
        let program: Box<Statement> = Box::new(Sequence(
            Box::new(func_soma1),
            Box::new(Sequence(
                Box::new(func_soma_mut),
                Box::new(Sequence(
                    Box::new(func_sub),
                    Box::new(Sequence(Box::new(func_sub_mut), mod_test_def)),
                )),
            )),
        ));

        let tests_set: Vec<(String, Option<String>)> = vec![("testes".to_string(), None)];
        let results: HashSet<(String, String, Option<String>)> = HashSet::from([
            (
                "testes::teste".to_string(),
                "Falhou".to_string(),
                Some("Erro: Somas diferentes".to_string()),
            ),
            ("testes::teste_1".to_string(), "Passou".to_string(), None),
        ]);

        match run(*program, &env) {
            Ok(ControlFlow::Continue(new_env)) => match execute_tests(tests_set, &new_env) {
                Ok(result) => {
                    assert_eq!(results, result)
                }
                Err(e) => assert!(false, "{}", e),
            },
            Ok(ControlFlow::Return(_)) => assert!(false),
            Err(s) => assert!(false, "{}", s),
        }
    }
    #[test]
    fn eval_only_test_1() {
        /*
         * Test for function test_1 inside modTest definition
         *
         *
         *   def soma1(a, b):
         *       return a+b
         *   def soma_mut(a, b, m):
         *       return(a+b)*m
         *   def sub (a,b):
         *      return a-b
         *   def sub_mut (a,b,m):
         *      return (a-b)*m
         *
         *   modTest testes {
         *       modTest teste {
         *           assertEQ(soma1(1, 2), soma_mut(1, 2, 3))
         *       }
         *      modTest teste_1{
         *          assertNEQ(sub(1,2), submut(1,2,3))}
         *      }
         *  }
         */
        let env: Environment<EnvValue> = Environment::new();

        let func_soma1 = FuncDef(Function {
            name: "soma1".to_string(),
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

        let func_soma_mut = FuncDef(Function {
            name: "soma_mut".to_string(),
            kind: Some(TInteger),
            params: Some(vec![
                ("a".to_string(), TInteger),
                ("b".to_string(), TInteger),
                ("m".to_string(), TInteger),
            ]),
            body: Some(Box::new(Return(Box::new(Mul(
                Box::new(Add(
                    Box::new(Var("a".to_string())),
                    Box::new(Var("b".to_string())),
                )),
                Box::new(Var("m".to_string())),
            ))))),
        });

        let func_sub = FuncDef(Function {
            name: "sub".to_string(),
            kind: Some(TInteger),
            params: Some(vec![
                ("a".to_string(), TInteger),
                ("b".to_string(), TInteger),
            ]),
            body: Some(Box::new(Return(Box::new(Sub(
                Box::new(Var("a".to_string())),
                Box::new(Var("b".to_string())),
            ))))),
        });

        let func_sub_mut = FuncDef(Function {
            name: "sub_mut".to_string(),
            kind: Some(TInteger),
            params: Some(vec![
                ("a".to_string(), TInteger),
                ("b".to_string(), TInteger),
                ("m".to_string(), TInteger),
            ]),
            body: Some(Box::new(Return(Box::new(Mul(
                Box::new(Sub(
                    Box::new(Var("a".to_string())),
                    Box::new(Var("b".to_string())),
                )),
                Box::new(Var("m".to_string())),
            ))))),
        });

        let body_test = Box::new(AssertEQ(
            Box::new(FuncCall("soma1".to_string(), vec![CInt(1), CInt(2)])),
            Box::new(FuncCall(
                "soma_mut".to_string(),
                vec![CInt(1), CInt(2), CInt(3)],
            )),
            "Somas diferentes".to_string(),
        ));

        let body_test_1 = Box::new(AssertNEQ(
            Box::new(FuncCall("sub".to_string(), vec![CInt(1), CInt(2)])),
            Box::new(FuncCall(
                "sub_mut".to_string(),
                vec![CInt(1), CInt(2), CInt(3)],
            )),
            "Subtrações diferentes".to_string(),
        ));

        let body_mod_test = Box::new(Sequence(
            Box::new(TestDef(Function {
                name: "teste".to_string(),
                kind: Some(TVoid),
                params: None,
                body: Some(body_test.clone()),
            })),
            Box::new(TestDef(Function {
                name: "teste_1".to_string(),
                kind: Some(TVoid),
                params: None,
                body: Some(body_test_1.clone()),
            })),
        ));

        let mod_test_def = Box::new(ModTestDef("testes".to_string(), body_mod_test));

        let program: Box<Statement> = Box::new(Sequence(
            Box::new(func_soma1),
            Box::new(Sequence(
                Box::new(func_soma_mut),
                Box::new(Sequence(
                    Box::new(func_sub),
                    Box::new(Sequence(Box::new(func_sub_mut), mod_test_def)),
                )),
            )),
        ));

        let tests_set: Vec<(String, Option<String>)> =
            vec![("testes".to_string(), Some("teste_1".to_string()))];

        let results: HashSet<(String, String, Option<String>)> =
            HashSet::from([("testes::teste_1".to_string(), "Passou".to_string(), None)]);

        match run(*program, &env) {
            Ok(ControlFlow::Continue(new_env)) => match execute_tests(tests_set, &new_env) {
                Ok(result) => {
                    assert_eq!(results, result)
                }
                Err(e) => assert!(false, "{}", e),
            },
            Ok(ControlFlow::Return(_)) => assert!(false),
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn eval_only_test() {
        /*
         * Test for function test inside modTest definition
         *
         *
         *   def soma1(a, b):
         *       return a+b
         *   def soma_mut(a, b, m):
         *       return(a+b)*m
         *   def sub (a,b):
         *      return a-b
         *   def sub_mut (a,b,m):
         *      return (a-b)*m
         *
         *   modTest testes {
         *       modTest teste {
         *           assertEQ(soma1(1, 2), soma_mut(1, 2, 3))
         *       }
         *      modTest teste_1{
         *          assertNEQ(sub(1,2), submut(1,2,3))}
         *      }
         *  }
         */
        let env: Environment<EnvValue> = Environment::new();

        let func_soma1 = FuncDef(Function {
            name: "soma1".to_string(),
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

        let func_soma_mut = FuncDef(Function {
            name: "soma_mut".to_string(),
            kind: Some(TInteger),
            params: Some(vec![
                ("a".to_string(), TInteger),
                ("b".to_string(), TInteger),
                ("m".to_string(), TInteger),
            ]),
            body: Some(Box::new(Return(Box::new(Mul(
                Box::new(Add(
                    Box::new(Var("a".to_string())),
                    Box::new(Var("b".to_string())),
                )),
                Box::new(Var("m".to_string())),
            ))))),
        });

        let func_sub = FuncDef(Function {
            name: "sub".to_string(),
            kind: Some(TInteger),
            params: Some(vec![
                ("a".to_string(), TInteger),
                ("b".to_string(), TInteger),
            ]),
            body: Some(Box::new(Return(Box::new(Sub(
                Box::new(Var("a".to_string())),
                Box::new(Var("b".to_string())),
            ))))),
        });

        let func_sub_mut = FuncDef(Function {
            name: "sub_mut".to_string(),
            kind: Some(TInteger),
            params: Some(vec![
                ("a".to_string(), TInteger),
                ("b".to_string(), TInteger),
                ("m".to_string(), TInteger),
            ]),
            body: Some(Box::new(Return(Box::new(Mul(
                Box::new(Sub(
                    Box::new(Var("a".to_string())),
                    Box::new(Var("b".to_string())),
                )),
                Box::new(Var("m".to_string())),
            ))))),
        });

        let body_test = Box::new(AssertEQ(
            Box::new(FuncCall("soma1".to_string(), vec![CInt(1), CInt(2)])),
            Box::new(FuncCall(
                "soma_mut".to_string(),
                vec![CInt(1), CInt(2), CInt(3)],
            )),
            "Somas diferentes".to_string(),
        ));

        let body_test_1 = Box::new(AssertNEQ(
            Box::new(FuncCall("sub".to_string(), vec![CInt(1), CInt(2)])),
            Box::new(FuncCall(
                "sub_mut".to_string(),
                vec![CInt(1), CInt(2), CInt(3)],
            )),
            "Subtrações diferentes".to_string(),
        ));

        let body_mod_test = Box::new(Sequence(
            Box::new(TestDef(Function {
                name: "teste".to_string(),
                kind: Some(TVoid),
                params: None,
                body: Some(body_test.clone()),
            })),
            Box::new(TestDef(Function {
                name: "teste_1".to_string(),
                kind: Some(TVoid),
                params: None,
                body: Some(body_test_1.clone()),
            })),
        ));

        let mod_test_def = Box::new(ModTestDef("testes".to_string(), body_mod_test));

        let program: Box<Statement> = Box::new(Sequence(
            Box::new(func_soma1),
            Box::new(Sequence(
                Box::new(func_soma_mut),
                Box::new(Sequence(
                    Box::new(func_sub),
                    Box::new(Sequence(Box::new(func_sub_mut), mod_test_def)),
                )),
            )),
        ));

        let tests_set: Vec<(String, Option<String>)> =
            vec![("testes".to_string(), Some("teste".to_string()))];

        let results: HashSet<(String, String, Option<String>)> = HashSet::from([(
            "testes::teste".to_string(),
            "Falhou".to_string(),
            Some("Erro: Somas diferentes".to_string()),
        )]);

        match run(*program, &env) {
            Ok(ControlFlow::Continue(new_env)) => match execute_tests(tests_set, &new_env) {
                Ok(result) => {
                    assert_eq!(results, result)
                }
                Err(e) => assert!(false, "{}", e),
            },
            Ok(ControlFlow::Return(_)) => assert!(false),
            Err(s) => assert!(false, "{}", s),
        }
    }
}
