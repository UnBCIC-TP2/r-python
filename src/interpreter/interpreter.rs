use crate::ir::ast::{EnvValue, Environment, Expression, Name, Statement, Type};
use crate::tc::type_checker::{check_stmt, ControlType};
use crate::HashMap;

type ErrorMessage = String;

#[derive(Debug)]
pub enum ControlFlow {
    Continue(Environment),
    Return(EnvValue),
}

pub fn eval(exp: Expression, env: &Environment) -> Result<EnvValue, ErrorMessage> {
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
        Expression::FuncCall(name, args) => call(name, args, env),
        Expression::MetaExp(f, args, return_type) => meta(f, args, return_type, env),
        _ if is_constant(exp.clone()) => Ok(EnvValue::Exp(exp)),
        _ => Err(String::from("Not implemented yet.")),
    }
}

//helper function for executing blocks
fn execute_block(stmts: Vec<Statement>, env: &Environment) -> Result<ControlFlow, ErrorMessage> {
    let mut current_env = env.clone();

    for stmt in stmts {
        match execute(stmt, &current_env, false)? {
            ControlFlow::Continue(new_env) => current_env = new_env,
            ControlFlow::Return(value) => return Ok(ControlFlow::Return(value)),
        }
    }

    Ok(ControlFlow::Continue(current_env))
}

pub fn execute(
    stmt: Statement,
    env: &Environment,
    mut init: bool,
) -> Result<ControlFlow, ErrorMessage> {
    let mut new_env = env.clone();

    if init {
        match check_stmt(stmt.clone(), &new_env, None)? {
            ControlType::Continue(control_env) => new_env = control_env,
            ControlType::Return(_) => unreachable!(),
        }
        init = false;
    }

    match stmt {
        Statement::Assignment(name, exp, _) => {
            let value = eval(*exp, &new_env)?;
            new_env.entry(name).and_modify(|e| e.0 = Some(value));
            Ok(ControlFlow::Continue(new_env))
        }
        Statement::IfThenElse(cond, then_stmt, else_stmt) => {
            let value = eval(*cond, &new_env)?;
            match value {
                EnvValue::Exp(Expression::CTrue) => match *then_stmt {
                    Statement::Block(stmts) => execute_block(stmts, &new_env),
                    _ => execute(*then_stmt, &new_env, false),
                },
                EnvValue::Exp(Expression::CFalse) => match else_stmt {
                    Some(else_stmt) => match *else_stmt {
                        Statement::Block(stmts) => execute_block(stmts, &new_env),
                        _ => execute(*else_stmt, &new_env, false),
                    },
                    None => Ok(ControlFlow::Continue(new_env)),
                },
                _ => Err("Condition must evaluate to a boolean".to_string()),
            }
        }

        Statement::Block(stmts) => execute_block(stmts, &new_env),

        Statement::While(cond, stmt) => {
            let mut value = eval(*cond.clone(), &new_env)?;
            loop {
                match value {
                    EnvValue::Exp(Expression::CTrue) => {
                        match execute(*stmt.clone(), &new_env, init)? {
                            ControlFlow::Continue(control_env) => {
                                new_env = control_env;
                                value = eval(*cond.clone(), &new_env)?;
                            }
                            ControlFlow::Return(value) => return Ok(ControlFlow::Return(value)),
                        }
                    }
                    EnvValue::Exp(Expression::CFalse) => return Ok(ControlFlow::Continue(new_env)),
                    _ => unreachable!(),
                }
            }
        }
        Statement::Sequence(s1, s2) => match execute(*s1, &new_env, init)? {
            ControlFlow::Continue(control_env) => {
                new_env = control_env;
                execute(*s2, &new_env, init)
            }
            ControlFlow::Return(value) => return Ok(ControlFlow::Return(value)),
        },
        Statement::FuncDef(name, func) => {
            new_env.insert(
                name,
                (Some(EnvValue::Func(func.clone())), func.kind.clone()),
            );
            Ok(ControlFlow::Continue(new_env))
        }
        Statement::Return(exp) => {
            let value = eval(*exp, &new_env)?;
            Ok(ControlFlow::Return(value))
        }

        Statement::MetaStmt(f, args_exprs, return_type) => {
            let mut args_values = Vec::new();
            for expr in &args_exprs {
                let env_value = eval(expr.clone(), &new_env)?;
                args_values.push(env_value);
            }
            

            let result_value = f(args_values);

            
            new_env.insert(
                "metaResult".to_string(),
                (Some(result_value.clone()), return_type.clone()),
            );

            Ok(ControlFlow::Continue(new_env))
        }

        _ => Err(String::from("not implemented yet")),
    }
}

fn call(name: Name, args: Vec<Expression>, env: &Environment) -> Result<EnvValue, ErrorMessage> {
    match env.get(&name) {
        Some((Some(EnvValue::Func(func)), _)) => {
            let mut new_env = HashMap::new();

            // Copy global functions to new environment
            for (key, value) in env.iter() {
                if let (Some(EnvValue::Func(_)), _) = value {
                    new_env.insert(key.clone(), value.clone());
                }
            }

            // Evaluate and bind arguments
            if let Some(params) = &func.params {
                for (param, arg) in params.iter().zip(args) {
                    let arg_value = eval(arg, env)?;
                    new_env.insert(param.0.clone(), (Some(arg_value), param.1.clone()));
                }
            }

            // Execute function body
            match execute(*func.body.clone(), &new_env, false)? {
                ControlFlow::Return(value) => Ok(value),
                ControlFlow::Continue(_) => Err("Function did not return a value".to_string()),
            }
        }
        _ => Err(format!("Function {} not found", name)),
    }
}

fn is_constant(exp: Expression) -> bool {
    match exp {
        Expression::CTrue => true,
        Expression::CFalse => true,
        Expression::CInt(_) => true,
        Expression::CReal(_) => true,
        Expression::CString(_) => true,
        _ => false,
    }
}

fn lookup(name: String, env: &Environment) -> Result<EnvValue, ErrorMessage> {
    match env.get(&name) {
        Some((Some(value), _)) => Ok(value.clone()),
        _ => Err(format!("Variable {} not found", name)),
    }
}

/* Arithmetic Operations */
fn eval_binary_arith_op<F>(
    lhs: Expression,
    rhs: Expression,
    env: &Environment,
    op: F,
    error_msg: &str,
) -> Result<EnvValue, ErrorMessage>
where
    F: Fn(f64, f64) -> f64,
{
    let v1 = eval(lhs, env)?;
    let v2 = eval(rhs, env)?;
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
        _ => Err(error_msg.to_string()),
    }
}

fn add(lhs: Expression, rhs: Expression, env: &Environment) -> Result<EnvValue, ErrorMessage> {
    eval_binary_arith_op(
        lhs,
        rhs,
        env,
        |a, b| a + b,
        "addition '(+)' is only defined for numbers (integers and real).",
    )
}

fn sub(lhs: Expression, rhs: Expression, env: &Environment) -> Result<EnvValue, ErrorMessage> {
    eval_binary_arith_op(
        lhs,
        rhs,
        env,
        |a, b| a - b,
        "subtraction '(-)' is only defined for numbers (integers and real).",
    )
}

fn mul(lhs: Expression, rhs: Expression, env: &Environment) -> Result<EnvValue, ErrorMessage> {
    eval_binary_arith_op(
        lhs,
        rhs,
        env,
        |a, b| a * b,
        "multiplication '(*)' is only defined for numbers (integers and real).",
    )
}

fn div(lhs: Expression, rhs: Expression, env: &Environment) -> Result<EnvValue, ErrorMessage> {
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
    env: &Environment,
    op: F,
    error_msg: &str,
) -> Result<EnvValue, ErrorMessage>
where
    F: Fn(bool, bool) -> Expression,
{
    let v1 = eval(lhs, env)?;
    let v2 = eval(rhs, env)?;
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
        _ => Err(error_msg.to_string()),
    }
}

fn and(lhs: Expression, rhs: Expression, env: &Environment) -> Result<EnvValue, ErrorMessage> {
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

fn or(lhs: Expression, rhs: Expression, env: &Environment) -> Result<EnvValue, ErrorMessage> {
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

fn not(lhs: Expression, env: &Environment) -> Result<EnvValue, ErrorMessage> {
    let v = eval(lhs, env)?;
    match v {
        EnvValue::Exp(Expression::CTrue) => Ok(EnvValue::Exp(Expression::CFalse)),
        EnvValue::Exp(Expression::CFalse) => Ok(EnvValue::Exp(Expression::CTrue)),
        _ => Err(String::from("'not' is only defined for booleans.")),
    }
}

/* Relational Operations */
fn eval_binary_rel_op<F>(
    lhs: Expression,
    rhs: Expression,
    env: &Environment,
    op: F,
    error_msg: &str,
) -> Result<EnvValue, ErrorMessage>
where
    F: Fn(f64, f64) -> Expression,
{
    let v1 = eval(lhs, env)?;
    let v2 = eval(rhs, env)?;
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
        _ => Err(error_msg.to_string()),
    }
}

fn eq(lhs: Expression, rhs: Expression, env: &Environment) -> Result<EnvValue, ErrorMessage> {
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

fn gt(lhs: Expression, rhs: Expression, env: &Environment) -> Result<EnvValue, ErrorMessage> {
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

fn lt(lhs: Expression, rhs: Expression, env: &Environment) -> Result<EnvValue, ErrorMessage> {
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

fn gte(lhs: Expression, rhs: Expression, env: &Environment) -> Result<EnvValue, ErrorMessage> {
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

fn lte(lhs: Expression, rhs: Expression, env: &Environment) -> Result<EnvValue, ErrorMessage> {
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

fn meta(
    f: fn(Vec<EnvValue>) -> EnvValue,
    args: Vec<Expression>,
    return_type: Type,
    env: &Environment,
) -> Result<EnvValue, ErrorMessage> {

    let mut args_values = Vec::new();
    for expr in &args {
        let env_value = eval(expr.clone(), env)?;
        args_values.push(env_value);
    }

    let result_value = f(args_values);

    if get_type_env_value(&result_value) != return_type {
                return Err(format!(
                    "Tipo incorreto: esperado {:?}, mas a função retornou {:?}",
                    return_type.clone(),
                    get_type_env_value(&result_value)
                ));
            }
    Ok(result_value)
}

pub fn get_type_env_value(value: &EnvValue) -> Type {
    match value {
        EnvValue::Exp(Expression::CInt(_)) => Type::TInteger,
        EnvValue::Exp(Expression::CReal(_)) => Type::TReal,
        EnvValue::Exp(Expression::CString(_)) => Type::TString,
        EnvValue::Exp(Expression::CTrue) | EnvValue::Exp(Expression::CFalse) => Type::TBool,
        EnvValue::Func(_) => Type::TFunction,
        _ => unreachable!(),
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
    use crate::stdlib::math::{sqrt, gcd};
    use crate::stdlib::string::{str_upper};
    use crate::stdlib::list::{len};

    use approx::relative_eq;

    #[test]
    fn eval_constant() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let c20 = CInt(20);

        assert_eq!(eval(c10, &env), Ok(EnvValue::Exp(CInt(10))));
        assert_eq!(eval(c20, &env), Ok(EnvValue::Exp(CInt(20))));
    }

    #[test]
    fn eval_add_expression1() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let c20 = CInt(20);
        let add1 = Add(Box::new(c10), Box::new(c20));
        assert_eq!(eval(add1, &env), Ok(EnvValue::Exp(CInt(30))));
    }

    #[test]
    fn eval_add_expression2() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let c20 = CInt(20);
        let c30 = CInt(30);
        let add1 = Add(Box::new(c10), Box::new(c20));
        let add2 = Add(Box::new(add1), Box::new(c30));
        assert_eq!(eval(add2, &env), Ok(EnvValue::Exp(CInt(60))));
    }

    #[test]
    fn eval_add_expression3() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let c20 = CReal(20.5);
        let add1 = Add(Box::new(c10), Box::new(c20));
        assert_eq!(eval(add1, &env), Ok(EnvValue::Exp(CReal(30.5))));
    }

    #[test]
    fn eval_sub_expression1() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let c20 = CInt(20);
        let sub1 = Sub(Box::new(c20), Box::new(c10));
        assert_eq!(eval(sub1, &env), Ok(EnvValue::Exp(CInt(10))));
    }

    #[test]
    fn eval_sub_expression2() {
        let env = HashMap::new();
        let c100 = CInt(100);
        let c200 = CInt(300);
        let sub1 = Sub(Box::new(c200), Box::new(c100));
        assert_eq!(eval(sub1, &env), Ok(EnvValue::Exp(CInt(200))));
    }

    #[test]
    fn eval_sub_expression3() {
        let env = HashMap::new();
        let c100 = CReal(100.5);
        let c300 = CInt(300);
        let sub1 = Sub(Box::new(c300), Box::new(c100));
        assert_eq!(eval(sub1, &env), Ok(EnvValue::Exp(CReal(199.5))));
    }

    #[test]
    fn eval_mul_expression1() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let c20 = CInt(20);
        let mul1 = Mul(Box::new(c10), Box::new(c20));
        assert_eq!(eval(mul1, &env), Ok(EnvValue::Exp(CInt(200))));
    }

    #[test]
    fn eval_mul_expression2() {
        let env = HashMap::new();
        let c10 = CReal(10.5);
        let c20 = CInt(20);
        let mul1 = Mul(Box::new(c10), Box::new(c20));
        assert_eq!(eval(mul1, &env), Ok(EnvValue::Exp(CReal(210.0))));
    }

    #[test]
    fn eval_div_expression1() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let c20 = CInt(20);
        let div1 = Div(Box::new(c20), Box::new(c10));
        assert_eq!(eval(div1, &env), Ok(EnvValue::Exp(CInt(2))));
    }

    #[test]
    fn eval_div_expression2() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let c3 = CInt(3);
        let div1 = Div(Box::new(c10), Box::new(c3));
        assert_eq!(eval(div1, &env), Ok(EnvValue::Exp(CInt(3))));
    }

    #[test]
    fn eval_div_expression3() {
        let env = HashMap::new();
        let c3 = CInt(3);
        let c21 = CInt(21);
        let div1 = Div(Box::new(c21), Box::new(c3));
        assert_eq!(eval(div1, &env), Ok(EnvValue::Exp(CInt(7))));
    }

    #[test]
    fn eval_div_expression4() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let c3 = CReal(3.0);
        let div1 = Div(Box::new(c10), Box::new(c3));
        let res = eval(div1, &env);
        match res {
            Ok(EnvValue::Exp(Expression::CReal(v))) => {
                assert!(relative_eq!(v, 3.3333333333333335, epsilon = f64::EPSILON))
            }
            Err(msg) => assert!(false, "{}", msg),
            _ => assert!(false, "Not expected."),
        }
    }

    #[test]
    fn eval_variable() {
        let env = HashMap::from([
            (String::from("x"), (Some(EnvValue::Exp(CInt(10))), TInteger)),
            (String::from("y"), (Some(EnvValue::Exp(CInt(20))), TInteger)),
        ]);
        let v1 = Var(String::from("x"));
        let v2 = Var(String::from("y"));
        assert_eq!(eval(v1, &env), Ok(EnvValue::Exp(CInt(10))));
        assert_eq!(eval(v2, &env), Ok(EnvValue::Exp(CInt(20))));
    }

    #[test]
    fn eval_expression_with_variables() {
        let env = HashMap::from([
            (String::from("a"), (Some(EnvValue::Exp(CInt(5))), TInteger)),
            (String::from("b"), (Some(EnvValue::Exp(CInt(3))), TInteger)),
        ]);
        let expr = Mul(
            Box::new(Var(String::from("a"))),
            Box::new(Add(Box::new(Var(String::from("b"))), Box::new(CInt(2)))),
        );
        assert_eq!(eval(expr, &env), Ok(EnvValue::Exp(CInt(25))));
    }

    #[test]
    fn eval_nested_expressions() {
        let env = HashMap::new();
        let expr = Add(
            Box::new(Mul(Box::new(CInt(2)), Box::new(CInt(3)))),
            Box::new(Sub(Box::new(CInt(10)), Box::new(CInt(4)))),
        );
        assert_eq!(eval(expr, &env), Ok(EnvValue::Exp(CInt(12))));
    }

    #[test]
    fn eval_variable_not_found() {
        let env = HashMap::new();
        let var_expr = Var(String::from("z"));

        assert_eq!(
            eval(var_expr, &env),
            Err(String::from("Variable z not found"))
        );
    }

    #[test]
    fn execute_assignment() {
        let env = HashMap::new();
        let assign_stmt = Assignment(String::from("x"), Box::new(CInt(42)), Some(TInteger));

        match execute(assign_stmt, &env, true) {
            Ok(ControlFlow::Continue(new_env)) => assert_eq!(
                new_env.get("x"),
                Some(&(Some(EnvValue::Exp(CInt(42))), TInteger))
            ),
            Ok(ControlFlow::Return(_)) => assert!(false),
            Err(s) => assert!(false, "{}", s),
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
        let env = HashMap::new();

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

        match execute(program, &env, true) {
            Ok(ControlFlow::Continue(new_env)) => {
                assert_eq!(
                    new_env.get("y"),
                    Some(&(Some(EnvValue::Exp(CInt(55))), TInteger))
                );
                assert_eq!(
                    new_env.get("x"),
                    Some(&(Some(EnvValue::Exp(CInt(0))), TInteger))
                );
            }
            Ok(ControlFlow::Return(_)) => assert!(false),
            Err(s) => assert!(false, "{}", s),
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
        let env = HashMap::new();

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

        match execute(program, &env, true) {
            Ok(ControlFlow::Continue(new_env)) => assert_eq!(
                new_env.get("y"),
                Some(&(Some(EnvValue::Exp(CInt(1))), TInteger))
            ),
            Ok(ControlFlow::Return(_)) => assert!(false),
            Err(s) => assert!(false, "{}", s),
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

        let env = HashMap::new();

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

        match execute(program, &env, true) {
            Ok(ControlFlow::Continue(new_env)) => assert_eq!(
                new_env.get("y"),
                Some(&(Some(EnvValue::Exp(CInt(2))), TInteger))
            ),
            Ok(ControlFlow::Return(_)) => assert!(false),
            Err(s) => assert!(false, "{}", s),
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

    //     match execute(&program, env) {
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

    //     match execute(&program, env) {
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
        let env = HashMap::new();

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

        match execute(program, &env, true) {
            Ok(ControlFlow::Continue(new_env)) => {
                assert_eq!(
                    new_env.get("x"),
                    Some(&(Some(EnvValue::Exp(CInt(5))), TInteger))
                );
                assert_eq!(
                    new_env.get("y"),
                    Some(&(Some(EnvValue::Exp(CInt(0))), TInteger))
                );
                assert_eq!(
                    new_env.get("z"),
                    Some(&(Some(EnvValue::Exp(CInt(13))), TInteger))
                );
            }
            Ok(ControlFlow::Return(_)) => assert!(false),
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn recursive_func_def_call() {
        /*
         * Test for a recursive function
         *
         * > def fibonacci(n: TInteger) -> TInteger:
         * >    if n < 0:
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
        let env = Environment::new();

        let func = FuncDef(
            "fibonacci".to_string(),
            Function {
                kind: TInteger,
                params: Some(vec![("n".to_string(), TInteger)]),
                body: Box::new(Sequence(
                    Box::new(IfThenElse(
                        Box::new(LT(Box::new(Var("n".to_string())), Box::new(CInt(0)))),
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
                )),
            },
        );

        let program = Sequence(
            Box::new(func),
            Box::new(Assignment(
                "fib".to_string(),
                Box::new(FuncCall("fibonacci".to_string(), vec![CInt(10)])),
                Some(TInteger),
            )),
        );

        match execute(program, &env, true) {
            Ok(ControlFlow::Continue(new_env)) => assert_eq!(
                new_env.get("fib"),
                Some(&(Some(EnvValue::Exp(CInt(34))), TInteger))
            ),
            Ok(ControlFlow::Return(_)) => assert!(false),
            Err(s) => assert!(false, "{}", s),
        }
    }

    
    #[test]
    fn metastmt_sqrt() {
        /*
        * Test for the square root function (sqrt) using MetaStmt
        *
        * Imaginary rpy code:
        *
        * > x: TReal = 16.0
        * > MetaStmt(sqrt, [x], TReal)
        * > result: TReal = metaResult
        *
        * After execution, 'result' should be 4.0 (sqrt(16.0) = 4.0).
        */

        let env = Environment::new();

        let assign_x = Statement::Assignment(
            "x".to_string(),
            Box::new(Expression::CReal(16.0)),
            Some(TReal),
        );

        let meta_stmt = Statement::MetaStmt(
            sqrt,
            vec![Expression::Var("x".to_string())],
            TReal
        );

        let assign_result = Statement::Assignment(
            "result".to_string(),
            Box::new(Expression::Var("metaResult".to_string())),
            Some(TReal),
        );

        let program = Statement::Sequence(
            Box::new(assign_x),
            Box::new(Statement::Sequence(
                Box::new(meta_stmt),
                Box::new(assign_result),
            )),
        );

        match execute(program, &env, true) {
            Ok(ControlFlow::Continue(new_env)) => {
                if let Some(&(Some(EnvValue::Exp(Expression::CReal(value))), _)) = new_env.get("metaResult") {
                    assert_eq!(value, 4.0);
                } else {
                    panic!("Variable 'metaResult' not found or has incorrect type");
                }
            }
            Ok(_) => panic!("The interpreter did not continue execution as expected"),
            Err(err) => panic!("Interpreter execution failed with error: {}", err),
        }
    }

    #[test]
    fn metastmt_function_sqrt() {
        /*
        * Test for the r-python square root function (sqrt) using MetaStmt
        *
        * Imaginary rpy code:
        *
        * > x: TReal = 25.0
        * > def sqrt(x: TReal) -> TReal:
        * >     MetaStmt(sqrt, [x], TReal)
        * >     return metaResult
        * > x = sqrt(x)
        *
        * After execution, 'x' should be 5.0 (sqrt(25.0) = 5.0).
        */
        let env = Environment::new();
        
        let assign_x = Statement::Assignment(
            "x".to_string(),
            Box::new(Expression::CReal(25.0)),
            Some(TReal),
        );

        let meta_stmt = Statement::MetaStmt(
            sqrt,
            vec![Expression::Var("x".to_string())],
            TReal
        );

        let func: Statement = FuncDef(
            "sqrt".to_string(), 
            Function {
                kind: TReal,
                params: Some(vec![("x".to_string(), TReal)]),
                body: Box::new(Sequence(
                    Box::new(meta_stmt),
                    Box::new(Return(
                        Box::new(Expression::Var("metaResult".to_string()))
                    ))
                ))
            }
            );
        
        let assign_result = Statement::Assignment(
            "x".to_string(),
            Box::new(Expression::FuncCall("sqrt".to_string(), vec![Expression::Var("x".to_string())])),
            Some(TReal),
        );

        let program = Statement::Sequence(
            Box::new(assign_x),
            Box::new(Statement::Sequence(
                Box::new(func),
                Box::new(assign_result),
            )),
        );

        match execute(program, &env, true) {
            Ok(ControlFlow::Continue(new_env)) => {
                if let Some(&(Some(EnvValue::Exp(Expression::CReal(value))), _)) = new_env.get("x") {
                    assert_eq!(value, 5.0);
                } else {
                    panic!("Variable 'x' not found or has incorrect type");
                }
            }
            Ok(_) => panic!("The interpreter did not continue execution as expected"),
            Err(err) => panic!("Interpreter execution failed with error: {}", err),
        }
    }

    #[test]
    fn metastmt_function_gcd() {
        /*
        * Test for the greatest common divisor function (gcd) using MetaStmt
        *
        * Imaginary rpy code:
        *
        * > a: TInteger = 48
        * > b: TInteger = 18
        * > def gcd(a: TInteger, b: TInteger) -> TInteger:
        * >     MetaStmt(gcd, [a, b], TInteger)
        * >     return metaResult
        * > result = gcd(a, b)
        *
        * After execution, 'result' should be 6 (gcd(48, 18) = 6).
        */
    
        let env = Environment::new();
    
        let assign_a = Statement::Assignment(
            "a".to_string(),
            Box::new(Expression::CInt(48)),
            Some(TInteger),
        );
    
        let assign_b = Statement::Assignment(
            "b".to_string(),
            Box::new(Expression::CInt(18)),
            Some(TInteger),
        );
    
        let meta_stmt = Statement::MetaStmt(
            gcd,
            vec![Expression::Var("a".to_string()), Expression::Var("b".to_string())],
            TInteger,
        );
    
        let func: Statement = Statement::FuncDef(
            "gcd".to_string(),
            Function {
                kind: TInteger,
                params: Some(vec![("a".to_string(), TInteger), ("b".to_string(), TInteger)]),
                body: Box::new(Statement::Sequence(
                    Box::new(meta_stmt),
                    Box::new(Statement::Return(
                        Box::new(Expression::Var("metaResult".to_string()))
                    )),
                )),
            },
        );
    
        let assign_result = Statement::Assignment(
            "result".to_string(),
            Box::new(Expression::FuncCall("gcd".to_string(), vec![Expression::Var("a".to_string()), Expression::Var("b".to_string())])),
            Some(TInteger),
        );
    
        let program = Statement::Sequence(
            Box::new(assign_a),
            Box::new(Statement::Sequence(
                Box::new(assign_b),
                Box::new(Statement::Sequence(
                    Box::new(func),
                    Box::new(assign_result),
                )),
            )),
        );
    
        match execute(program, &env, true) {
            Ok(ControlFlow::Continue(new_env)) => {
                if let Some(&(Some(EnvValue::Exp(Expression::CInt(value))), _)) = new_env.get("result") {
                    assert_eq!(value, 6);
                } else {
                    panic!("Variable 'result' not found or has incorrect type");
                }
            }
            Ok(_) => panic!("The interpreter did not continue execution as expected"),
            Err(err) => panic!("Interpreter execution failed with error: {}", err),
        }
    }
    
    #[test]
    fn metaexp_sqrt() {
        /*
        * Test for the r-python square root function (sqrt) using MetaExp
        *
        * Imaginary rpy code:
        *
        * > x: TReal = 25.0
        * > x = MetaExp(sqrt, [x], TReal)
        *
        * After execution, 'x' should be 5.0 (sqrt(25.0) = 5.0).
        */
    
        let env = Environment::new();
    
        let assign_x = Statement::Assignment(
            "x".to_string(),
            Box::new(Expression::CReal(25.0)),
            Some(Type::TReal),
        );
    
        let meta_expr = Expression::MetaExp(
            sqrt,
            vec![Expression::Var("x".to_string())],
            Type::TReal,
        );
    
        let assign_result = Statement::Assignment(
            "x".to_string(),
            Box::new(meta_expr),
            Some(Type::TReal),
        );
    
        let program = Statement::Sequence(
            Box::new(assign_x),
            Box::new(assign_result),
        );
    
        match execute(program, &env, true) {
            Ok(ControlFlow::Continue(new_env)) => {
                if let Some(&(Some(EnvValue::Exp(Expression::CReal(value))), _)) = new_env.get("x") {
                    assert_eq!(value, 5.0);
                } else {
                    panic!("Variable 'x' not found or has incorrect type");
                }
            }
            Ok(_) => panic!("The interpreter did not continue execution as expected"),
            Err(err) => panic!("Interpreter execution failed with error: {}", err),
        }
    }
    
    #[test]
    fn metaexp_function_gcd() {
        /*
        * Test for the greatest common divisor function (gcd) using MetaStmt
        *
        * Imaginary rpy code:
        *
        * > a: TInteger = 48
        * > b: TInteger = 18
        * > def gcd(a: TInteger, b: TInteger) -> TInteger:
        * >     return MetaStmt(gcd, [a, b], TInteger)
        * > result = gcd(a, b)
        *
        * After execution, 'result' should be 6 (gcd(48, 18) = 6).
        */
    
        let env = Environment::new();
    
        let assign_a = Statement::Assignment(
            "a".to_string(),
            Box::new(Expression::CInt(48)),
            Some(TInteger),
        );
    
        let assign_b = Statement::Assignment(
            "b".to_string(),
            Box::new(Expression::CInt(18)),
            Some(TInteger),
        );
    
        let meta_epx = Expression::MetaExp(
            gcd,
            vec![Expression::Var("a".to_string()), Expression::Var("b".to_string())],
            TInteger,
        );
    
        let func = Statement::FuncDef(
            "gcd".to_string(),
            Function {
                kind: TInteger,
                params: Some(vec![("a".to_string(), TInteger), ("b".to_string(), TInteger)]),
                body: Box::new(Statement::Return(Box::new(meta_epx))),
            },
        );
    
        let assign_result = Statement::Assignment(
            "result".to_string(),
            Box::new(Expression::FuncCall("gcd".to_string(), vec![Expression::Var("a".to_string()), Expression::Var("b".to_string())])),
            Some(TInteger),
        );
    
        let program = Statement::Sequence(
            Box::new(assign_a),
            Box::new(Statement::Sequence(
                Box::new(assign_b),
                Box::new(Statement::Sequence(
                    Box::new(func),
                    Box::new(assign_result),
                )),
            )),
        );
    
        match execute(program, &env, true) {
            Ok(ControlFlow::Continue(new_env)) => {
                if let Some(&(Some(EnvValue::Exp(Expression::CInt(value))), _)) = new_env.get("result") {
                    assert_eq!(value, 6);
                } else {
                    panic!("Variable 'result' not found or has incorrect type");
                }
            }
            Ok(_) => panic!("The interpreter did not continue execution as expected"),
            Err(err) => panic!("Interpreter execution failed with error: {}", err),
        }
    }

    #[test]
    fn metastmt_function_upper(){
        /*
        * Test for the r-python square root function (sqrt) using MetaStmt
        *
        * Imaginary rpy code:
        *
        * > x: TString = "banana"
        * > def upper(x: TString) -> TString:
        * >     MetaStmt(str_upper, [x], TString)
        * >     return metaResult
        * > x = upper(x)
        *
        * After execution, 'x' should be 5.0 (sqrt(25.0) = 5.0).
        */
        let env = Environment::new();
        let assign_x = Statement::Assignment(
            "x".to_string(),
            Box::new(Expression::CString("banana".to_string())),
            Some(TString)
        );
        let meta_stmt = Statement::MetaStmt(
            str_upper,
            vec![Expression::Var("x".to_string())],
            TString
        );
        let func = Statement::FuncDef(
            "upper".to_string(),
            Function{
                kind: TString,
                params: Some(vec![("x".to_string(), TString)]),
                body: Box::new(Sequence(
                    Box::new(meta_stmt),
                    Box::new(Return(
                        Box::new(Expression::Var("metaResult".to_string()))
                    ))
                ))
            }
        );
        let assign_result = Statement::Assignment(
            "x".to_string(),
            Box::new(Expression::FuncCall("upper".to_string(), vec![Expression::Var("x".to_string())])),
            Some(TString),
        );
        let program = Statement::Sequence(
            Box::new(assign_x),
            Box::new(Statement::Sequence(
                Box::new(func),
                Box::new(assign_result)
            ))
        );
        match execute(program, &env, true){
            Ok(ControlFlow::Continue(new_env)) => {
                if let Some(&(Some(EnvValue::Exp(Expression::CString(ref value))), _)) = new_env.get("x"){
                    assert_eq!(value, &"BANANA".to_string());
                } else{
                    panic!("Variable 'x' not found or has incorret type");
                }

            }
            Ok(_) => panic!("The interpreter did not continue execution as expect"),
            Err(err) => panic!("Interpreter execution failed with error: {}", err),

        }
    }
        
}
