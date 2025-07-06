use crate::environment::environment::Environment;
use crate::ir::ast::{Expression, Name, Type};
use std::sync::Arc;
type ErrorMessage = String;

pub fn check_expr(exp: &Expression, env: &Environment<Type>) -> Result<Type, ErrorMessage> {
    match exp {
        Expression::CTrue => Ok(Type::TBool),
        Expression::CFalse => Ok(Type::TBool),
        Expression::CVoid => Ok(Type::TVoid),
        Expression::CInt(_) => Ok(Type::TInteger),
        Expression::CReal(_) => Ok(Type::TReal),
        Expression::CString(_) => Ok(Type::TString),
        Expression::Add(l, r) => check_bin_arithmetic_expression(l, r, env),
        Expression::Sub(l, r) => check_bin_arithmetic_expression(l, r, env),
        Expression::Mul(l, r) => check_bin_arithmetic_expression(l, r, env),
        Expression::Div(l, r) => check_bin_arithmetic_expression(l, r, env),
        Expression::And(l, r) => check_bin_boolean_expression(l, r, env),
        Expression::Or(l, r) => check_bin_boolean_expression(l, r, env),
        Expression::Not(e) => check_not_expression(e, env),
        Expression::EQ(l, r) => check_bin_relational_expression(l, r, env),
        Expression::GT(l, r) => check_bin_relational_expression(l, r, env),
        Expression::LT(l, r) => check_bin_relational_expression(l, r, env),
        Expression::GTE(l, r) => check_bin_relational_expression(l, r, env),
        Expression::LTE(l, r) => check_bin_relational_expression(l, r, env),
        Expression::Var(name) => check_var_name(name.clone(), env),
        Expression::COk(e) => check_result_ok(e, env),
        Expression::CErr(e) => check_result_err(e, env),
        Expression::CJust(e) => check_maybe_just(e, env),
        Expression::CNothing => Ok(Type::TMaybe(Box::new(Type::TAny))),
        Expression::IsError(e) => check_iserror_type(e, env),
        Expression::IsNothing(e) => check_isnothing_type(e, env),
        Expression::Unwrap(e) => check_unwrap_type(e, env),
        Expression::Propagate(e) => check_propagate_type(e, env),
        Expression::ListValue(elements) => check_list_value(elements.as_slice(), env),
        Expression::Constructor(name, args) => check_adt_constructor(name.clone(), args, env),
        Expression::Match(expr, arms) => check_match_expression(expr, arms, &mut env.clone()),

        _ => Err("not implemented yet.".to_string()),
    }
}

fn check_var_name(name: Name, env: &Environment<Type>) -> Result<Type, ErrorMessage> {
    match env.lookup(&name) {
        Some((_, t)) => Ok(t.clone()),
        None => Err(format!("[Name Error] '{}' is not defined.", name)),
    }
}

fn check_bin_arithmetic_expression(
    left: &Expression,
    right: &Expression,
    env: &Environment<Type>
) -> Result<Type, ErrorMessage> {
    let left_type = check_expr(left, env)?;
    let right_type = check_expr(right, env)?;

    match (left_type, right_type) {
        (Type::TInteger, Type::TInteger) => Ok(Type::TInteger),
        (Type::TInteger, Type::TReal) => Ok(Type::TReal),
        (Type::TReal, Type::TInteger) => Ok(Type::TReal),
        (Type::TReal, Type::TReal) => Ok(Type::TReal),
        _ => Err(String::from("[Type Error] expecting numeric type values.")),
    }
}

fn check_bin_boolean_expression(
    left: &Expression,
    right: &Expression,
    env: &Environment<Type>,
) -> Result<Type, ErrorMessage> {
    let left_type = check_expr(left, env)?;
    let right_type = check_expr(right, env)?;

    match (left_type, right_type) {
        (Type::TBool, Type::TBool) => Ok(Type::TBool),
        _ => Err(String::from("[Type Error] expecting boolean type values.")),
    }
}

fn check_not_expression(exp: &Expression, env: &Environment<Type>) -> Result<Type, ErrorMessage> {
    let exp_type = check_expr(exp, env)?;

    match exp_type {
        Type::TBool => Ok(Type::TBool),
        _ => Err(String::from("[Type Error] expecting a boolean type value.")),
    }
}

fn check_bin_relational_expression(
    left: &Expression,
    right: &Expression,
    env: &Environment<Type>
) -> Result<Type, ErrorMessage> {
    let left_type = check_expr(left, env)?;
    let right_type = check_expr(right, env)?;

    match (left_type, right_type) {
        (Type::TInteger, Type::TInteger) => Ok(Type::TBool),
        (Type::TInteger, Type::TReal) => Ok(Type::TBool),
        (Type::TReal, Type::TInteger) => Ok(Type::TBool),
        (Type::TReal, Type::TReal) => Ok(Type::TBool),
        _ => Err(String::from("[Type Error] expecting numeric type values.")),
    }
}

fn check_result_ok(exp: &Expression, env: &Environment<Type>) -> Result<Type, ErrorMessage> {
    let exp_type = check_expr(exp, env)?;
    return Ok(Type::TResult(Box::new(exp_type), Box::new(Type::TAny)));
}

fn check_result_err(exp: &Expression, env: &Environment<Type>) -> Result<Type, ErrorMessage> {
    let exp_type = check_expr(exp, env)?;
    return Ok(Type::TResult(Box::new(Type::TAny), Box::new(exp_type)));
}

fn check_unwrap_type(exp: &Expression, env: &Environment<Type>) -> Result<Type, ErrorMessage> {
    let exp_type = check_expr(exp, env)?;

    match exp_type {
        Type::TMaybe(t) => Ok(*t),
        Type::TResult(tl, _) => Ok(*tl),
        _ => Err(String::from(
            "[Type Error] expecting a maybe or result type value.",
        )),
    }
}

fn check_propagate_type(exp: &Expression, env: &Environment<Type>) -> Result<Type, ErrorMessage> {
    let exp_type = check_expr(exp, env)?;

    match exp_type {
        Type::TMaybe(t) => Ok(*t),
        Type::TResult(tl, _) => Ok(*tl),
        _ => Err(String::from(
            "[Type Error] expecting a maybe or result type value.",
        )),
    }
}

fn check_maybe_just(exp: &Expression, env: &Environment<Type>) -> Result<Type, ErrorMessage> {
    let exp_type = check_expr(exp, env)?;
    Ok(Type::TMaybe(Box::new(exp_type)))
}

fn check_iserror_type(exp: &Expression, env: &Environment<Type>) -> Result<Type, ErrorMessage> {
    let v = check_expr(exp, env)?;

    match v {
        Type::TResult(_, _) => Ok(Type::TBool),
        _ => Err(String::from("[Type Error] expecting a result type value.")),
    }
}

fn check_isnothing_type(exp: &Expression, env: &Environment<Type>) -> Result<Type, ErrorMessage> {
    let exp_type = check_expr(exp, env)?;

    match exp_type {
        Type::TMaybe(_) => Ok(Type::TBool),
        _ => Err(String::from("[Type Error] expecting a maybe type value.")),
    }
}

fn check_list_value(
    elements: &[Expression],
    env: &Environment<Type>,
) -> Result<Type, ErrorMessage> {
    if elements.is_empty() {
        return Ok(Type::TList(Box::new(Type::TAny)));
    }

    // Check the type of the first element
    let first_type = check_expr(&elements[0], env)?;

    // Check that all other elements have the same type
    for element in elements.iter().skip(1) {
        let element_type = check_expr(element, env)?;
        if element_type != first_type {
            return Err(format!(
                "[Type Error] List elements must have the same type. Expected '{:?}', found '{:?}'.",
                first_type, element_type
            ));
        }
    }

    Ok(Type::TList(Box::new(first_type)))
}

fn check_adt_constructor(
    name: Name,
    args: &Vec<Box<Expression>>,
    env: &Environment<Type>
) -> Result<Type, ErrorMessage> {
    // Search for the constructor in all scopes (including globals)
    let found = env.stack // Search in stack
        .iter()
        .find_map(|scope| {
            scope.adts.iter()
            .find_map(|(adt_name, constructors)| {
                constructors.get(&name)
                    .map(|constructor| (
                        adt_name.clone(),
                        constructor.clone(),
                        Arc::clone(constructors)
                    ))
            })
        })
        .or_else(|| {  // Search in globals if not found in stack
        env.globals.adts.iter().find_map(|(adt_name, constructors)| {
            constructors.get(&name)
                .map(|constructor| (
                    adt_name.clone(),
                    constructor.clone(),
                    Arc::clone(constructors)
                ))
        })
    });

    match found {
        Some((adt_type_name, constructor, constructors)) => {
            // Check that we have the right number of arguments
            if args.len() != constructor.len() {
                return Err(format!(
                    "[Type Error] Constructor '{}' expects {} arguments, but got {}.",
                    name,
                    constructor.len(),
                    args.len()
                ));
            }
            // Check each argument's type
            for (arg, expected_type) in args.into_iter().zip(constructor.into_iter()) {
                let arg_type = check_expr(&*arg, env)?;
                if arg_type != expected_type {
                    return Err(format!(
                        "[Type Error] Argument type mismatch in constructor '{}'. Expected '{:?}', found '{:?}'.",
                        name, expected_type, arg_type
                    ));
                }
            }
            // Return the algebraic type
            Ok(Type::TAlgebraicData(adt_type_name, (*constructors).clone()))
        }
        None => Err(format!(
            "[Type Error] Constructor '{}' is not defined in any ADT.",
            name
        )),
    }
}

fn check_match_expression(
    expr: &Expression,
    arms: &Vec<((Name, Vec<Name>), Expression)>,
    env: &mut Environment<Type>
) -> Result<Type, ErrorMessage> {
    if let Type::TAlgebraicData(adt_name, constructors) = check_expr(expr, env)? {
        arms.iter()
            .fold(
                Ok(Vec::new()),
                |acc, ((name, vars), arm)| {
                    let mut arms_types = acc?;
                    match constructors.get(name) {
                        None => return Err(format!("[Type Error] Constructor '{}' is not defined in ADT '{}'.", name, adt_name)),
                        Some(types) => {
                            env.push();
                            if types.len() != vars.len() {
                                return Err(format!("[Type Error] Constructor '{}' expects {} arguments, but got {}.", name, types.len(), vars.len()));
                            }
                            // Updates the environment with the variables on the arm scope
                            for (tp, var) in types.into_iter().zip(vars.into_iter()) {
                                env.map_variable(var.clone(), false, tp.clone());
                            }

                            let arm_type = check_expr(arm, env)?;
                            // Checks if all of the arms evaluate to the same type
                            if arms_types.is_empty() {
                                arms_types.push(arm_type);
                            } else {
                                if arms_types[0] != arm_type {
                                    return Err(format!("[Type Error] All of the arms must evaluate to the same type!"));
                                }
                            }
                            env.pop();
                            Ok(arms_types)
                        }
                    }
            })
            .and_then(|v| v.into_iter().next().map_or(Err(format!("[Type Error] No arms in match!")), Ok))
    } else {
        Err(format!("[Type Error] Expression must be an algebraic data type for match expression."))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::environment::environment::Environment;
    use crate::ir::ast::Expression::*;
    use crate::ir::ast::Type::*;
    use std::collections::HashMap;

    #[test]
    fn check_constant() {
        let env = Environment::new();
        let c10 = CInt(10);

        assert_eq!(check_expr(&c10, &env), Ok(TInteger));
    }

    #[test]
    fn check_add_integers() {
        let env = Environment::new();

        let c10 = CInt(10);
        let c20 = CInt(20);
        let add = Add(Box::new(c10), Box::new(c20));

        assert_eq!(check_expr(&add, &env), Ok(TInteger));
    }

    #[test]
    fn check_add_reals() {
        let env = Environment::new();

        let c10 = CReal(10.5);
        let c20 = CReal(20.3);
        let add = Add(Box::new(c10), Box::new(c20));

        assert_eq!(check_expr(&add, &env), Ok(TReal));
    }

    #[test]
    fn check_add_real_and_integer() {
        let env = Environment::new();

        let c10 = CInt(10);
        let c20 = CReal(20.3);
        let add = Add(Box::new(c10), Box::new(c20));

        assert_eq!(check_expr(&add, &env), Ok(TReal));
    }

    #[test]
    fn check_add_integer_and_real() {
        let env = Environment::new();

        let c10 = CReal(10.5);
        let c20 = CInt(20);
        let add = Add(Box::new(c10), Box::new(c20));

        assert_eq!(check_expr(&add, &env), Ok(TReal));
    }

    #[test]
    fn check_type_error_arithmetic_expression() {
        let env = Environment::new();

        let e1 = CInt(10);
        let e2 = CFalse;
        let e3 = Add(Box::new(e1), Box::new(e2));

        assert!(
            matches!(check_expr(&e3, &env), Err(_)),
            "Expecting a type error."
        );
    }

    #[test]
    fn check_type_error_not_expression() {
        let env = Environment::new();

        let e1 = CInt(10);
        let e2 = Not(Box::new(e1));

        assert!(
            matches!(check_expr(&e2, &env), Err(_)),
            "Expecting a type error."
        );
    }

    #[test]
    fn check_type_error_and_expression() {
        let env = Environment::new();

        let e1 = CInt(10);
        let e2 = CTrue;
        let e3 = And(Box::new(e1), Box::new(e2));

        assert!(
            matches!(check_expr(&e3, &env), Err(_)),
            "Expecting a type error."
        );
    }

    #[test]
    fn check_type_error_or_expression() {
        let env = Environment::new();

        let e1 = CInt(10);
        let e2 = CTrue;
        let e3 = Or(Box::new(e1), Box::new(e2));

        assert!(
            matches!(check_expr(&e3, &env), Err(_)),
            "Expecting a type error."
        );
    }

    #[test]
    fn check_ok_result() {
        let env = Environment::new();
        let e1 = CReal(10.0);
        let e2 = COk(Box::new(e1));

        assert_eq!(
            check_expr(&e2, &env),
            Ok(TResult(Box::new(TReal), Box::new(TAny)))
        );
    }

    #[test]
    fn check_err_result() {
        let env = Environment::new();
        let e1 = CInt(1);
        let e2 = CErr(Box::new(e1));

        assert_eq!(
            check_expr(&e2, &env),
            Ok(TResult(Box::new(TAny), Box::new(TInteger)))
        );
    }

    #[test]
    fn check_just_integer() {
        let env = Environment::new();
        let e1 = CInt(5);
        let e2 = CJust(Box::new(e1));

        assert_eq!(check_expr(&e2, &env), Ok(TMaybe(Box::new(TInteger))))
    }

    #[test]
    fn check_is_error_result_positive() {
        let env = Environment::new();
        let e1 = CTrue;
        let e2 = COk(Box::new(e1));
        let e3 = IsError(Box::new(e2));

        assert_eq!(check_expr(&e3, &env), Ok(TBool));
    }

    #[test]
    fn check_is_error_result_error() {
        let env = Environment::new();
        let e1 = CTrue;
        let e2 = IsError(Box::new(e1));

        assert!(
            matches!(check_expr(&e2, &env), Err(_)),
            "Expecting a result type value."
        );
    }

    #[test]
    fn check_nothing() {
        let env = Environment::new();

        assert_eq!(check_expr(&CNothing, &env), Ok(TMaybe(Box::new(TAny))));
    }

    #[test]
    fn check_is_nothing_on_maybe() {
        let env = Environment::new();
        let e1 = CInt(5);
        let e2 = CJust(Box::new(e1));
        let e3 = IsNothing(Box::new(e2));

        assert_eq!(check_expr(&e3, &env), Ok(TBool));
    }

    #[test]
    fn check_is_nothing_type_error() {
        let env = Environment::new();
        let e1 = CInt(5);
        let e2 = IsNothing(Box::new(e1));

        assert!(
            matches!(check_expr(&e2, &env), Err(_)),
            "expecting a maybe type value."
        );
    }

    #[test]
    fn check_unwrap_maybe() {
        let env = Environment::new();
        let e1 = CInt(5);
        let e2 = CJust(Box::new(e1));
        let e3 = Unwrap(Box::new(e2));

        assert_eq!(check_expr(&e3, &env), Ok(TInteger));
    }

    #[test]
    fn check_unwrap_maybe_type_error() {
        let env = Environment::new();
        let e1 = CInt(5);
        let e2 = Unwrap(Box::new(e1));

        assert!(
            matches!(check_expr(&e2, &env), Err(_)),
            "expecting a maybe or result type value."
        );
    }

    #[test]
    fn check_unwrap_result() {
        let env = Environment::new();
        let e1 = CTrue;
        let e2 = COk(Box::new(e1));
        let e3 = Unwrap(Box::new(e2));

        assert_eq!(check_expr(&e3, &env), Ok(TBool));
    }

    #[test]
    fn check_propagate_maybe() {
        let env = Environment::new();
        let e1 = CInt(5);
        let e2 = CJust(Box::new(e1));
        let e3 = Propagate(Box::new(e2));

        assert_eq!(check_expr(&e3, &env), Ok(TInteger));
    }

    #[test]
    fn check_propagate_maybe_type_error() {
        let env = Environment::new();
        let e1 = CInt(5);
        let e2 = Propagate(Box::new(e1));

        assert!(
            matches!(check_expr(&e2, &env), Err(_)),
            "expecting a maybe or result type value."
        );
    }

    #[test]
    fn check_propagate_result() {
        let env = Environment::new();
        let e1 = CTrue;
        let e2 = COk(Box::new(e1));
        let e3 = Propagate(Box::new(e2));

        assert_eq!(check_expr(&e3, &env), Ok(TBool));
    }

    #[test]
    fn test_undefined_variable() {
        let env = Environment::new();
        let exp = Expression::Var("x".to_string());

        // Should fail - x is not defined
        assert!(check_expr(&exp, &env).is_err());
    }

    #[test]
    fn test_defined_variable() {
        let mut env = Environment::new();
        env.map_variable("x".to_string(), true, Type::TInteger);
        let exp = Expression::Var("x".to_string());

        // Should succeed and return integer type
        assert_eq!(check_expr(&exp, &env), Ok(Type::TInteger));
    }

    #[test]
    fn test_adt_constructor_valid() {
        let mut env = Environment::new();
        let figure_type = HashMap::from([
            ("Circle".to_string(), vec![Type::TInteger]),
            ("Rectangle".to_string(), vec![Type::TInteger, Type::TInteger]),
        ]);
        env.map_adt("Figure".to_string(), figure_type);

        let circle = Constructor("Circle".to_string(), vec![Box::new(CInt(5))]);
        let result = check_expr(&circle, &env);
        assert!(result.is_ok());
    }

    #[test]
    fn test_adt_constructor_wrong_args() {
        let mut env = Environment::new();
        let figure_type = HashMap::from([
            ("Circle".to_string(), vec![Type::TInteger]),
            ("Rectangle".to_string(), vec![Type::TInteger, Type::TInteger]),
        ]);
        env.map_adt("Figure".to_string(), figure_type);

        let circle = Constructor(
            "Circle".to_string(),
            vec![Box::new(CString("invalid".to_string()))],
        );
        let result = check_expr(&circle, &env);
        assert!(result.is_err());
    }

    #[test]
    fn test_adt_constructor_wrong_count() {
        let mut env = Environment::new();
        let figure_type = HashMap::from([
            ("Circle".to_string(), vec![Type::TInteger]),
            ("Rectangle".to_string(), vec![Type::TInteger, Type::TInteger]),
        ]);
        env.map_adt("Figure".to_string(), figure_type);

        let rectangle = Constructor("Rectangle".to_string(), vec![Box::new(CInt(5))]); // Missing second argument
        let result = check_expr(&rectangle, &env);
        assert!(result.is_err());
    }

    #[test]
    fn test_adt_constructor_undefined() {
        let env = Environment::new();
        let circle = Constructor("Circle".to_string(), vec![Box::new(CInt(5))]);
        let result = check_expr(&circle, &env);
        assert!(result.is_err());
    }

    #[test]
    fn test_adt_constructor_with_mutable_vars() {
        let mut env = Environment::new();
        let figure_type = HashMap::from([
            ("Circle".to_string(), vec![Type::TInteger]),
            ("Rectangle".to_string(), vec![Type::TInteger, Type::TInteger]),
        ]);
        env.map_adt("Figure".to_string(), figure_type);

        // Create a mutable variable to use in constructor
        env.map_variable("radius".to_string(), true, Type::TInteger);

        let circle = Constructor(
            "Circle".to_string(),
            vec![Box::new(Var("radius".to_string()))],
        );
        let result = check_expr(&circle, &env);
        assert!(result.is_ok());
    }

    #[test]
    fn test_match_expression_valid() {
        let mut env = Environment::new();
        let figure_type = HashMap::from([
            ("Circle".to_string(), vec![Type::TInteger]),
            ("Rectangle".to_string(), vec![Type::TInteger, Type::TInteger]),
        ]);
        env.map_adt("Figure".to_string(), figure_type);

        let circle = Constructor("Circle".to_string(), vec![Box::new(CInt(5))]);
        let arms = vec![
            (("Circle".to_string(), vec!["radius".to_string()]), CInt(10)),
            (("Rectangle".to_string(), vec!["width".to_string(), "height".to_string()]), CInt(20)),
        ];
        let match_expr = Match(Box::new(circle), arms);
        
        let result = check_expr(&match_expr, &env);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), TInteger);
    }

    #[test]
    fn test_match_expression_non_adt() {
        let env = Environment::new();
        let arms = vec![
            (("Circle".to_string(), vec!["radius".to_string()]), CInt(10)),
        ];
        let match_expr = Match(Box::new(CInt(5)), arms);
        
        let result = check_expr(&match_expr, &env);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("must be an algebraic data type"));
    }

    #[test]
    fn test_match_expression_undefined_constructor() {
        let mut env = Environment::new();
        let figure_type = HashMap::from([
            ("Circle".to_string(), vec![Type::TInteger]),
        ]);
        env.map_adt("Figure".to_string(), figure_type);

        let circle = Constructor("Circle".to_string(), vec![Box::new(CInt(5))]);
        let arms = vec![
            (("Triangle".to_string(), vec!["side".to_string()]), CInt(10)), // Triangle não existe
        ];
        let match_expr = Match(Box::new(circle), arms);
        
        let result = check_expr(&match_expr, &env);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("is not defined in ADT"));
    }

    #[test]
    fn test_match_expression_wrong_arg_count() {
        let mut env = Environment::new();
        let figure_type = HashMap::from([
            ("Circle".to_string(), vec![Type::TInteger]),
            ("Rectangle".to_string(), vec![Type::TInteger, Type::TInteger]),
        ]);
        env.map_adt("Figure".to_string(), figure_type);

        let circle = Constructor("Circle".to_string(), vec![Box::new(CInt(5))]);
        let arms = vec![
            (("Circle".to_string(), vec!["radius".to_string(), "extra".to_string()]), CInt(10)), // Circle só tem 1 arg
        ];
        let match_expr = Match(Box::new(circle), arms);
        
        let result = check_expr(&match_expr, &env);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("expects 1 arguments, but got 2"));
    }

    #[test]
    fn test_match_expression_different_arm_types() {
        let mut env = Environment::new();
        let figure_type = HashMap::from([
            ("Circle".to_string(), vec![Type::TInteger]),
            ("Rectangle".to_string(), vec![Type::TInteger, Type::TInteger]),
        ]);
        env.map_adt("Figure".to_string(), figure_type);

        let circle = Constructor("Circle".to_string(), vec![Box::new(CInt(5))]);
        let arms = vec![
            (("Circle".to_string(), vec!["radius".to_string()]), CInt(10)),
            (("Rectangle".to_string(), vec!["width".to_string(), "height".to_string()]), CTrue), // Diferente tipo
        ];
        let match_expr = Match(Box::new(circle), arms);
        
        let result = check_expr(&match_expr, &env);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("must evaluate to the same type"));
    }

    #[test]
    fn test_match_expression_empty_arms() {
        let mut env = Environment::new();
        let figure_type = HashMap::from([
            ("Circle".to_string(), vec![Type::TInteger]),
        ]);
        env.map_adt("Figure".to_string(), figure_type);

        let circle = Constructor("Circle".to_string(), vec![Box::new(CInt(5))]);
        let arms = vec![]; // Sem braços
        let match_expr = Match(Box::new(circle), arms);
        
        let result = check_expr(&match_expr, &env);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("No arms in match"));
    }

    #[test]
    fn test_match_expression_with_variables() {
        let mut env = Environment::new();
        let figure_type = HashMap::from([
            ("Circle".to_string(), vec![Type::TInteger]),
            ("Rectangle".to_string(), vec![Type::TInteger, Type::TInteger]),
        ]);
        env.map_adt("Figure".to_string(), figure_type);

        let circle = Constructor("Circle".to_string(), vec![Box::new(CInt(5))]);
        let arms = vec![
            (("Circle".to_string(), vec!["radius".to_string()]), Var("radius".to_string())), // Usa a variável do padrão
        ];
        let match_expr = Match(Box::new(circle), arms);
        
        let result = check_expr(&match_expr, &env);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), TInteger);
    }
}
