use std::collections::HashMap;

use crate::ir::ast::Expression;
use crate::ir::ast::Name;
use crate::ir::ast::Type;

type ErrorMessage = String;

type Environment = HashMap<Name, Type>;

pub fn check(exp: Expression, env: &Environment) -> Result<Type, ErrorMessage> {
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
        Expression::LTE(l, r) => check_bin_boolean_expression(*l, *r, env),
        Expression::Dict(entries) => check_dictionary(entries, env),
        Expression::DictMerge(var_exp, new_entries) => {
            check_dictionary_merge_expression(var_exp, new_entries, env)
        }
        Expression::In(_, var_exp) => check_membership_expression(var_exp, env),
        Expression::NotIn(_, var_exp) => check_membership_expression(var_exp, env),
        _ => Err(String::from("not implemented yet")),
    }
}

fn check_bin_arithmetic_expression(
    left: Expression,
    right: Expression,
    env: &Environment,
) -> Result<Type, ErrorMessage> {
    let left_type = check(left, env)?;
    let right_type = check(right, env)?;

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
    let left_type = check(left, env)?;
    let right_type = check(right, env)?;

    match (left_type, right_type) {
        (Type::TBool, Type::TBool) => Ok(Type::TBool),
        _ => Err(String::from("[Type Error] expecting boolean type values.")),
    }
}

/// Only accepts keys that are a CString.
fn check_dictionary(
    dict_entries: Vec<(Box<Expression>, Box<Expression>)>,
    env: &Environment,
) -> Result<Type, ErrorMessage> {
    let mut dict_entries_type: Vec<(Name, Type)> = Vec::new();

    for (key_expr, value_exp) in dict_entries {
        let key = match *key_expr {
            Expression::CString(s) => s,
            _ => {
                return Err(String::from(
                    "[Type Error] dictionary key must be a string literal.",
                ))
            }
        };
        let value_type = check(*value_exp, env)?;
        dict_entries_type.push((key, value_type));
    }
    Ok(Type::TDict(dict_entries_type))
}

fn check_dictionary_merge_expression(
    var_exp: Box<Expression>,
    new_entries: Vec<(Box<Expression>, Box<Expression>)>,
    env: &Environment,
) -> Result<Type, ErrorMessage> {
    let dict_name = match *var_exp {
        Expression::Var(s) => s,
        _ => {
            return Err(String::from(
                "[Type Error] expected a variable on the left side.",
            ))
        }
    };

    let old_entries = match env.get(&dict_name) {
        Some(Type::TDict(entries)) => entries,
        Some(_) => return Err(format!("[Type Error] '{}' is not a dictionary.", dict_name)),
        None => return Err(format!("[Type Error] '{}' is not defined.", dict_name)),
    };

    let new_entries_checked = match check_dictionary(new_entries, env) {
        Ok(Type::TDict(entries)) => entries,
        Err(msg) => return Err(msg),
        _ => unreachable!(),
    };

    let mut merged_types = old_entries.clone();

    for (key, value_type) in new_entries_checked.iter() {
        if let Some(index) = merged_types
            .iter()
            .position(|(existing_key, _)| **existing_key == **key)
        {
            merged_types[index] = (key.clone(), value_type.clone());
        } else {
            merged_types.push((key.clone(), value_type.clone()));
        }
    }

    Ok(Type::TDict(merged_types))
}

fn check_membership_expression(
    var_exp: Box<Expression>,
    env: &Environment,
) -> Result<Type, ErrorMessage> {
    let collection_name = match *var_exp {
        Expression::Var(s) => s,
        _ => {
            return Err(String::from(
                "[Type Error] expected a variable on the left side.",
            ))
        }
    };

    match env.get(&collection_name) {
        Some(Type::TDict(_)) => Ok(Type::TBool),
        _ => Err(String::from(
            "[Type Error] expecting a collection type on the right side.",
        )),
    }
}

fn check_not_expression(exp: Expression, env: &Environment) -> Result<Type, ErrorMessage> {
    let exp_type = check(exp, env)?;

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
    let left_type = check(left, env)?;
    let right_type = check(right, env)?;

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
    use super::*;

    use crate::ir::ast::Expression::*;
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
        assert_eq!(check(c10, &env), Ok(TInteger));
    }

    #[test]
    fn check_add_integers() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let c20 = CInt(20);
        let add = Add(Box::new(c10), Box::new(c20));

        assert_eq!(check(add, &env), Ok(TInteger));
    }

    #[test]
    fn check_add_reals() {
        let env = HashMap::new();
        let c10 = CReal(10.5);
        let c20 = CReal(20.3);
        let add = Add(Box::new(c10), Box::new(c20));

        assert_eq!(check(add, &env), Ok(TReal));
    }

    #[test]
    fn check_add_real_and_integer() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let c20 = CReal(20.3);
        let add = Add(Box::new(c10), Box::new(c20));

        assert_eq!(check(add, &env), Ok(TReal));
    }

    #[test]
    fn check_add_integer_and_real() {
        let env = HashMap::new();
        let c10 = CReal(10.5);
        let c20 = CInt(20);
        let add = Add(Box::new(c10), Box::new(c20));

        assert_eq!(check(add, &env), Ok(TReal));
    }

    #[test]
    fn check_type_error_arithmetic_expression() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let bool = CFalse;
        let add = Add(Box::new(c10), Box::new(bool));

        assert_eq!(
            check(add, &env),
            Err(String::from("[Type Error] expecting numeric type values."))
        );
    }

    #[test]
    fn check_type_error_not_expression() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let not = Not(Box::new(c10));

        assert_eq!(
            check(not, &env),
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
            check(and, &env),
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
            check(or, &env),
            Err(String::from("[Type Error] expecting boolean type values."))
        );
    }

    #[test]
    fn check_dictionary_expression() {
        /*
         * For:
         *  dict = { x: 10 + 12 + 20, y: true && false || true, z: 50.0 }
         *
         * Expected:
         *  TDict([(String::from("x"), TInteger), (String::from("y"), TBool), (String::from("z"), TReal)])
         */
        let env = HashMap::new();
        let dict = Dict(vec![
            (
                Box::new(CString(String::from("x"))),
                Box::new(Add(
                    Box::new(Add(Box::new(CInt(10)), Box::new(CInt(12)))),
                    Box::new(CInt(20)),
                )),
            ),
            (
                Box::new(CString(String::from("y"))),
                Box::new(Or(
                    Box::new(And(Box::new(CTrue), Box::new(CFalse))),
                    Box::new(CTrue),
                )),
            ),
            (Box::new(CString(String::from("z"))), Box::new(CReal(50.0))),
        ]);

        let dict_type = check(dict, &env);
        assert_eq!(
            dict_type,
            Ok(TDict(vec![
                (String::from("x"), TInteger),
                (String::from("y"), TBool),
                (String::from("z"), TReal)
            ]))
        );
    }

    #[test]
    fn check_dictionary_merge_expression() {
        /*
         * For:
         *  dict = { x: 10, y: 20 }
         *  new_dict = dict with { y: true, z: 30 }
         *
         * Expected:
         *  TDict([(String::from("x"), TInteger), (String::from("y"), TBool), (String::from("z"), TInteger)])
         */
        let env = HashMap::from([(
            String::from("dict"),
            TDict(vec![
                (String::from("x"), TInteger),
                (String::from("y"), TInteger),
            ]),
        )]);

        let merge_exp = DictMerge(
            // The dictionary name is now an Expression
            Box::new(Expression::Var(String::from("dict"))),
            vec![
                (Box::new(CString(String::from("y"))), Box::new(CTrue)),
                (Box::new(CString(String::from("z"))), Box::new(CInt(30))),
            ],
        );

        let dict_type = check(merge_exp, &env);
        assert_eq!(
            dict_type,
            Ok(TDict(vec![
                (String::from("x"), TInteger),
                (String::from("y"), TBool),
                (String::from("z"), TInteger)
            ]))
        );
    }

    #[test]
    fn check_in_expression() {
        let env = HashMap::from([(String::from("dict"), TDict(vec![]))]);
        let in_exp = In(
            Box::new(CString(String::from("key"))),
            Box::new(Expression::Var(String::from("dict"))),
        );
        assert_eq!(check(in_exp, &env), Ok(TBool));
    }

    #[test]
    fn check_not_in_expression() {
        let env = HashMap::from([(String::from("dict"), TDict(vec![]))]);
        let not_in_exp = NotIn(
            Box::new(CString(String::from("key"))),
            Box::new(Expression::Var(String::from("dict"))),
        );
        assert_eq!(check(not_in_exp, &env), Ok(TBool));
    }

    #[test]
    fn check_type_error_in_expression() {
        let env = HashMap::from([(String::from("var"), TInteger)]);
        let in_exp = In(
            Box::new(CString(String::from("key"))),
            Box::new(Var(String::from("var"))),
        );

        assert_eq!(
            check(in_exp, &env),
            Err(String::from(
                "[Type Error] expecting a collection type on the right side."
            ))
        );
    }

    #[test]
    fn check_type_error_not_in_expression() {
        let env = HashMap::from([(String::from("var"), TInteger)]);
        let not_in_exp = NotIn(
            Box::new(CString(String::from("key"))),
            Box::new(Var(String::from("var"))),
        );

        assert_eq!(
            check(not_in_exp, &env),
            Err(String::from(
                "[Type Error] expecting a collection type on the right side."
            ))
        );
    }
}
