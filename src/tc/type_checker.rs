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
        Expression::DictMerge(dict_expr, new_entries) => {
            check_dictionary_merge_expression(*dict_expr, new_entries, env)
        }
        Expression::In(_, collection_expr) => check_membership_expression(collection_expr, env),
        Expression::NotIn(_, collection_expr) => check_membership_expression(collection_expr, env),
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

/// Attempts to extract a key (Name) from an expression.
/// Only accepts CString or Var.
fn extract_key(exp: &Expression) -> Option<Name> {
    match exp {
        Expression::CString(s) => Some(s.clone()),
        Expression::Var(s) => Some(s.clone()),
        _ => None,
    }
}

/// Updated dictionary type checking: keys are now Expressions.
/// Only accepts keys that are either a CString or a Var.
fn check_dictionary(
    dict_entries: Vec<(Expression, Box<Expression>)>,
    env: &Environment,
) -> Result<Type, ErrorMessage> {
    let mut dict_entries_type: Vec<(Name, Type)> = Vec::new();

    for (key_expr, value_exp) in dict_entries {
        // Convert key expression to Name (only CString and Var are allowed)
        let key = match key_expr {
            Expression::CString(s) => s,
            Expression::Var(s) => s,
            _ => {
                return Err(String::from(
                    "[Type Error] dictionary key must be a string literal or variable.",
                ))
            }
        };
        let value_type = check(*value_exp, env)?;
        dict_entries_type.push((key, value_type));
    }
    Ok(Type::TDict(dict_entries_type))
}

/// Updated dictionary merge type checking.
/// The dictionary name is now an Expression (must be CString or Var).
fn check_dictionary_merge_expression(
    dict_expr: Expression,
    new_entries: Vec<(Expression, Box<Expression>)>,
    env: &Environment,
) -> Result<Type, ErrorMessage> {
    // Extract dictionary name from the expression
    let dict_name = match dict_expr {
        Expression::Var(s) => s,
        Expression::CString(s) => s,
        _ => {
            return Err(String::from(
                "[Type Error] invalid dictionary name in merge.",
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

    // Merge the old and new dictionary types
    let mut merged_types: HashMap<Name, Type> = old_entries.clone().into_iter().collect();
    for (key, t) in new_entries_checked {
        merged_types.insert(key, t);
    }

    let new_dict_type: Vec<(Name, Type)> = merged_types.into_iter().collect();
    Ok(Type::TDict(new_dict_type))
}

/// Updated membership expression type checking.
/// The collection expression is now converted to a Name (only Var or CString allowed).
fn check_membership_expression(
    collection_expr: Expression,
    env: &Environment,
) -> Result<Type, ErrorMessage> {
    let collection_name = match collection_expr {
        Expression::Var(s) => s,
        Expression::CString(s) => s,
        _ => {
            return Err(String::from(
                "[Type Error] invalid collection name in membership expression",
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
                CString(String::from("x")),
                Box::new(Add(
                    Box::new(Add(Box::new(CInt(10)), Box::new(CInt(12)))),
                    Box::new(CInt(20)),
                )),
            ),
            (
                CString(String::from("y")),
                Box::new(Or(
                    Box::new(And(Box::new(CTrue), Box::new(CFalse))),
                    Box::new(CTrue),
                )),
            ),
            (CString(String::from("z")), Box::new(CReal(50.0))),
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
            Expression::Var(String::from("dict")),
            vec![
                (CString(String::from("y")), Box::new(CTrue)),
                (CString(String::from("z")), Box::new(CInt(30))),
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
        let in_exp = In(CString(String::from("key")), Expression::Var(String::from("dict")));
        assert_eq!(check(in_exp, &env), Ok(TBool));
    }

    #[test]
    fn check_not_in_expression() {
        let env = HashMap::from([(String::from("dict"), TDict(vec![]))]);
        let not_in_exp = NotIn(CString(String::from("key")), Expression::Var(String::from("dict")));
        assert_eq!(check(not_in_exp, &env), Ok(TBool));
    }

    #[test]
    fn check_type_error_in_expression() {
        let env = HashMap::from([(String::from("var"), TInteger)]);
        let in_exp = In(String::from("key"), String::from("var"));

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
        let not_in_exp = NotIn(String::from("key"), String::from("var"));

        assert_eq!(
            check(not_in_exp, &env),
            Err(String::from(
                "[Type Error] expecting a collection type on the right side."
            ))
        );
    }
}
