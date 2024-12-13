use std::collections::HashMap;

use crate::ir::ast::{Expression, Expression::*, Name, Type, Type::*};

type ErrorMessage = String;

type Environment = HashMap<Name, Type>;

pub fn check(exp: Expression, env: &Environment) -> Result<Type, ErrorMessage> {
    match exp {
        CTrue => Ok(TBool),
        CFalse => Ok(TBool),
        CInt(_) => Ok(TInteger),
        CReal(_) => Ok(TReal),
        CString(_) => Ok(TString),
        Add(l, r) => check_bin_arithmetic_expression(*l, *r, env),
        Sub(l, r) => check_bin_arithmetic_expression(*l, *r, env),
        Mul(l, r) => check_bin_arithmetic_expression(*l, *r, env),
        Div(l, r) => check_bin_arithmetic_expression(*l, *r, env),
        And(l, r) => check_bin_boolean_expression(*l, *r, env),
        Or(l, r) => check_bin_boolean_expression(*l, *r, env),
        Not(e) => check_not_expression(*e, env),
        EQ(l, r) => check_bin_relational_expression(*l, *r, env),
        GT(l, r) => check_bin_relational_expression(*l, *r, env),
        LT(l, r) => check_bin_relational_expression(*l, *r, env),
        GTE(l, r) => check_bin_relational_expression(*l, *r, env),
        LTE(l, r) => check_bin_boolean_expression(*l, *r, env),
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
        (TInteger, TInteger) => Ok(TInteger),
        (TInteger, TReal) => Ok(TReal),
        (TReal, TInteger) => Ok(TReal),
        (TReal, TReal) => Ok(TReal),
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
        (TBool, TBool) => Ok(TBool),
        _ => Err(String::from("[Type Error] expecting boolean type values.")),
    }
}

fn check_not_expression(exp: Expression, env: &Environment) -> Result<Type, ErrorMessage> {
    let exp_type = check(exp, env)?;

    match exp_type {
        TBool => Ok(TBool),
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
        (TInteger, TInteger) => Ok(TBool),
        (TInteger, TReal) => Ok(TBool),
        (TReal, TInteger) => Ok(TBool),
        (TReal, TReal) => Ok(TBool),
        _ => Err(String::from("[Type Error] expecting numeric type values.")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
