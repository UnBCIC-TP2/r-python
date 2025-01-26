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
        
        Expression::List(elements,type_list)=>
        check_create_list(elements,type_list, env),

        Expression::Push(list,elem)=>
        check_push_list(*list,*elem,env),

        Expression::Pop(list)=>
        check_pop_list(*list,env),

        Expression::Get(list,index ) =>
        check_get_list(*list,*index,env),

        Expression::Len(list) =>
        check_len_list(*list,env),


        _ => Err(String::from("not implemented yet")),
    }
}

fn check_create_list(
    maybe_list: Option<Vec<Expression>>,
    type_list: Option<Box<Expression>>,
    env: &Environment,) 
-> Result<Type,ErrorMessage>{
    match maybe_list{
        Some(vec)=>{
            if vec.is_empty(){
                if let Some(expected_type_box) = type_list{
                    let expected_type = check(*expected_type_box,&env)?;
                    return Ok(Type::TList(Box::new(expected_type)));
                }
                else{
                    return Err(String::from("Can't create empty list without type"))
                }
            }

            let first_type = check(vec[0].clone(),env)?;

            for item in vec.iter(){
                let item_type = check(item.clone(),env)?;
                if item_type != first_type{
                    return Err(String::from("[Type error] Differents types in list"));
                }
            }

            if let Some(expected_type_box) = type_list{
                let expected_type = check(*expected_type_box,&env)?;
                if expected_type != first_type{
                    return Err(format!(
                        "Type {:?} does not match type {:?}",
                        expected_type, first_type
                    ));
                }
            }

            Ok(Type::TList(Box::new(first_type)))
        }
        None=>{
            Err(String::from("[Type error] expected list as argument"))
        }
    }
}

fn check_push_list(list: Expression, elem: Expression ,env: &Environment)
-> Result<Type,ErrorMessage>{
    let list_type = check(list,env)?;
    let elem_type = check(elem,env)?;
    match list_type {
        Type::TList(boxed_type) if *boxed_type == elem_type => Ok(Type::TBool),
        _ => Err(String::from("[Type error] element type does not match list type")),
    }
}

fn check_pop_list(list: Expression, env: &Environment)
->Result<Type,ErrorMessage>{
    let list_type = check(list,env)?;

    match list_type{
        Type::TList(boxed_type) => {
            match *boxed_type{
                Type::TInteger | Type::TReal | Type::TString => {
                    Ok(*boxed_type)
                },
                _=> Err(String::from("cannot pop from a empty list"))
            }
        }
        _ => Err(String::from("[Type error] cannot pop from a non-list type"))
    }
}

fn check_get_list(list: Expression, index: Expression,env: &Environment)
->Result<Type,ErrorMessage>{
    let list_type = check(list,env)?;
    let index_type = check(index,env)?;
    match(list_type,index_type){
        (Type::TList(boxed_type),Type::TInteger)=>{
            Ok(*boxed_type)
        }
        _=> Err(String::from("[Type error] index must be integer"))
    }
}

fn check_len_list(list:Expression, env: &Environment)->
Result<Type, String>{
    let list_type = check(list,env)?;
    match list_type{
        Type::TList(_)=>{
            Ok(Type::TInteger)
        }
        _=>Err(String::from("[Type error] first argument must be a list"))
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
    use std::hash::Hash;

    use super::*;

    use crate::ir::ast::Expression::*;
    use crate::ir::ast::Type::*;

    #[test]
    fn check_create_valid_list() {
        let env = HashMap::new();
        let type_list = Box::new(Expression::CInt(0));
        let elements = Some(vec![CInt(1), CInt(2), CInt(3)]);
        let list = List(elements,Some(type_list));

        assert_eq!(check(list, &env), Ok(TList(Box::new(TInteger))));
    }

    #[test]
    fn check_create_inconsistent_list() {
        let env = HashMap::new();
        let type_list = Box::new(Expression::CInt(0));
        let elements = Some(vec![CInt(1), CReal(2.0)]);
        let list = List(elements,Some(type_list));
    
        assert_eq!(
            check(list, &env),
            Err(String::from("[Type error] Differents types in list"))
        ); 
    }

    #[test]
    fn check_create_none_type_list(){
        let env = HashMap::new();
        let list = List(Some(vec![]),None);
        assert!(check(list,&env).is_err());
    }

    #[test]
    fn check_push_valid_elements_in_list(){
        let env = HashMap::new();
        let type_list = Box::new(Expression::CInt(0));
        let list = List(Some(vec![]),Some(type_list));
        let elem = Expression::CInt(5);
        let push = Expression::Push(Box::new(list),Box::new(elem));
        
        assert!(check(push,&env).is_ok());
    }

    #[test]
    fn check_pop_valid(){
        let env = HashMap::new();
        let type_list = Box::new(Expression::CInt(0));
        let list = List(Some(vec![Expression::CInt(5)]),Some(type_list));
        let pop = Expression::Pop(Box::new(list));
        assert!(check(pop,&env).is_ok());
    }

    #[test]
    fn check_pop_in_non_list(){
        let env = HashMap::new();
        let type_list = Box::new(Expression::CInt(0));
        let list = List(None,Some(type_list));
        let pop = Expression::Pop(Box::new(list));
        assert!(check(pop,&env).is_err());
    }

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
