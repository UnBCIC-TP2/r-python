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

        Expression::Tuple(elements) => 
        check_create_tuple(elements, env),   

        Expression::AddTuple(tuple, new_element) => 
        check_add_tuple(*tuple, *new_element, env),
        
        Expression::LengthTuple(tuple) => 
        check_length_tuple(*tuple, env),

        Expression::GetTuple(tuple, index) => 
        check_get_tuple(*tuple, *index, env),

        Expression::List(elements,type_list)=>
        check_create_list(elements,type_list, env),

        Expression::Append(list,elem)=>
        check_append_list(*list,*elem,env),

        Expression::Pop(list)=>
        check_pop_list(*list,env),

        Expression::Get(list,index ) =>
        check_get_list(*list,*index,env),

        Expression::Len(list) =>
        check_len_list(*list,env),

        Expression::Dict(elements)=> 
        check_dict_creation(elements, env),

        Expression::GetDict(dict, key)=> 
        check_dict_get(*dict, *key, env),

        Expression::SetDict(dict, key, value)=> 
        check_dict_set(*dict, *key, *value, env),

        Expression::RemoveDict(dict, key)=> 
        check_dict_remove(*dict, *key, env),



        _ => Err(String::from("not implemented yet")),
    }
}
fn check_dict_creation(
    elements: Option<Vec<(Expression, Expression)>>,
    env: &Environment,
) -> Result<Type, ErrorMessage> {
    if let Some(pairs) = elements {
        let mut key_type = None;
        let mut value_type = None;

        for (key, value) in pairs {
            let current_key_type = check(key, env)?;
            let current_value_type = check(value, env)?;

            if let Some(expected_key_type) = &key_type {
                if *expected_key_type != current_key_type {
                    return Err(String::from("[Type Error] inconsistent key types in dictionary."));
                }
            } else {
                key_type = Some(current_key_type);
            }

            if let Some(expected_value_type) = &value_type {
                if *expected_value_type != current_value_type {
                    return Err(String::from("[Type Error] inconsistent value types in dictionary."));
                }
            } else {
                value_type = Some(current_value_type);
            }
        }

        Ok(Type::TDict(
            Box::new(key_type.unwrap()),
            Box::new(value_type.unwrap()),
        ))
    } else {
        Err(String::from("[Type Error] dictionary cannot be empty."))
    }
}
//GetDict
fn check_dict_get(
    dict: Expression,
    key: Expression,
    env: &Environment,
) -> Result<Type, ErrorMessage> {
    let dict_type = check(dict, env)?;
    let key_type = check(key, env)?;

    if let Type::TDict(boxed_key_type, boxed_value_type) = dict_type {
        if *boxed_key_type == key_type {
            Ok(*boxed_value_type)
        } else {
            Err(String::from("[Type Error] key type does not match dictionary key type."))
        }
    } else {
        Err(String::from("[Type Error] expected a dictionary type."))
    }
}
//SetDict
fn check_dict_set(
    dict: Expression,
    key: Expression,
    value: Expression,
    env: &Environment,
) -> Result<Type, ErrorMessage> {
    let dict_type = check(dict, env)?;
    let key_type = check(key, env)?;
    let value_type = check(value, env)?;

    if let Type::TDict(boxed_key_type, boxed_value_type) = dict_type {
        if *boxed_key_type == key_type && *boxed_value_type == value_type {
            Ok(Type::TDict(boxed_key_type, boxed_value_type))
        } else {
            Err(String::from(
                "[Type Error] key or value type does not match dictionary type.",
            ))
        }
    } else {
        Err(String::from("[Type Error] expected a dictionary type."))
    }
}
//RemoveDict
fn check_dict_remove(
    dict: Expression,
    key: Expression,
    env: &Environment,
) -> Result<Type, ErrorMessage> {
    let dict_type = check(dict, env)?;
    let key_type = check(key, env)?;

    if let Type::TDict(ref boxed_key_type, _) = dict_type {
        if boxed_key_type.as_ref() == &key_type {
            Ok(dict_type)
        } else {
            Err(String::from("[Type Error] key type does not match dictionary key type."))
        }
    } else {
        Err(String::from("[Type Error] expected a dictionary type."))
    }
}

fn check_create_tuple(
    maybe_tuple: Vec<Expression>,
    env: &Environment,
) -> Result<Type, ErrorMessage> {
    match maybe_tuple {
        vec => {
            // Verifica o tipo do primeiro elemento da tupla
            let first_type = check(vec[0].clone(), env)?;

            // Verifica se todos os elementos têm o mesmo tipo
            for item in vec.iter() {
                let item_type = check(item.clone(), env)?;
                if item_type != first_type {
                    return Err(String::from("[Type error] Different types in tuple"));
                }
            }

            // Retorna a tupla com o tipo homogêneo
            Ok(Type::TTuple(Box::new(first_type)))
        }
    }
}

fn check_add_tuple(
    tuple_expr: Expression,
    new_element: Expression,
    env: &Environment,
) -> Result<Type, ErrorMessage> {
    let new_element_type = check(new_element, env)?;

    match tuple_expr {
        Expression::Tuple(ref elements) => {
            if elements.is_empty() {
                return Ok(Type::TTuple(Box::new(new_element_type)));
            }

            let first_element_type = check(elements[0].clone(), env)?;

            if new_element_type != first_element_type {
                return Err(format!(
                    "Type {:?} does not match type {:?}",
                    new_element_type, first_element_type
                ));
            }

            let tuple_type = Type::TTuple(Box::new(first_element_type));
            Ok(tuple_type)
        }
        _ => {
            return Err("Provided expression is not a tuple".to_string());
        }
    }
}

fn check_length_tuple(
    tuple_expr: Expression,
    env: &Environment,
) -> Result<Type, ErrorMessage> {
    let tuple_type = check(tuple_expr.clone(), env)?;
    
    match tuple_type {
        Type::TTuple(_inner_type) => {
            Ok(Type::TInteger)
        }
        _ => Err(String::from("[Type error] Argument must be a tuple")),
    }
}

fn check_get_tuple(
    tuple_expr: Expression,
    index_expr: Expression,
    env: &Environment,
) -> Result<Type, ErrorMessage> {
    // Verifica o tipo da tupla
    let tuple_type = check(tuple_expr.clone(), env)?;
    match tuple_type {
        Type::TTuple(inner_type) => {
            // Verifica o tipo do índice
            let index_type = check(index_expr.clone(), env)?;
            match index_type {
                Type::TInteger => {
                    // Índice é válido, retorna o tipo dos elementos da tupla
                    Ok(*inner_type)
                }
                _ => Err(String::from("[Type error] Index must be an integer")),
            }
        }
        _ => Err(String::from("[Type error] First argument must be a tuple")),
    }
}

fn check_create_list(
    maybe_list: Vec<Expression>,
    type_list: Box<Expression>,
    env: &Environment,) 
-> Result<Type,ErrorMessage>{
    match maybe_list{
        vec=>{
            if vec.is_empty(){
                match *type_list{
                    Expression::CInt(_) |
                    Expression::CReal(_) |
                    Expression::CString(_) =>{
                        let expected_type = check(*type_list,&env)?;
                        return Ok(Type::TList(Box::new(expected_type)))
                    }
                    _=>{
                        return Err(String::from("Can't create empty list without type"))
                    }
                }
            }

            let first_type = check(vec[0].clone(),env)?;

            for item in vec.iter(){
                let item_type = check(item.clone(),env)?;
                if item_type != first_type{
                    return Err(String::from("[Type error] Differents types in list"));
                }
            }

            let expected_type_box = type_list;
            let expected_type = check(*expected_type_box,&env)?;

            if expected_type != first_type{
                return Err(format!(
                    "Type {:?} does not match type {:?}",
                    expected_type, first_type
                ));
            }
            Ok(Type::TList(Box::new(first_type)))
        }
        
        _=>{
            Err(String::from("[Type error] expected list as argument"))
        }
    }
}

fn check_append_list(list: Expression, elem: Expression ,env: &Environment)
-> Result<Type,ErrorMessage>{
    let list_type = check(list,env)?;
    let elem_type = check(elem,env)?;
    match (list_type,elem_type) {
        (Type::TList(boxed_type),Type::TList(boxed_type2))=>{
            if *boxed_type == *boxed_type2{
                Ok(Type::TBool)
            }
            else{
                Err(String::from("[Type error] Both lists may have same type"))
            }
        } 
        (Type::TList(boxed_type),type_elem)=>{
            if *boxed_type == type_elem{
                Ok(Type::TBool)
            }
            else{
                Err(String::from("[Type error] element type does not match list type"))
            }
        }
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
                _=> Err(String::from("cannot pop from an non-typed list"))
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

    //Teste de criação do dicionário (check_dict_creation)
    #[test]
    fn test_check_dict_creation() {
        let env = HashMap::new();
    
        let result = check_dict_creation(Some(vec![
            (Expression::CString("key1".to_string()), Expression::CInt(1)),
            (Expression::CString("key2".to_string()), Expression::CInt(2)),
        ]), &env);
    
        assert!(result.is_ok());
        if let Ok(typ) = result {
            assert_eq!(typ, Type::TDict(Box::new(Type::TString), Box::new(Type::TInteger)));
        }
    }
    //Teste de acesso a valor do dicionário (check_dict_get)
    #[test]
    fn test_check_dict_get() {
        let env = HashMap::new();
        
        let dict = Expression::Dict(Some(vec![
            (Expression::CString("key1".to_string()), Expression::CInt(1)),
        ]));

        let result = check_dict_get(dict, Expression::CString("key1".to_string()), &env);

        assert!(result.is_ok());
        if let Ok(typ) = result {
            assert_eq!(typ, Type::TInteger);
        }
    }
    //Teste de alteração ou inserção no dicionário (check_dict_set)
    #[test]
    fn test_check_dict_set() {
        let env = HashMap::new();
        
        let dict = Expression::Dict(Some(vec![
            (Expression::CString("key1".to_string()), Expression::CInt(1)),
        ]));
    
        let result = check_dict_set(dict, Expression::CString("key2".to_string()), Expression::CInt(2), &env);
    
        assert!(result.is_ok());
        if let Ok(typ) = result {
            assert_eq!(typ, Type::TDict(Box::new(Type::TString), Box::new(Type::TInteger)));
        }
    }
    //Teste de remoção de valor do dicionário (check_dict_remove)
    #[test]
    fn test_check_dict_remove() {
        let env = HashMap::new();

        let dict = Expression::Dict(Some(vec![
            (Expression::CString("key1".to_string()), Expression::CInt(1)),
            (Expression::CString("key2".to_string()), Expression::CInt(2)),
        ]));

        let result = check_dict_remove(dict, Expression::CString("key1".to_string()), &env);

        assert!(result.is_ok());
        if let Ok(typ) = result {
            assert_eq!(typ, Type::TDict(Box::new(Type::TString), Box::new(Type::TInteger)));
        }
    }

    #[test]
    fn check_create_valid_tuple(){
        let env = HashMap::new();
        let tuple = Tuple(vec![CInt(1), CInt(2), CInt(3)]);
        assert_eq!(check(tuple, &env), Ok(TTuple(Box::new(TInteger))));
    }

    #[test]
    fn check_create_invalid_tuple(){
        let env = HashMap::new();
        let tuple = Tuple(vec![CInt(1), CString("Rust".to_string()), CInt(3)]);
        assert_eq!(check(tuple, &env), 
        Err(String::from("[Type error] Different types in tuple")));
    }

    #[test]
    fn check_create_valid_list() {
        let env = HashMap::new();
        let type_list = Box::new(Expression::CInt(0));
        let elements = vec![CInt(1), CInt(2), CInt(3)];
        let list = List(elements,type_list);

        assert_eq!(check(list, &env), Ok(TList(Box::new(TInteger))));
    }

    #[test]
    fn check_create_inconsistent_list() {
        let env = HashMap::new();
        let type_list = Box::new(Expression::CInt(0));
        let elements = vec![CInt(1), CReal(2.0)];
        let list = List(elements,type_list);
    
        assert_eq!(
            check(list, &env),
            Err(String::from("[Type error] Differents types in list"))
        ); 
    }

    #[test]
    fn check_append_valid_elements_in_list(){
        let env = HashMap::new();
        let type_list = Box::new(Expression::CInt(0));
        let list = List(vec![],type_list);
        let elem = Expression::CInt(5);
        let push = Expression::Append(Box::new(list),Box::new(elem));
        
        assert!(check(push,&env).is_ok());
    }

    #[test]
    fn check_append_valid_list_with_list(){
        let env = HashMap::new();
        let type_list = Box::new(CInt(0));
        let list1 = List(vec![CInt(5)],type_list.clone());
        let list2 = List(vec![CInt(10)],type_list.clone());
        let list3 = Append(Box::new(list1),Box::new(list2));

        assert!(check(list3,&env).is_ok());
    }

    #[test]
    fn check_append_invalid_list_with_list(){
        let env = HashMap::new();
        let type_list = Box::new(CInt(0));
        let type_list2 = Box::new(CReal(0.0));
        let list1 = List(vec![CInt(5)],type_list.clone());
        let list2 = List(vec![CReal(10.5)],type_list2.clone());
        let list3 = Append(Box::new(list1),Box::new(list2));

        assert!(check(list3.clone(),&env).is_err());
        assert_eq!(
            check(list3, &env),
            Err(String::from("[Type error] Both lists may have same type"))
        )
    }

    #[test]
    fn check_append_valid_list_with_empty_list(){
        let env = HashMap::new();
        let type_list = Box::new(CInt(0));
        let list1 = List(vec![CInt(5)],type_list.clone());
        let list2 = List(vec![],type_list.clone());
        let list3 = Append(Box::new(list1),Box::new(list2));

        assert!(check(list3,&env).is_ok());
    }

    #[test]
    fn check_pop_valid(){
        let env = HashMap::new();
        let type_list = Box::new(Expression::CInt(0));
        let list = List(vec![Expression::CInt(5)],type_list);
        let pop = Expression::Pop(Box::new(list));
        assert!(check(pop,&env).is_ok());
    }

    #[test]
    fn check_get_element_tuple(){
        let env = HashMap::new();
        let idx = Expression::CInt(1);
        let tuple = Expression::Tuple(vec!
            [Expression::CInt(5),Expression::CInt(8)]);

        let elem = check(Expression::GetTuple(Box::new(tuple),Box::new(idx)),&env).unwrap();

        assert_eq!(elem,TInteger);
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
        let t_tuple1 = TTuple(Box::new(TInteger));
        let t_tuple2 = TTuple(Box::new(TInteger));

        assert_eq!(t_tuple1 == t_tuple2, true);
    }

    #[test]
    fn check_ttuple_comparison_different_types() {
        let t_tuple1 = TTuple(Box::new(TInteger));
        let t_tuple2 = TTuple(Box::new(TBool));

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