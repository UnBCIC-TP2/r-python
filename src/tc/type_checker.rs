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

        Expression::Set(elements) =>
        check_create_set(elements,env),

        Expression::Dict(elements)=> 
        check_dict_creation(elements, env),
        Expression::GetDict(dict, key)=> 
        check_dict_get(*dict, *key, env),
        Expression::SetDict(dict, key, value)=> 
        check_dict_set(*dict, *key, *value, env),
        Expression::RemoveDict(dict, key)=> 
        check_dict_remove(*dict, *key, env),

        Expression::Hash(elements)=>
        check_hash_creation(elements, env),
        Expression::GetHash(hash, key)=>
        check_hash_get(*hash, *key, env),
        Expression::SetHash(hash, key, value)=>
        check_hash_set(*hash, *key, *value, env),
        Expression::RemoveHash(mut hash, key)=>
        check_hash_remove(&mut *hash, *key, env),
   
        Expression::Tuple(elements) => check_create_tuple(elements,env),
        Expression::List(elements,type_list)=>
        check_create_list(elements,type_list, env),
        Expression::Append(list,elem)=>
        check_append_list(*list,*elem,env),
        Expression::Pop(list)=>
        check_pop_list(*list,env),

        Expression::Get(data_structure,index ) =>
        check_get(*data_structure,*index,env),

        Expression::Len(data_structure) =>
        check_len(*data_structure,env),

        _ => Err(String::from("not implemented yet")),
    }
}

fn check_create_set(
    maybe_set: Vec<Expression>,
    env: &Environment,
) -> Result<Type, ErrorMessage> {
    match maybe_set {
        vec => {
            let first_type = check(vec[0].clone(), env)?;

            for item in vec.iter() {
                let item_type = check(item.clone(), env)?;
                if item_type != first_type {
                    return Err(String::from("[Type error] Different types in set"));
                }
            }
            Ok(Type::TSet(Box::new(first_type)))
        }
    }
}

fn check_dict_creation(elements: Option<Vec<(Expression, Expression)>>, env: &Environment) -> Result<Type, ErrorMessage> {
    match elements {
        Some(items) => {
            let mut key_type: Option<Type> = None;
            let mut value_type: Option<Type> = None;

            for (key, value) in items {
                let key_type_check = check(key, env)?;
                let value_type_check = check(value, env)?;

                if key_type.is_none() {
                    key_type = Some(key_type_check);
                } else if key_type != Some(key_type_check) {
                    return Err(String::from("Inconsistent key types in Dict"));
                }

                if value_type.is_none() {
                    value_type = Some(value_type_check);
                } else if value_type != Some(value_type_check) {
                    return Err(String::from("Inconsistent value types in Dict"));
                }
            }

            match (key_type, value_type) {
                (Some(k), Some(v)) => Ok(Type::TDict(Box::new(k), Box::new(v))),
                _ => Err(String::from("Dict must have consistent key and value types")),
            }
        },
        None => Err(String::from("Dict creation requires elements")),
    }
}

fn check_hash_creation(elements: Option<HashMap<Expression, Expression>>, env: &Environment) -> Result<Type, ErrorMessage> {
    match elements {
        Some(map) => {
            let mut key_type: Option<Type> = None;
            let mut value_type: Option<Type> = None;

            for (key, value) in map {
                let key_type_check = check(key, env)?;
                let value_type_check = check(value, env)?;

                if key_type.is_none() {
                    key_type = Some(key_type_check);
                } else if key_type != Some(key_type_check) {
                    return Err(String::from("Inconsistent key types in Hash"));
                }

                if value_type.is_none() {
                    value_type = Some(value_type_check);
                } else if value_type != Some(value_type_check) {
                    return Err(String::from("Inconsistent value types in Hash"));
                }
            }

            match (key_type, value_type) {
                (Some(k), Some(v)) => Ok(Type::THash(Box::new(k), Box::new(v))),
                _ => Err(String::from("Hash must have consistent key and value types")),
            }
        },
        None => Err(String::from("Hash creation requires elements")),
    }
}

fn check_dict_get(dict: Expression, key: Expression, _env: &Environment) -> Result<Type, String> {
    match dict {
        Expression::Dict(Some(map)) => {
            if let Some(value) = map.iter().find(|(k, _)| k == &key) {
                match value {
                    (Expression::CString(_), Expression::CInt(_)) => Ok(Type::TInteger),
                    (Expression::CString(_), Expression::CString(_)) => Ok(Type::TString),
                    (Expression::CString(_), Expression::CReal(_)) => Ok(Type::TReal),
                    (Expression::CString(_), Expression::CTrue | Expression::CFalse) => Ok(Type::TBool),
                    (Expression::CString(_), Expression::List(_, _)) => Ok(Type::TList(Box::new(Type::TInteger))),
                    (Expression::CString(_), Expression::Tuple(_)) => Ok(Type::TTuple(vec![Type::TInteger])),
                    (Expression::CString(_), Expression::Dict(_)) => Ok(Type::TDict(Box::new(Type::TInteger), Box::new(Type::TString))),
                    (Expression::CString(_), Expression::Hash(_)) => Ok(Type::THash(Box::new(Type::TInteger), Box::new(Type::TString))),
                    (Expression::CInt(_), Expression::CInt(_)) => Ok(Type::TInteger),
                    (Expression::CInt(_), Expression::CString(_)) => Ok(Type::TString),
                    (Expression::CInt(_), Expression::CReal(_)) => Ok(Type::TReal),
                    (Expression::CInt(_), Expression::CTrue | Expression::CFalse) => Ok(Type::TBool),
                    (Expression::CInt(_), Expression::List(_, _)) => Ok(Type::TList(Box::new(Type::TInteger))),
                    (Expression::CInt(_), Expression::Tuple(_)) => Ok(Type::TTuple(vec![Type::TInteger])),
                    (Expression::CInt(_), Expression::Dict(_)) => Ok(Type::TDict(Box::new(Type::TInteger), Box::new(Type::TString))),
                    (Expression::CInt(_), Expression::Hash(_)) => Ok(Type::THash(Box::new(Type::TInteger), Box::new(Type::TString))),
                    _ => Err(String::from("Incompatible value type in Dict")),
                }
            } else {
                Err(String::from("Key not found in Dict"))
            }
        },
        _ => Err(String::from("Expected a Dict expression")),
    }
}

fn check_hash_get(hash: Expression, key: Expression, _env: &Environment) -> Result<Type, String> {
    match hash {
        Expression::Hash(Some(map)) => {
            if let Some(value) = map.get(&key) {
                return match value {
                    Expression::CInt(_) => Ok(Type::TInteger),
                    Expression::CString(_) => Ok(Type::TString),
                    Expression::CReal(_) => Ok(Type::TReal),
                    Expression::CTrue | Expression::CFalse => Ok(Type::TBool),
                    Expression::List(_, _) => Ok(Type::TList(Box::new(Type::TInteger))),
                    Expression::Tuple(_) => Ok(Type::TTuple(vec![Type::TInteger])),
                    Expression::Dict(_) => Ok(Type::TDict(Box::new(Type::TInteger), Box::new(Type::TString))),
                    Expression::Hash(_) => Ok(Type::THash(Box::new(Type::TInteger), Box::new(Type::TString))),
                    _ => Err(String::from("Incompatible value type in Hash")),
                };
            }
            Err(String::from("Key not found in Hash"))
        },
        _ => Err(String::from("Expected a Hash expression")),
    }
}

fn check_dict_set(mut dict: Expression, key: Expression, value: Expression, _env: &Environment) -> Result<Type, String> {
    match dict {
        Expression::Dict(Some(ref mut map)) => {
            if let Some(existing_value) = map.iter_mut().find(|(k, _)| *k == key) {
                match (&existing_value.1, &value) {
                    (Expression::CString(_), Expression::CInt(_)) => {
                        existing_value.1 = value; 
                        Ok(Type::TInteger)
                    }
                    (Expression::CString(_), Expression::CString(_)) => {
                        existing_value.1 = value;
                        Ok(Type::TString)
                    }
                    (Expression::CString(_), Expression::CReal(_)) => {
                        existing_value.1 = value;
                        Ok(Type::TReal)
                    }
                    (Expression::CString(_), Expression::CTrue | Expression::CFalse) => {
                        existing_value.1 = value;
                        Ok(Type::TBool)
                    }
                    (Expression::CString(_), Expression::List(_, _)) => {
                        existing_value.1 = value;
                        Ok(Type::TList(Box::new(Type::TInteger)))
                    }
                    (Expression::CString(_), Expression::Tuple(_)) => {
                        existing_value.1 = value;
                        Ok(Type::TTuple(vec![Type::TInteger]))
                    }
                    (Expression::CString(_), Expression::Dict(_)) => {
                        existing_value.1 = value;
                        Ok(Type::TDict(Box::new(Type::TInteger), Box::new(Type::TString)))
                    }
                    (Expression::CString(_), Expression::Hash(_)) => {
                        existing_value.1 = value;
                        Ok(Type::THash(Box::new(Type::TInteger), Box::new(Type::TString)))
                    }
                    (Expression::CInt(_), Expression::CInt(_)) => {
                        existing_value.1 = value; 
                        Ok(Type::TInteger)
                    }
                    (Expression::CInt(_), Expression::CString(_)) => {
                        existing_value.1 = value;
                        Ok(Type::TString)
                    }
                    (Expression::CInt(_), Expression::CReal(_)) => {
                        existing_value.1 = value;
                        Ok(Type::TReal)
                    }
                    (Expression::CInt(_), Expression::CTrue | Expression::CFalse) => {
                        existing_value.1 = value;
                        Ok(Type::TBool)
                    }
                    (Expression::CInt(_), Expression::List(_, _)) => {
                        existing_value.1 = value;
                        Ok(Type::TList(Box::new(Type::TInteger)))
                    }
                    (Expression::CInt(_), Expression::Tuple(_)) => {
                        existing_value.1 = value;
                        Ok(Type::TTuple(vec![Type::TInteger]))
                    }
                    (Expression::CInt(_), Expression::Dict(_)) => {
                        existing_value.1 = value;
                        Ok(Type::TDict(Box::new(Type::TInteger), Box::new(Type::TString)))
                    }
                    (Expression::CInt(_), Expression::Hash(_)) => {
                        existing_value.1 = value;
                        Ok(Type::THash(Box::new(Type::TInteger), Box::new(Type::TString)))
                    }
                    _ => Err(String::from("Incompatible value type for Dict")),
                }
            } else {
                Err(String::from("Key not found in Dict"))
            }
        }
        _ => Err(String::from("Expected a Dict expression")),
    }
}

fn check_hash_set(hash: Expression, key: Expression, value: Expression, _env: &Environment) -> Result<Type, String> {
    match hash {
        Expression::Hash(Some(map)) => {
            if let Some(existing_value) = map.iter().find(|(k, _)| **k == key) {
                match (&existing_value.1, &value) {
                    (Expression::CInt(_), Expression::CInt(_)) => Ok(Type::TInteger),
                    (Expression::CString(_), Expression::CString(_)) => Ok(Type::TString),
                    (Expression::CReal(_), Expression::CReal(_)) => Ok(Type::TReal),
                    (Expression::CTrue | Expression::CFalse, Expression::CTrue | Expression::CFalse) => Ok(Type::TBool),
                    (Expression::List(_, _), Expression::List(_, _)) => Ok(Type::TList(Box::new(Type::TInteger))),
                    (Expression::Tuple(_), Expression::Tuple(_)) => Ok(Type::TTuple(vec![Type::TInteger])),
                    (Expression::Dict(_), Expression::Dict(_)) => Ok(Type::TDict(Box::new(Type::TInteger), Box::new(Type::TString))),
                    (Expression::Hash(_), Expression::Hash(_)) => Ok(Type::THash(Box::new(Type::TInteger), Box::new(Type::TString))),
                    _ => Err(String::from("Incompatible value type for Hash")),
                }
            } else {
                Err(String::from("Key not found in Hash"))
            }
        },
        _ => Err(String::from("Expected a Hash expression")),
    }
}

fn check_dict_remove(dict: Expression, key: Expression, _env: &Environment) -> Result<Type, String> {
    if let Expression::Dict(Some(mut map)) = dict {
        if let Some(pos) = map.iter().position(|(k, _)| {
            match (k, &key) {
                (Expression::CString(s1), Expression::CString(s2)) => s1 == s2,
                (Expression::CInt(i1), Expression::CInt(i2)) => i1 == i2,
                (Expression::CReal(r1), Expression::CReal(r2)) => r1 == r2,
                (Expression::CTrue, Expression::CTrue) => true,
                (Expression::CFalse, Expression::CFalse) => true,
                (Expression::List(_, _), Expression::List(_, _)) => true,
                (Expression::Tuple(_), Expression::Tuple(_)) => true,
                (Expression::Dict(_), Expression::Dict(_)) => true,
                (Expression::Hash(_), Expression::Hash(_)) => true,
                _ => false,
            }
        }) {
            map.remove(pos);
            return Ok(Type::TUnit);
        } else {
            return Err(String::from("Key not found in Dict"));
        }
    }
    Err(String::from("Expected a Dict expression"))
}

fn check_hash_remove(hash: &mut Expression, key: Expression, _env: &Environment) -> Result<Type, String> {
    match hash {
        Expression::Hash(Some(ref mut map)) => {
            if map.contains_key(&key) {
                map.remove(&key);
                Ok(Type::TUnit)
            } else {
                Err(String::from("Key not found in Hash"))
            }
        },
        _ => Err(String::from("Expected a Hash expression")),
    }
}

fn check_create_tuple(
    maybe_tuple: Vec<Expression>,
    env: &Environment,
) -> Result<Type, ErrorMessage> {
    
    let mut vec_types = Vec::new();
    match maybe_tuple {
        elements => {
            if elements.is_empty() {
                return Ok(Type::TTuple(vec![]));
            }
            for elem in elements{
                let type_elem = check(elem.clone(),&env)?;
                if !vec_types.contains(&type_elem){
                    vec_types.push(type_elem);
                }
            }

            let tuple_type = Type::TTuple(vec_types);
            Ok(tuple_type)
        }
        //_ => {
            //return Err("Provided expression is not a tuple".to_string());
        //}
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
        
        // _=>{
        //     Err(String::from("[Type error] expected list as argument"))
        // }
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

fn check_get(data_structure: Expression, index: Expression,env: &Environment)
->Result<Type,ErrorMessage>{
    let data_structure_type = check(data_structure,env)?;
    let index_type = check(index,env)?;

    match(data_structure_type,&index_type){
        (Type::TList(boxed_type),Type::TInteger)=>{
            Ok(*boxed_type)
        }
        (Type::TTuple(_),Type::TInteger)=>{
            match index_type {
                Type::TInteger => {
                    Ok(Type::TBool)
                }
                _ => Err(String::from("[Type error] Index must be an integer")),
            }
        }
        _=> Err(String::from("[Type error] index must be integer"))
    }
}

fn check_len(data_structure:Expression, env: &Environment)->
Result<Type, String>{
    let data_structure_type = check(data_structure,env)?;
    match data_structure_type{
        Type::TList(_)=>{
            Ok(Type::TInteger)
        }
        Type::TTuple(_)=>{
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
    //use std::hash::Hash;

    use super::*;

    use crate::ir::ast::Expression::*;
    use crate::ir::ast::Type::*;


    #[test]
    fn check_create_valid_set(){
        let env = HashMap::new();
        let set = Set(vec![CInt(1), CInt(2), CInt(3)]);
        assert_eq!(check(set, &env), Ok(TSet(Box::new(TInteger))));
    }

    #[test]
    fn check_dict_creation_t() {
        let env = Environment::new();

        let elements = Some(vec![
            (Expression::CInt(1), Expression::CReal(3.14)),
            (Expression::CInt(2), Expression::CReal(2.71)),
        ]);

        let result = check_dict_creation(elements, &env);
        assert_eq!(result, Ok(Type::TDict(Box::new(Type::TInteger), Box::new(Type::TReal))));
    }
    
    #[test]
    fn check_hash_creation_t() {
        let env = Environment::new();

        let mut map = HashMap::new();
        map.insert(Expression::CInt(1), Expression::CReal(3.14));
        map.insert(Expression::CInt(2), Expression::CReal(2.71));

        let result = check_hash_creation(Some(map), &env);
        assert_eq!(result, Ok(Type::THash(Box::new(Type::TInteger), Box::new(Type::TReal))));
    }

    #[test]
    fn check_dict_get_t() {
        let env = Environment::new();

        let dict = Expression::Dict(Some(vec![
            (Expression::CString("chave1".to_string()), Expression::CInt(10)),
            (Expression::CString("chave2".to_string()), Expression::CInt(20)),
        ]));

        let key = Expression::CString("chave1".to_string());

        let result = check_dict_get(dict, key, &env);
        assert_eq!(result, Ok(Type::TInteger));
    }

    #[test]
    fn check_hash_get_t() {
        let env = Environment::new();
    
        let mut hash_map = HashMap::new();
        hash_map.insert(Expression::CString("chave1".to_string()), Expression::CInt(10));
        hash_map.insert(Expression::CString("chave2".to_string()), Expression::CInt(20));
    
        let hash = Expression::Hash(Some(hash_map));
        let key = Expression::CString("chave1".to_string());
    
        let result = check_hash_get(hash, key, &env);
        assert_eq!(result, Ok(Type::TInteger));
    }

    #[test]
    fn check_dict_set_t() {
        let env = Environment::new();
    
        let dict_elements = vec![
            (Expression::CString("chave1".to_string()), Expression::CInt(10)),
            (Expression::CString("chave2".to_string()), Expression::CInt(20)),
        ];
        let dict = Expression::Dict(Some(dict_elements.clone()));
    
        let key = Expression::CString("chave1".to_string());
        let value = Expression::CInt(30);
    
        let result = check_dict_set(dict, key.clone(), value, &env);
        assert_eq!(result, Ok(Type::TInteger));
    
        let dict = Expression::Dict(Some(dict_elements));
        let result = check_dict_get(dict, key, &env);
        assert_eq!(result, Ok(Type::TInteger));
    }
    #[test]
    fn check_hash_set_t() {
        let env = Environment::new();

        let mut hash_map = HashMap::new();
        hash_map.insert(Expression::CString("chave1".to_string()), Expression::CInt(10));
        hash_map.insert(Expression::CString("chave2".to_string()), Expression::CInt(20));
        let hash = Expression::Hash(Some(hash_map.clone()));
        let key = Expression::CString("chave1".to_string());
        let value = Expression::CInt(30);

        let result = check_hash_set(hash.clone(), key.clone(), value.clone(), &env);
        assert_eq!(result, Ok(Type::TInteger));

        let result = check_hash_get(hash, key, &env);
        assert_eq!(result, Ok(Type::TInteger));
    }

    #[test]
    fn check_dict_remove_t() {
        let env = Environment::new();
    
        let dict_elements = vec![
            (Expression::CString("chave1".to_string()), Expression::CInt(10)),
            (Expression::CString("chave2".to_string()), Expression::CInt(20)),
        ];
        let dict = Expression::Dict(Some(dict_elements.clone()));
        let key = Expression::CString("chave1".to_string());
    
        let result = check_dict_remove(dict.clone(), key.clone(), &env);
        assert_eq!(result, Ok(Type::TUnit));
    
        let updated_dict_elements: Vec<(Expression, Expression)> = dict_elements
            .into_iter()
            .filter(|(k, _)| *k != key)
            .collect();
        let updated_dict = Expression::Dict(Some(updated_dict_elements));
    
        match updated_dict {
            Expression::Dict(Some(ref map)) => {
                assert!(!map.iter().any(|(k, _)| *k == key));
            },
            _ => panic!("Expected a Dict expression"),
        }
    
        let result = check_dict_get(updated_dict, key, &env);
        assert_eq!(result, Err(String::from("Key not found in Dict")));
    }
    
    #[test]
    fn check_hash_remove_t() {
        let env = Environment::new();
        
        let mut hash_elements = HashMap::new();
        hash_elements.insert(Expression::CString("chave1".to_string()), Expression::CInt(10));
        hash_elements.insert(Expression::CString("chave2".to_string()), Expression::CInt(20));
        
        let mut hash_expr = Expression::Hash(Some(hash_elements));
        
        let key = Expression::CString("chave1".to_string());
        
        let result = check_hash_remove(&mut hash_expr, key.clone(), &env);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Type::TUnit);
        
        let result = check_hash_get(hash_expr, key, &env);
        assert_eq!(result, Err(String::from("Key not found in Hash")));
    }
        

    #[test]
    fn check_tlist_comparison_different_types() {
        let t_list1 = TList(Box::new(TInteger));
        let t_list2 = TList(Box::new(TBool));

        assert_eq!(t_list1 == t_list2, false);
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
    fn check_tlist_comparison() {
        let t_list1 = TList(Box::new(TInteger));
        let t_list2 = TList(Box::new(TInteger));

        assert_eq!(t_list1 == t_list2, true);
    }

    #[test]
    fn check_create_valid_tuple(){
        let env = HashMap::new();
        let elements = vec![CInt(0),CReal(2.5),CString("Rust".to_string())];
        let tuple = check(
            Tuple(elements),&env)
            .unwrap();
        assert_eq!(tuple,TTuple(vec![TInteger,TReal,TString]));
    }

    #[test]
    fn check_create_invalid_tuple(){
        let env = HashMap::new();
        let elements = vec![CInt(0),CReal(2.5),CString("Rust".to_string())];
        let tuple = check(Tuple(elements),&env)
            .unwrap();
        assert_ne!(tuple,TTuple(vec![TInteger,TString,TReal]));
    }

    #[test]
    fn check_ttuple_comparison() {
        let tuple1 = TTuple(vec![TInteger,TBool]);
        let tuple2 = TTuple(vec![TInteger,TBool]);
        assert_eq!(tuple1 == tuple2, true);
    }

    #[test]
    fn check_ttuple_comparison_different_types() {
        let tuple1 = TTuple(vec![TInteger,TBool]);
        let tuple2 = TTuple(vec![TInteger,TReal]);
        assert_eq!(tuple1 == tuple2, false);
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