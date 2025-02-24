use std::collections::HashMap;
use crate::ir::ast::{Environment, Expression, Name, Statement, Type};

type ErrorMessage = String;

pub enum ControlFlow {
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

        // Data Structure
        Expression::Set(elements) =>
        check_create_set(elements,env),
        Expression::Hash(elements)=>
        check_hash_creation(elements, env),
        Expression::GetHash(hash, key)=>
        check_hash_get(*hash, *key, env),
        Expression::SetHash(hash, key, value)=>
        check_hash_set(*hash, *key, *value, env),
        Expression::RemoveHash(mut hash, key)=>
        check_hash_remove(&mut *hash, *key, env),
        Expression::Tuple(elements) => 
        check_create_tuple(elements,env),
        Expression::List(elements)=>
        check_create_list(elements, env),
        Expression::Append(list,elem)=>
        check_append_list(*list,*elem,env),
        Expression::Insert(list,elem)=>
        check_insert_list(*list,*elem,env),
        Expression::Concat(list1, list2)=>
        check_concat_list(*list1, *list2, env),
        Expression::PopBack(list)=>
        check_pop_back_list(*list,env),
        Expression::PopFront(list)=>
        check_pop_front_list(*list,env),
        Expression::Get(data_structure,index ) =>
        check_get(*data_structure,*index,env),
        Expression::Len(data_structure) =>
        check_len(*data_structure,env),
        Expression::Union(set1,set2)=>
        check_union_set(*set1,*set2,env),
        Expression::Intersection(set1,set2)=>
        check_intersection_set(*set1,*set2,env),
        Expression::Difference(set1,set2)=>
        check_difference_set(*set1,*set2,env),
        Expression::Disjunction(set1,set2)=>
        check_disjunction_set(*set1,*set2,env),
        //_ => Err(String::from("not implemented yet")),
    }
}
// Data Structure
fn check_create_set(
    maybe_set: Vec<Expression>,
    env: &Environment<Type>,
) -> Result<Type, ErrorMessage> {
    if maybe_set.is_empty() {
        return Ok(Type::NotDefine);
    }

    let first_type = check_exp(maybe_set[0].clone(), env)?;

    for item in &maybe_set {
        let item_type = check_exp(item.clone(), env)?;
        if item_type != first_type {
            return Err(String::from("[Type error] Different types in set"));
        }
    }

    Ok(Type::TSet(Box::new(first_type)))
}

fn check_union_set(set1: Expression, set2: Expression, env: &Environment<Type>) -> Result<Type, ErrorMessage> {
    let set1_type = check_exp(set1, env)?;
    let set2_type = check_exp(set2, env)?;

    match (set1_type, set2_type) {
        (Type::TSet(boxed_type1), Type::TSet(boxed_type2)) => {
            if boxed_type1 == boxed_type2 {
                Ok(Type::TSet(boxed_type1))
            } else {
                Err(String::from("[Type error] Set types do not match"))
            }
        }
        (Type::NotDefine, Type::TSet(boxed_type)) | (Type::TSet(boxed_type), Type::NotDefine) => {
            Ok(Type::TSet(boxed_type))
        }
        _ => Err(String::from("[Type error] Expected two sets")),
    }
}

fn check_intersection_set(set1: Expression, set2: Expression, env: &Environment<Type>) -> Result<Type, ErrorMessage> {
    let set1_type = check_exp(set1, env)?;
    let set2_type = check_exp(set2, env)?;

    match (set1_type, set2_type) {
        (Type::TSet(boxed_type1), Type::TSet(boxed_type2)) => {
            if boxed_type1 == boxed_type2 {
                Ok(Type::TSet(boxed_type1))
            } else {
                Err(String::from("[Type error] Set types do not match"))
            }
        }
        _ => Err(String::from("[Type error] Expected two sets")),
    }
}

fn check_difference_set(set1: Expression, set2: Expression, env: &Environment<Type>) -> Result<Type, ErrorMessage> {
    let set1_type = check_exp(set1, env)?;
    let set2_type = check_exp(set2, env)?;

    match (set1_type, set2_type) {
        (Type::TSet(boxed_type1), Type::TSet(boxed_type2)) => {
            if boxed_type1 == boxed_type2 {
                Ok(Type::TSet(boxed_type1))
            } else {
                Err(String::from("[Type error] Set types do not match"))
            }
        }
        _ => Err(String::from("[Type error] Expected two sets")),
    }
}

fn check_disjunction_set(set1: Expression, set2: Expression, env: &Environment<Type>) -> Result<Type, ErrorMessage> {
    let set1_type = check_exp(set1, env)?;
    let set2_type = check_exp(set2, env)?;

    match (set1_type, set2_type) {
        (Type::TSet(boxed_type1), Type::TSet(boxed_type2)) => {
            if boxed_type1 == boxed_type2 {
                Ok(Type::TSet(boxed_type1))
            } else {
                Err(String::from("[Type error] Set types do not match"))
            }
        }
        _ => Err(String::from("[Type error] Expected two sets")),
    }
}

fn check_hash_creation(elements: Option<HashMap<Expression, Expression>>, env: &Environment<Type>) -> Result<Type, ErrorMessage> {
    match elements {
        Some(map) => {
            let mut key_type: Option<Type> = None;
            let mut value_type: Option<Type> = None;

            for (key, value) in map {
                let key_type_check = check_exp(key, env)?;
                let value_type_check = check_exp(value, env)?;

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

fn check_hash_get(hash: Expression, key: Expression, env: &Environment<Type>) -> Result<Type, String> {
    let hash_type = check_exp(hash.clone(), env)?;
    let key_type = check_exp(key.clone(), env)?;

    match hash_type {
        Type::THash(boxed_key_type, boxed_value_type) => {
            if *boxed_key_type == key_type {
                Ok(*boxed_value_type)
            } else {
                Err(String::from("Key type mismatch"))
            }
        }
        _ => Err(String::from("Expected a hash")),
    }
}

fn check_hash_set(hash: Expression, key: Expression, value: Expression, env: &Environment<Type>) -> Result<Type, String> {
    let hash_type = check_exp(hash.clone(), env)?;
    let key_type = check_exp(key.clone(), env)?;
    let value_type = check_exp(value.clone(), env)?;

    match hash_type {
        Type::THash(boxed_key_type, boxed_value_type) => {
            if *boxed_key_type != key_type {
                Err(String::from("Key type mismatch"))
            } else if *boxed_value_type != value_type {
                Err(String::from("Incompatible value type for Hash"))
            } else {
                Ok(*boxed_value_type)
            }
        }
        _ => Err(String::from("Expected a Hash")),
    }
}

fn check_hash_remove(hash: &Expression, key: Expression, env: &Environment<Type>) -> Result<Type, String> {
    let hash_type = check_exp(hash.clone(), env)?;
    let key_type = check_exp(key.clone(), env)?;

    match hash_type {
        Type::THash(boxed_key_type, value_type) => {
            if *boxed_key_type == key_type {
                Ok(Type::THash(boxed_key_type, value_type))
            } else {
                Err(String::from("Key type mismatch"))
            }
        }
        _ => Err(String::from("Expected a Hash")),
    }
}

fn check_create_tuple(
    maybe_tuple: Vec<Expression>,
    env: &Environment<Type>,
) -> Result<Type, ErrorMessage> {
    
    let mut vec_types = Vec::new();
    match maybe_tuple {
        elements => {
            if elements.is_empty() {
                return Ok(Type::TTuple(vec![]));
            }
            for elem in elements{
                let type_elem = check_exp(elem.clone(),&env)?;
                if !vec_types.contains(&type_elem){
                    vec_types.push(type_elem);
                }
            }

            let tuple_type = Type::TTuple(vec_types);
            Ok(tuple_type)
        }
    }
}


fn check_create_list(
    maybe_list: Vec<Expression>,
    env: &Environment<Type>,) 
-> Result<Type,ErrorMessage>{
    
    match maybe_list{
        elements => {

            if elements.is_empty(){
                return Ok(Type::NotDefine);
            }

            let first_elem = elements[0].clone();
            let type_list = check_exp(first_elem,env)?;

            for elem in elements{
                let type_elem = check_exp(elem.clone(),&env)?;
                if type_elem != type_list{
                    return Err(String::from("[Type error] Different types in list"))
                }
            }

            Ok(Type::TList(Box::new(type_list)))
        }
    }
}

fn check_concat_list(list: Expression, list2: Expression, env: &Environment<Type>) 
-> Result<Type, ErrorMessage> {
    let list_type1 = check_exp(list, env)?;
    let list_type2 = check_exp(list2, env)?;
    
    match (list_type1.clone(), list_type2.clone()) {
        (Type::NotDefine, Type::TList(_)) => Ok(list_type2),
        (Type::TList(_), Type::NotDefine) => Ok(list_type1),
        (Type::NotDefine,Type::NotDefine)=> Ok(Type::NotDefine),
        (Type::TList(boxed_type1), Type::TList(boxed_type2)) => {
            if *boxed_type1 == *boxed_type2 {
                Ok(Type::TList(boxed_type1.clone()))
            } else {
                Err(String::from("[Type error] Both lists must have the same type"))
            }
        }
        _ => Err(String::from("[Type error] element type does not match list type")),
    }
}

fn check_append_list(list: Expression, elem: Expression ,env: &Environment<Type>)
-> Result<Type,ErrorMessage>{
    let list_type = check_exp(list,env)?;
    let elem_type = check_exp(elem,env)?;
    match (list_type.clone(),elem_type.clone()) {
        (Type::NotDefine,type_elem)=> Ok(type_elem),
        (Type::TList(boxed_type),type_elem)=>{
            if *boxed_type == type_elem{
                Ok(Type::TList(boxed_type.clone()))
            }
            else{
                Err(String::from("[Type error] element type does not match list type"))
            }
        }
        _ => Err(String::from("[Type error] element type does not match list type")),
    }
}

fn check_insert_list(list: Expression, elem: Expression, env: &Environment<Type>) -> Result<Type, ErrorMessage> {
    let list_type = check_exp(list, env)?;
    let elem_type = check_exp(elem, env)?;

    match list_type {
        Type::TList(boxed_type) => {
            if *boxed_type == elem_type {
                Ok(Type::TList(boxed_type))
            } else {
                Err(String::from("[Type error] Element type does not match list type"))
            }
        }
        Type::NotDefine => Ok(Type::TList(Box::new(elem_type))),
        _ => Err(String::from("[Type error] Expected a list")),
    }
}

fn check_pop_back_list(list: Expression, env: &Environment<Type>)
->Result<Type,ErrorMessage>{
    let list_type = check_exp(list,env)?;

    match list_type.clone(){
        Type::TList(boxed_type) => {
                Ok(Type::TList(boxed_type.clone()))
            }
        Type::NotDefine=> Err(String::from("cannot pop from an empty list")),

        _ => Err(String::from("[Type error] cannot pop from a non-list type"))
    }
}

fn check_pop_front_list(list: Expression, env: &Environment<Type>)
->Result<Type,ErrorMessage>{
    let list_type = check_exp(list,&env)?;
    match list_type{
        Type::TList(boxed_type) => {
            Ok(Type::TList(boxed_type.clone()))
            }

        Type::NotDefine=> Err(String::from("cannot pop from an empty list")),
        _=>Err(String::from("[Type error] cannot pop from a non-list type"))
    }
}

fn check_get(data_structure: Expression, index: Expression,env: &Environment<Type>)
->Result<Type,ErrorMessage>{
    let data_structure_type = check_exp(data_structure,env)?;
    let index_type = check_exp(index,env)?;

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
        (Type::NotDefine,Type::TInteger)=>Err(String::from("cannot get element from an empty list")),
        _=> Err(String::from("[Type error] index must be integer"))
    }
}

fn check_len(data_structure:Expression, env: &Environment<Type>)->
Result<Type, String>{
    let data_structure_type = check_exp(data_structure,env)?;
    match data_structure_type{
        Type::TList(_)=>{
            Ok(Type::TInteger)
        }
        Type::TTuple(_)=>{
            Ok(Type::TInteger)
        }
        Type::NotDefine=>{
            Ok(Type::TInteger)
        },
        _=>Err(String::from("[Type error] first argument must be a list"))
    }
}


pub fn check_stmt(stmt: Statement, env: &Environment<Type>) -> Result<ControlFlow, ErrorMessage> {
    let mut new_env = env.clone();

    match stmt {
        Statement::Assignment(name, exp, kind) => {
            let exp_type = check_exp(*exp, &new_env)?;

            if let Some(state_type) = kind {
                if exp_type != state_type {
                    return Err(format!("[Type Error on '{}()'] '{}' has mismatched types: expected '{:?}', found '{:?}'.", new_env.scope_name(), name, state_type, exp_type));
                }
            } else {
                let stated_type = check_var_name(name.clone(), &new_env, true)?;

                if exp_type != stated_type {
                    return Err(format!("[Type Error on '{}()'] '{}' has mismatched types: expected '{:?}', found '{:?}'.", new_env.scope_name(), name, stated_type, exp_type));
                }
            }

            new_env.insert_variable(name, exp_type);

            Ok(ControlFlow::Continue(new_env))
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
                None => return Ok(ControlFlow::Continue(new_env)),
            };

            match (stmt_then_result, stmt_else_result) {
                (ControlFlow::Return(kind), ControlFlow::Continue(_)) => {
                    Ok(ControlFlow::Return(kind))
                }
                (ControlFlow::Continue(_), ControlFlow::Return(kind)) => {
                    Ok(ControlFlow::Return(kind))
                }
                (ControlFlow::Return(kind1), ControlFlow::Return(_)) => {
                    Ok(ControlFlow::Return(kind1))
                }
                _ => Ok(ControlFlow::Continue(new_env)),
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
                ControlFlow::Continue(_) => Ok(ControlFlow::Continue(new_env)),
                ControlFlow::Return(kind) => Ok(ControlFlow::Return(kind)),
            }
        }
        Statement::Sequence(stmt1, stmt2) => {
            if let ControlFlow::Continue(control_env) = check_stmt(*stmt1, &new_env)? {
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
                ControlFlow::Continue(_) => Err(format!(
                    "[Syntax Error] '{}()' does not have a return statement.",
                    func.name
                )),
                ControlFlow::Return(_) => {
                    new_env.remove_frame();
                    new_env.insert_variable(func.name, func_type);
                    Ok(ControlFlow::Continue(new_env))
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

                Ok(ControlFlow::Return(exp_type))
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
    fn check_create_valid_set() {
        let env = Environment::new();
        let set = Expression::Set(vec![Expression::CInt(1), Expression::CInt(2), Expression::CInt(3)]);
        assert_eq!(check_exp(set, &env), Ok(Type::TSet(Box::new(Type::TInteger))));
    }
    
    #[test]
    fn check_union_valid_sets() {
        let env = Environment::new();
        let set1 = Expression::Set(vec![Expression::CInt(1), Expression::CInt(2)]);
        let set2 = Expression::Set(vec![Expression::CInt(2), Expression::CInt(3)]);
    
        assert_eq!(
            check_union_set(set1, set2, &env),
            Ok(Type::TSet(Box::new(Type::TInteger)))
        );
    }
    
    #[test]
    fn check_union_invalid_sets() {
        let env = Environment::new();
        let set1 = Expression::Set(vec![Expression::CInt(1), Expression::CInt(2)]);
        let set2 = Expression::Set(vec![Expression::CReal(2.0), Expression::CReal(3.0)]);
    
        assert_eq!(
            check_union_set(set1, set2, &env),
            Err(String::from("[Type error] Set types do not match"))
        );
    }
    
    #[test]
    fn check_union_with_empty_set() {
        let env = Environment::new();
        let set1 = Expression::Set(vec![Expression::CInt(1), Expression::CInt(2)]);
        let set2 = Expression::Set(vec![]);
    
        assert_eq!(
            check_union_set(set1, set2, &env),
            Ok(Type::TSet(Box::new(Type::TInteger)))
        );
    }
    
    #[test]
    fn check_intersection_valid_sets() {
        let env = Environment::new();
        let set1 = Expression::Set(vec![Expression::CInt(1), Expression::CInt(2)]);
        let set2 = Expression::Set(vec![Expression::CInt(2), Expression::CInt(3)]);
    
        assert_eq!(
            check_intersection_set(set1, set2, &env),
            Ok(Type::TSet(Box::new(Type::TInteger)))
        );
    }
    
    #[test]
    fn check_intersection_invalid_sets() {
        let env = Environment::new();
        let set1 = Expression::Set(vec![Expression::CInt(1), Expression::CInt(2)]);
        let set2 = Expression::Set(vec![Expression::CReal(2.0), Expression::CReal(3.0)]);
    
        assert_eq!(
            check_intersection_set(set1, set2, &env),
            Err(String::from("[Type error] Set types do not match"))
        );
    }
    
    #[test]
    fn check_difference_valid_sets() {
        let env = Environment::new();
        let set1 = Expression::Set(vec![Expression::CInt(1), Expression::CInt(2)]);
        let set2 = Expression::Set(vec![Expression::CInt(2), Expression::CInt(3)]);
    
        assert_eq!(
            check_difference_set(set1, set2, &env),
            Ok(Type::TSet(Box::new(Type::TInteger)))
        );
    }
    
    #[test]
    fn check_difference_invalid_sets() {
        let env = Environment::new();
        let set1 = Expression::Set(vec![Expression::CInt(1), Expression::CInt(2)]);
        let set2 = Expression::Set(vec![Expression::CReal(2.0), Expression::CReal(3.0)]);
    
        assert_eq!(
            check_difference_set(set1, set2, &env),
            Err(String::from("[Type error] Set types do not match"))
        );
    }
    
    #[test]
    fn check_disjunction_valid_sets() {
        let env = Environment::new();
        let set1 = Expression::Set(vec![Expression::CInt(1), Expression::CInt(2)]);
        let set2 = Expression::Set(vec![Expression::CInt(2), Expression::CInt(3)]);
    
        assert_eq!(
            check_disjunction_set(set1, set2, &env),
            Ok(Type::TSet(Box::new(Type::TInteger)))
        );
    }
    
    #[test]
    fn check_disjunction_invalid_sets() {
        let env = Environment::new();
        let set1 = Expression::Set(vec![Expression::CInt(1), Expression::CInt(2)]);
        let set2 = Expression::Set(vec![Expression::CReal(2.0), Expression::CReal(3.0)]);
    
        assert_eq!(
            check_disjunction_set(set1, set2, &env),
            Err(String::from("[Type error] Set types do not match"))
        );
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
    fn check_hash_remove_t() {
        let env = Environment::new();
        
        let mut hash_elements = HashMap::new();
        hash_elements.insert(Expression::CString("chave1".to_string()), Expression::CInt(10));
        hash_elements.insert(Expression::CString("chave2".to_string()), Expression::CInt(20));
        
        let mut hash_expr = Expression::Hash(Some(hash_elements));
        let key = Expression::CString("chave1".to_string());
    
        let result = check_hash_remove(&mut hash_expr, key.clone(), &env);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Type::THash(Box::new(Type::TString), Box::new(Type::TInteger))); // Alterado
    
        let result = check_hash_get(hash_expr.clone(), key, &env);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Type::TInteger);
    }

    #[test]
    fn check_tlist_comparison_different_types() {
        let t_list1 = TList(Box::new(TInteger));
        let t_list2 = TList(Box::new(TBool));
    
        assert_eq!(t_list1 == t_list2, false);
    }
    
    #[test]
    fn check_create_valid_list() {
        let env = Environment::new();
        let elements = vec![CInt(1), CInt(2), CInt(3)];
        let list = Expression::List(elements);
    
        assert_eq!(check_exp(list, &env), Ok(Type::TList(Box::new(Type::TInteger))));
    }
    
    #[test]
    fn check_create_inconsistent_list() {
        let env = Environment::new();
        let elements = vec![CInt(1), CReal(2.0)];
        let list = Expression::List(elements);
    
        assert_eq!(
            check_exp(list, &env),
            Err(String::from("[Type error] Different types in list"))
        );
    }
    
    #[test]
    fn check_append_valid_elements_in_list() {
        let env = Environment::new();
        let list = Expression::List(vec![]);
        let elem = Expression::CInt(5);
        let append = Expression::Append(Box::new(list), Box::new(elem));
    
        assert!(check_exp(append, &env).is_ok());
    }
    
    #[test]
    fn check_insert_valid_list() {
        let env = Environment::new();
        let list = Expression::List(vec![CInt(1), CInt(2)]);
        let new_elem = Expression::CInt(3);
    
        assert_eq!(
            check_insert_list(list, new_elem, &env),
            Ok(Type::TList(Box::new(Type::TInteger)))
        );
    }
    
    #[test]
    fn check_insert_invalid_element_type() {
        let env = Environment::new();
        let list = Expression::List(vec![CInt(1), CInt(2)]);
        let new_elem = Expression::CReal(3.0);
    
        assert_eq!(
            check_insert_list(list, new_elem, &env),
            Err(String::from("[Type error] Element type does not match list type"))
        );
    }
    
    #[test]
    fn check_insert_empty_list() {
        let env = Environment::new();
        let list = Expression::List(vec![]);
        let new_elem = Expression::CInt(1);
    
        assert_eq!(
            check_insert_list(list, new_elem, &env),
            Ok(Type::TList(Box::new(Type::TInteger)))
        );
    }
    
    #[test]
    fn check_concat_valid_list_with_list() {
        let env = Environment::new();
        let list1 = Expression::List(vec![CInt(5)]);
        let list2 = Expression::List(vec![CInt(10)]);
        let list3 = Expression::Concat(Box::new(list1), Box::new(list2));
    
        assert!(check_exp(list3, &env).is_ok());
    }
    
    #[test]
    fn check_concat_invalid_list_with_list() {
        let env = Environment::new();
        let list1 = Expression::List(vec![CInt(5)]);
        let list2 = Expression::List(vec![CReal(10.5)]);
        let list3 = Expression::Concat(Box::new(list1), Box::new(list2));
    
        assert!(check_exp(list3.clone(), &env).is_err());
        assert_eq!(
            check_exp(list3, &env),
            Err(String::from("[Type error] Both lists must have the same type"))
        );
    }
    
    #[test]
    fn check_concat_valid_list_with_empty_list() {
        let env = Environment::new();
        let list1 = Expression::List(vec![CInt(5)]);
        let list2 = Expression::List(vec![]);
        let list3 = Expression::Concat(Box::new(list1), Box::new(list2));
    
        assert_eq!(check_exp(list3, &env), Ok(Type::TList(Box::new(Type::TInteger))));
    }
    
    #[test]
    fn check_pop_valid() {
        let env = Environment::new();
        let list = Expression::List(vec![Expression::CInt(5)]);
        let pop = Expression::PopBack(Box::new(list));
        assert!(check_exp(pop, &env).is_ok());
    }
    
    #[test]
    fn check_tlist_comparison() {
        let t_list1 = Type::TList(Box::new(Type::TInteger));
        let t_list2 = Type::TList(Box::new(Type::TInteger));
    
        assert_eq!(t_list1 == t_list2, true);
    }
    
    #[test]
    fn check_create_valid_tuple() {
        let env = Environment::new();
        let elements = vec![CInt(0), CReal(2.5), CString("Rust".to_string())];
        let tuple = check_exp(Expression::Tuple(elements), &env).unwrap();
        assert_eq!(tuple, Type::TTuple(vec![Type::TInteger, Type::TReal, Type::TString]));
    }
    
    #[test]
    fn check_create_invalid_tuple() {
        let env = Environment::new();
        let elements = vec![CInt(0), CReal(2.5), CString("Rust".to_string())];
        let tuple = check_exp(Expression::Tuple(elements), &env).unwrap();
        assert_ne!(tuple, Type::TTuple(vec![Type::TInteger, Type::TString, Type::TReal]));
    }
    
    #[test]
    fn check_ttuple_comparison() {
        let tuple1 = Type::TTuple(vec![Type::TInteger, Type::TBool]);
        let tuple2 = Type::TTuple(vec![Type::TInteger, Type::TBool]);
    
        assert_eq!(tuple1 == tuple2, true);
    }
    
    #[test]
    fn check_ttuple_comparison_different_types() {
        let tuple1 = Type::TTuple(vec![Type::TInteger, Type::TBool]);
        let tuple2 = Type::TTuple(vec![Type::TInteger, Type::TReal]);
    
        assert_eq!(tuple1 == tuple2, false);
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

        let assignment = Assignment("a".to_string(), Box::new(CTrue), Some(TBool));

        match check_stmt(assignment, &env) {
            Ok(ControlFlow::Continue(new_env)) => {
                assert_eq!(new_env.search_frame("a".to_string()), Some(TBool).as_ref());
            }
            Ok(_) => assert!(false),
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn check_assignment_error1() {
        let env: Environment<Type> = Environment::new();

        let assignment = Assignment("a".to_string(), Box::new(CTrue), Some(TInteger));

        match check_stmt(assignment, &env) {
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

        let assignment1 = Assignment("a".to_string(), Box::new(CTrue), Some(TBool));
        let assignment2 = Assignment("a".to_string(), Box::new(CInt(1)), None);
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
            Box::new(Assignment(
                "a".to_string(),
                Box::new(CInt(1)),
                Some(TInteger),
            )),
            Some(Box::new(Assignment(
                "b".to_string(),
                Box::new(CReal(2.0)),
                Some(TReal),
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

        let assignment1 = Assignment("a".to_string(), Box::new(CInt(3)), Some(TInteger));
        let assignment2 = Assignment("b".to_string(), Box::new(CInt(0)), Some(TInteger));
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
            Ok(ControlFlow::Continue(new_env)) => {
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
            Ok(ControlFlow::Continue(new_env)) => {
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
            Ok(ControlFlow::Continue(_)) => assert!(true),
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
