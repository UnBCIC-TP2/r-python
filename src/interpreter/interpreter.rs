use std::collections::HashMap;

use crate::ir::ast::{Expression, Name, Statement};

type ErrorMessage = String;

type Environment = HashMap<Name, Expression>;

pub fn eval(exp: Expression, env: &Environment) -> Result<Expression, ErrorMessage> {
    match exp {
        Expression::Add(lhs, rhs) => add(*lhs, *rhs, env),
        Expression::Sub(lhs, rhs) => sub(*lhs, *rhs, env),
        Expression::Mul(lhs, rhs) => mul(*lhs, *rhs, env),
        Expression::Div(lhs, rhs) => div(*lhs, *rhs, env),
        Expression::Rmd(lhs, rhs) => rmd(*lhs, *rhs, env),
        Expression::And(lhs, rhs) => and(*lhs, *rhs, env),
        Expression::Or(lhs, rhs) => or(*lhs, *rhs, env),
        Expression::Not(lhs) => not(*lhs, env),
        Expression::EQ(lhs, rhs) => eq(*lhs, *rhs, env),
        Expression::GT(lhs, rhs) => gt(*lhs, *rhs, env),
        Expression::LT(lhs, rhs) => lt(*lhs, *rhs, env),
        Expression::GTE(lhs, rhs) => gte(*lhs, *rhs, env),
        Expression::LTE(lhs, rhs) => lte(*lhs, *rhs, env),
        Expression::Var(name) => lookup(name, env),

        Expression::Set(elements) => 
        eval_create_set(elements, env),

        Expression::Tuple(elements) => 
        eval_create_tuple(elements, env),

        Expression::Concat(list1,list2)=>
        eval_concat_list(*list1,*list2,env),

        Expression::List(elements_list)=>
        eval_create_list(elements_list, env),

        Expression::Append(list,elem)=>
        eval_append_list(*list,*elem,env),

        Expression::Insert(list,elem)=>
        eval_insert_list(*list,*elem,env),

        Expression::PopBack(list)=>
        eval_pop_back_list(*list,env),

        Expression::PopFront(list)=>
        eval_pop_front_list(*list,env),

        Expression::Get(data_strucutre,index)=>
        eval_get_element_list(*data_strucutre,*index,env),

        Expression::Len(list)=>
        eval_len_list(*list,env),

        // Expression::Dict(elements)=> 
        // eval_create_dict(elements, env),
        // Expression::GetDict(dict, key)=> 
        // eval_get_dict(*dict, *key, env),
        // Expression::SetDict(dict, key, value)=> 
        // eval_set_dict(*dict, *key, *value, env),
        // Expression::RemoveDict(dict, key)=> 
        // eval_remove_dict(*dict, *key, env),

        Expression::Hash(elements)=> 
        eval_create_hash(elements, env),
        Expression::GetHash(hash, key)=> 
        eval_get_hash(*hash, *key, env),
        Expression::SetHash(hash, key, value)=> 
        eval_set_hash(*hash, *key, *value, env),
        Expression::RemoveHash(hash, key) => 
        eval_remove_hash(*hash, *key, env),

        _ if is_constant(exp.clone()) => Ok(exp),
        _ => Err(String::from("Not implemented yet.")),
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

fn lookup(name: String, env: &Environment) -> Result<Expression, ErrorMessage> {
    match env.get(&name) {
        Some(value) => Ok(value.clone()),
        None => Err(format!("Variable {} not found", name)),
    }
}

/* Data structure */

fn eval_create_set(elements: Vec<Expression>, env: &Environment) -> Result<Expression, ErrorMessage> {
    let mut evaluated_elements = Vec::new();
    let mut type_set_eval: Option<Expression> = None;

    for element in elements {
        let eval_elem = eval(element, env)?;

        if type_set_eval.is_none() {
            type_set_eval = Some(eval_elem.clone());
        }

        // Verifica se o elemento já está no conjunto
        if evaluated_elements.iter().any(|e| e == &eval_elem) {
            return Err(format!("Element {:?} is already in set", eval_elem));
        }

        // Garante que todos os elementos sejam do mesmo tipo
        match (&eval_elem, type_set_eval.as_ref().unwrap()) {
            (Expression::CInt(_), Expression::CInt(_)) |
            (Expression::CReal(_), Expression::CReal(_)) |
            (Expression::CString(_), Expression::CString(_)) => {
                evaluated_elements.push(eval_elem);
            }
            _ => {
                return Err(format!(
                    "Type {:?} does not match type {:?}",
                    eval_elem, type_set_eval.unwrap()
                ));
            }
        }
    }

    Ok(Expression::Set(evaluated_elements))
}


// fn eval_create_dict(
//     elements: Option<Vec<(Expression, Expression)>>, 
//     _env: &Environment
// ) -> Result<Expression, ErrorMessage> {
//     match elements {
//         Some(vec) => Ok(Expression::Dict(Some(vec))),
//         None => Ok(Expression::Dict(Some(Vec::new()))),
//     }
// }

fn eval_create_hash(
    elements: Option<HashMap<Expression, Expression>>, 
    _env: &Environment
) -> Result<Expression, ErrorMessage> {
    match elements {
        Some(map) => Ok(Expression::Hash(Some(map))),
        None => Ok(Expression::Hash(Some(HashMap::new()))),
    }
}

// fn eval_get_dict(
//     dict: Expression, 
//     key: Expression, 
//     _env: &Environment
// ) -> Result<Expression, ErrorMessage> {
//     if let Expression::Dict(Some(elements)) = dict {
//         for (k, v) in elements {
//             if k == key {
//                 return Ok(v);
//             }
//         }
//         Err(format!("Key not found in Dict"))
//     } else {
//         Err(format!("Expected a Dict"))
//     }
// }

fn eval_get_hash(
    hash: Expression, 
    key: Expression, 
    _env: &Environment
) -> Result<Expression, ErrorMessage> {
    if let Expression::Hash(Some(map)) = hash {
        if let Some(value) = map.get(&key) {
            return Ok(value.clone());
        }
        Err(format!("Key not found in Hash"))
    } else {
        Err(format!("Expected a Hash"))
    }
}

// fn eval_set_dict(
//     dict: Expression, 
//     key: Expression, 
//     value: Expression, 
//     _env: &Environment
// ) -> Result<Expression, ErrorMessage> {
//     if let Expression::Dict(Some(mut elements)) = dict {
//         let mut found = false;
        
//         for (k, v) in &mut elements {
//             if *k == key {
//                 *v = value.clone();
//                 found = true;
//                 break;
//             }
//         }
        
//         if !found {
//             elements.push((key, value));
//         }
        
//         Ok(Expression::Dict(Some(elements)))
//     } else {
//         Err(format!("Expected a Dict"))
//     }
// }

fn eval_set_hash(
    hash: Expression, 
    key: Expression, 
    value: Expression, 
    _env: &Environment
) -> Result<Expression, ErrorMessage> {
    if let Expression::Hash(Some(mut map)) = hash {
        map.insert(key.clone(), value.clone());
        Ok(Expression::Hash(Some(map)))
    } else {
        Err(format!("Expected a Hash"))
    }
}

// fn eval_remove_dict(
//     dict: Expression, 
//     key: Expression, 
//     _env: &Environment
// ) -> Result<Expression, ErrorMessage> {
//     if let Expression::Dict(Some(mut elements)) = dict {
//         if let Some(pos) = elements.iter().position(|(k, _)| *k == key) {
//             elements.remove(pos);
//             Ok(Expression::Dict(Some(elements)))
//         } else {
//             Err(format!("Key not found in Dict"))
//         }
//     } else {
//         Err(format!("Expected a Dict"))
//     }
// }

fn eval_remove_hash(
    hash: Expression, 
    key: Expression, 
    _env: &Environment
) -> Result<Expression, ErrorMessage> {
    if let Expression::Hash(Some(mut map)) = hash {
        if map.remove(&key).is_some() {
            Ok(Expression::Hash(Some(map)))
        } else {
            Err(format!("Key not found in Hash"))
        }
    } else {
        Err(format!("Expected a Hash"))
    }
}

fn eval_create_tuple(elements: Vec<Expression>, env: &Environment) -> Result<Expression, ErrorMessage> {
    
    let mut evaluated_elements = Vec::new();

    for element in elements {
        let eval_elem = eval(element, env)?;

        evaluated_elements.push(eval_elem);
    }

    Ok(Expression::Tuple(evaluated_elements))
}

fn eval_create_list(
    elements: Vec<Expression>, 
    env: &Environment
) -> Result<Expression, ErrorMessage> {

    let mut eval_elements = Vec::new();
    let mut type_list_eval: Option<Expression> = None;

    for element in elements {
        let eval_elem = eval(element, env)?;
        if type_list_eval.is_none() {
            type_list_eval = Some(eval_elem.clone());
        }
        match (&eval_elem, type_list_eval.as_ref().unwrap()) {
            (Expression::CInt(_), Expression::CInt(_)) |
            (Expression::CReal(_), Expression::CReal(_)) |
            (Expression::CString(_), Expression::CString(_)) => {
                eval_elements.push(eval_elem);
            }
            _ => {
                return Err(format!(
                    "Type {:?} does not match type {:?}",
                    eval_elem, type_list_eval.unwrap()
                ));
            }
        }
    }
    Ok(Expression::List(eval_elements))
}

fn eval_concat_list(list1:Expression, list2:Expression, env: &Environment)
->Result<Expression,ErrorMessage>{

    let list1_eval = eval(list1,&env)?;
    let list2_eval = eval(list2,&env)?;

    match(list1_eval,list2_eval){

        (Expression::List(mut vec),
        Expression::List(mut vec2))=>{
            if vec.is_empty(){
                for i in 0..vec2.len(){
                    if let Some(aux) = vec2.get(i){
                        vec.push(aux.clone());
                    }
                }
                return Ok(Expression::List(vec));
            }

            if vec2.is_empty(){
                for i in 0..vec.len(){
                    if let Some(aux) = vec.get(i){
                        vec2.push(aux.clone());
                    }
                }
                return Ok(Expression::List(vec2));
            }

            match (vec[0].clone(),vec2[0].clone()){
                (Expression::CInt(_),Expression::CInt(_)) |
                (Expression::CReal(_),Expression::CReal(_)) |
                (Expression::CString(_),Expression::CString(_)) =>{
                    for i in 0..vec2.len(){
                        if let Some(aux) = vec2.get(i){
                            vec.push(aux.clone());
                        }
                    }
                    Ok(Expression::List(vec))
                }
                _ => Err(String::from("[Type error] Both lists may have same type"))
            }

        }
        _=>Err(String::from("Expected two lists as arguments."))
    }

}


fn eval_append_list(list_expr: Expression, new_elem: Expression, env: &Environment)
->Result<Expression,ErrorMessage>{

    let eval_new_element = eval(new_elem, env)?;
    match list_expr {
        Expression::List(ref elements) => {
            if elements.is_empty() {
                return Ok(Expression::List(vec![eval_new_element]));
            }

            let first_element = &elements[0];
            match (first_element, &eval_new_element) {
                (Expression::CInt(_), Expression::CInt(_)) |
                (Expression::CReal(_), Expression::CReal(_)) |
                (Expression::CString(_), Expression::CString(_)) => {
                    let mut updated_elements = elements.clone();
                    updated_elements.push(eval_new_element);
                    return Ok(Expression::List(updated_elements));
                }
                _ => {
                    return Err(format!(
                        "Type {:?} does not match type {:?}",
                        eval_new_element, first_element
                    ));
                }
            }
        }
        Expression::Set(ref elements) => {
            if elements.is_empty() {
                return Ok(Expression::Set(vec![eval_new_element]));
            }

            let first_element = &elements[0];
            match (first_element, &eval_new_element) {
                (Expression::CInt(_), Expression::CInt(_)) |
                (Expression::CReal(_), Expression::CReal(_)) |
                (Expression::CString(_), Expression::CString(_)) => {
                    let mut updated_elements = elements.clone();
                    updated_elements.push(eval_new_element);
                    return Ok(Expression::Set(updated_elements));
                }
                _ => {
                    return Err(format!(
                        "Type {:?} does not match type {:?}",
                        eval_new_element, first_element
                    ));
                }
            }
        }
        _ => {
            return Err("Provided expression is not a list".to_string());
        }
    }
}

fn eval_pop_back_list(list: Expression, env: &Environment)
->Result<Expression,ErrorMessage>{

    let eval_list = eval(list.clone(), env)?;
    match eval_list {
        Expression::List(mut vec) => {
            if vec.is_empty(){
                return Err(String::from("Cannot pop from an empty list."));
            }
            vec.pop();
            Ok(Expression::List(vec))
        }
        _ => Err(String::from("Expected a list for pop operation.")),
    }
}

fn eval_pop_front_list(list: Expression, env: &Environment)
->Result<Expression,ErrorMessage>{
    let eval_list = eval(list.clone(), env)?;
    match eval_list {
        Expression::List(vec) => {
            if vec.is_empty(){
                return Err(String::from("Cannot pop from an empty list."));
            }
            let mut new_list = Vec::new();
            for i in 1..vec.len(){
                if let Some(aux) = vec.get(i){
                    new_list.push(aux.clone());
                }
            }
            Ok(Expression::List(new_list))
    }
        _ => Err(String::from("Expected a list for pop operation.")),
    }
}

fn eval_get_element_list(list: Expression, index: Expression, env: &Environment)
->Result<Expression,ErrorMessage>{

    let list_eval = eval(list,env)?;
    let index_eval = eval(index,env)?;

    match(list_eval,index_eval){
        (Expression::List(vec), Expression::CInt(v1))=>{
            if vec.is_empty(){
                return Err(String::from("List is empty"));
            }

            if v1 < 0 || v1 >= vec.len() as i32{
                Err(String::from("Index out of bounds"))
            }
            else{
                
                let idx = v1 as usize; 
                match vec.get(idx){
                    Some(elem) => Ok(elem.clone()),
                    None => Err(String::from("Element not found"))
                }
            }
        },
        (Expression::Tuple(elements), Expression::CInt(index)) => {
            if index < 0 || index as usize >= elements.len() {
                Err(String::from("Index out of bounds"))
            } else {
                Ok(elements[index as usize].clone())
            }
        }
        (Expression::Set(elements), Expression::CInt(index)) => {
            if index < 0 || index as usize >= elements.len() {
                Err(String::from("Index out of bounds"))
            } else {
                Ok(elements[index as usize].clone())
            }
        }

        (Expression::Set(_), _) => Err(String::from("Index must be an integer")),
        (Expression::Tuple(_), _) => Err(String::from("Index must be an integer")),
        (Expression::List(_),_)=>Err(String::from("Index must be an integer")),
        _=> Err(String::from("First argument must be a list"))
    }
}

fn eval_len_list(data_structure: Expression,env: &Environment)->Result<Expression,ErrorMessage>{
    let data_structure_type = eval(data_structure,env)?;
    match data_structure_type{
        Expression::List(vec)=>{
            let elem = vec.len() as i32;
            Ok(Expression::CInt(elem))
        }
        Expression::Tuple(vec)=>{
            let elem = vec.len() as i32;
            Ok(Expression::CInt(elem))
        }
        Expression::Set(vec)=>{
            let elem = vec.len() as i32;
            Ok(Expression::CInt(elem))
        }
        _=> Err(String::from("First argument must be a list"))
    }
}

/* Arithmetic Operations */
fn eval_binary_arith_op<F>(
    lhs: Expression,
    rhs: Expression,
    env: &Environment,
    op: F,
    error_msg: &str,
) -> Result<Expression, ErrorMessage>
where
    F: Fn(f64, f64) -> f64,
{
    let v1 = eval(lhs, env)?;
    let v2 = eval(rhs, env)?;
    match (v1, v2) {
        (Expression::CInt(v1), Expression::CInt(v2)) => {
            Ok(Expression::CInt(op(v1 as f64, v2 as f64) as i32))
        }
        (Expression::CInt(v1), Expression::CReal(v2)) => Ok(Expression::CReal(op(v1 as f64, v2))),
        (Expression::CReal(v1), Expression::CInt(v2)) => Ok(Expression::CReal(op(v1, v2 as f64))),
        (Expression::CReal(v1), Expression::CReal(v2)) => Ok(Expression::CReal(op(v1, v2))),
        _ => Err(error_msg.to_string()),
    }
}

fn add(lhs: Expression, rhs: Expression, env: &Environment) -> Result<Expression, ErrorMessage> {
    eval_binary_arith_op(
        lhs,
        rhs,
        env,
        |a, b| a + b,
        "addition '(+)' is only defined for numbers (integers and real).",
    )
}

fn sub(lhs: Expression, rhs: Expression, env: &Environment) -> Result<Expression, ErrorMessage> {
    eval_binary_arith_op(
        lhs,
        rhs,
        env,
        |a, b| a - b,
        "subtraction '(-)' is only defined for numbers (integers and real).",
    )
}

fn mul(lhs: Expression, rhs: Expression, env: &Environment) -> Result<Expression, ErrorMessage> {
    eval_binary_arith_op(
        lhs,
        rhs,
        env,
        |a, b| a * b,
        "multiplication '(*)' is only defined for numbers (integers and real).",
    )
}

fn div(lhs: Expression, rhs: Expression, env: &Environment) -> Result<Expression, ErrorMessage> {
    eval_binary_arith_op(
        lhs,
        rhs,
        env,
        |a, b| a / b,
        "division '(/)' is only defined for numbers (integers and real).",
    )
}

fn rmd(lhs: Expression, rhs: Expression, env: &Environment) -> Result<Expression, ErrorMessage> {
    eval_binary_arith_op(
        lhs,
        rhs,
        env,
        |a, b| a % b,
        "Remainder operation '(%)' is only defined for numbers (integers and real).",
    )
}
/* Boolean Expressions */
fn eval_binary_boolean_op<F>(
    lhs: Expression,
    rhs: Expression,
    env: &Environment,
    op: F,
    error_msg: &str,
) -> Result<Expression, ErrorMessage>
where
    F: Fn(bool, bool) -> Expression,
{
    let v1 = eval(lhs, env)?;
    let v2 = eval(rhs, env)?;
    match (v1, v2) {
        (Expression::CTrue, Expression::CTrue) => Ok(op(true, true)),
        (Expression::CTrue, Expression::CFalse) => Ok(op(true, false)),
        (Expression::CFalse, Expression::CTrue) => Ok(op(false, true)),
        (Expression::CFalse, Expression::CFalse) => Ok(op(false, false)),
        _ => Err(error_msg.to_string()),
    }
}

fn and(lhs: Expression, rhs: Expression, env: &Environment) -> Result<Expression, ErrorMessage> {
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

fn or(lhs: Expression, rhs: Expression, env: &Environment) -> Result<Expression, ErrorMessage> {
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

fn not(lhs: Expression, env: &Environment) -> Result<Expression, ErrorMessage> {
    let v = eval(lhs, env)?;
    match v {
        Expression::CTrue => Ok(Expression::CFalse),
        Expression::CFalse => Ok(Expression::CTrue),
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
) -> Result<Expression, ErrorMessage>
where
    F: Fn(f64, f64) -> Expression,
{
    let v1 = eval(lhs, env)?;
    let v2 = eval(rhs, env)?;
    match (v1, v2) {
        (Expression::CInt(v1), Expression::CInt(v2)) => Ok(op(v1 as f64, v2 as f64)),
        (Expression::CInt(v1), Expression::CReal(v2)) => Ok(op(v1 as f64, v2)),
        (Expression::CReal(v1), Expression::CInt(v2)) => Ok(op(v1, v2 as f64)),
        (Expression::CReal(v1), Expression::CReal(v2)) => Ok(op(v1, v2)),
        _ => Err(error_msg.to_string()),
    }
}

fn eq(lhs: Expression, rhs: Expression, env: &Environment) -> Result<Expression, ErrorMessage> {
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

fn gt(lhs: Expression, rhs: Expression, env: &Environment) -> Result<Expression, ErrorMessage> {
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

fn lt(lhs: Expression, rhs: Expression, env: &Environment) -> Result<Expression, ErrorMessage> {
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

fn gte(lhs: Expression, rhs: Expression, env: &Environment) -> Result<Expression, ErrorMessage> {
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

fn lte(lhs: Expression, rhs: Expression, env: &Environment) -> Result<Expression, ErrorMessage> {
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

pub fn execute(stmt: Statement, env: Environment) -> Result<Environment, ErrorMessage> {
    match stmt {
        Statement::Assignment(name, exp) => {
            let value = eval(*exp, &env)?;
            let mut new_env = env;
            new_env.insert(name.clone(), value);
            Ok(new_env.clone())
        }
        Statement::IfThenElse(cond, stmt_then, stmt_else) => {
            let value = eval(*cond, &env)?;
            match value {
                Expression::CTrue => execute(*stmt_then, env),
                Expression::CFalse => match stmt_else {
                    Some(else_statement) => execute(*else_statement, env),
                    None => Ok(env),
                },
                _ => Err(String::from("expecting a boolean value.")),
            }
        }
        Statement::While(cond, stmt) => {
            let mut value = eval(*cond.clone(), &env)?;
            let mut new_env = env;
            while value == Expression::CTrue {
                new_env = execute(*stmt.clone(), new_env.clone())?;
                value = eval(*cond.clone(), &new_env.clone())?;
            }

            Ok(new_env)
        }
        Statement::Sequence(s1, s2) => execute(*s1, env).and_then(|new_env| execute(*s2, new_env)),
        _ => Err(String::from("not implemented yet")),
    }
}

#[cfg(test)]
mod tests {

    //use std::hash::Hash;
    use super::*;
    use crate::ir::ast::Expression::*;
    use crate::ir::ast::Statement::*;
    use approx::relative_eq;

    #[test]
    fn eval_create_valid_set() {
        let env = HashMap::new();
        let set = Expression::Set(vec![Expression::CInt(1), Expression::CInt(2), Expression::CInt(3)]);
        let result = eval_create_set(vec![Expression::CInt(1), Expression::CInt(2), Expression::CInt(3)], &env);
        assert_eq!(result, Ok(set));
    }

    #[test]
    fn eval_len_set(){
        let env = HashMap::new();
        let set = Expression::Set(vec![
            Expression::CString("Rust".to_string()),
            Expression::CString("Java".to_string()),
            Expression::CString("Python".to_string())]);
        let tam_set = eval(Expression::Len(Box::new(set)),&env).unwrap();

        assert_eq!(tam_set,Expression::CInt(3));
    }

    #[test]
    fn eval_get_element_set(){
        let env = HashMap::new();
        let idx = Expression::CInt(1);
        let set = Expression::Set(vec!
            [Expression::CInt(5),Expression::CInt(8)]);

        let elem = eval(Expression::Get(Box::new(set),Box::new(idx)),&env).unwrap();

        assert_eq!(elem,Expression::CInt(8));
    }

    #[test]
    fn eval_append_set() {
        let mut set = Expression::Set(vec![
            Expression::CInt(5), Expression::CInt(10), Expression::CInt(15)]);
        
        let elem = Expression::CInt(20);
        let env = HashMap::new();
        
        set = eval(Expression::Append(Box::new(set), Box::new(elem)), &env).unwrap();
        
        assert_eq!(
            set,
            Expression::Set(vec![
                Expression::CInt(5), 
                Expression::CInt(10), 
                Expression::CInt(15), 
                Expression::CInt(20)
            ])
        );
    }

    // #[test]
    // fn eval_create_dict_t() {
    //     let elements = vec![
    //         (Expression::CString("chave1".to_string()), Expression::CInt(10)),
    //         (Expression::CString("chave2".to_string()), Expression::CInt(20)),
    //     ];
    
    //     let result = eval_create_dict(Some(elements.clone()), &HashMap::new());
    //     assert_eq!(result, Ok(Expression::Dict(Some(elements))));
    // }
    
    #[test]
    fn eval_create_hash_t() {
        let mut elements = HashMap::new();
        elements.insert(Expression::CString("chave1".to_string()), Expression::CInt(10));
        elements.insert(Expression::CString("chave2".to_string()), Expression::CInt(20));
    
        let result = eval_create_hash(Some(elements.clone()), &HashMap::new());
        assert_eq!(result, Ok(Expression::Hash(Some(elements))));
    }

    // #[test]
    // fn eval_get_dict_t() {
    //     let elements = vec![
    //         (Expression::CString("chave1".to_string()), Expression::CInt(10)),
    //         (Expression::CString("chave2".to_string()), Expression::CInt(20)),
    //     ];
    //     let dict = Expression::Dict(Some(elements));

    //     let key = Expression::CString("chave1".to_string());
    //     let result = eval_get_dict(dict, key, &HashMap::new());
    //     assert_eq!(result, Ok(Expression::CInt(10)));
    // }

    #[test]
    fn eval_get_hash_t() {
        let mut map = HashMap::new();
        map.insert(Expression::CString("chave1".to_string()), Expression::CInt(10));
        map.insert(Expression::CString("chave2".to_string()), Expression::CInt(20));
        
        let hash = Expression::Hash(Some(map));

        let key = Expression::CString("chave1".to_string());
        let result = eval_get_hash(hash, key, &HashMap::new());
        assert_eq!(result, Ok(Expression::CInt(10)));
    }

    // #[test]
    // fn eval_set_dict_t() {
    //     let elements = vec![
    //         (Expression::CString("chave1".to_string()), Expression::CInt(10)),
    //         (Expression::CString("chave2".to_string()), Expression::CInt(20)),
    //     ];
    //     let dict = Expression::Dict(Some(elements));

    //     let key = Expression::CString("chave1".to_string());
    //     let value = Expression::CInt(30);
    //     let result = eval_set_dict(dict, key, value, &HashMap::new());

    //     let expected_elements = vec![
    //         (Expression::CString("chave1".to_string()), Expression::CInt(30)),
    //         (Expression::CString("chave2".to_string()), Expression::CInt(20)),
    //     ];
    //     assert_eq!(result, Ok(Expression::Dict(Some(expected_elements))));
    // }

    #[test]
    fn eval_set_hash_t() {
        let mut map = HashMap::new();
        map.insert(Expression::CString("chave1".to_string()), Expression::CInt(10));
        map.insert(Expression::CString("chave2".to_string()), Expression::CInt(20));

        let hash = Expression::Hash(Some(map));

        let key = Expression::CString("chave1".to_string());
        let value = Expression::CInt(30);
        let result = eval_set_hash(hash, key, value, &HashMap::new());

        let mut expected_map = HashMap::new();
        expected_map.insert(Expression::CString("chave1".to_string()), Expression::CInt(30));
        expected_map.insert(Expression::CString("chave2".to_string()), Expression::CInt(20));

        assert_eq!(result, Ok(Expression::Hash(Some(expected_map))));
    }

    // #[test]
    // fn eval_remove_dict_t() {
    //     let elements = vec![
    //         (Expression::CString("chave1".to_string()), Expression::CInt(10)),
    //         (Expression::CString("chave2".to_string()), Expression::CInt(20)),
    //     ];
    //     let dict = Expression::Dict(Some(elements));

    //     let key = Expression::CString("chave1".to_string());
    //     let result = eval_remove_dict(dict, key, &HashMap::new());

    //     let expected_elements = vec![
    //         (Expression::CString("chave2".to_string()), Expression::CInt(20)),
    //     ];
    //     assert_eq!(result, Ok(Expression::Dict(Some(expected_elements))));
    // }

    #[test]
    fn eval_remove_hash_t() {
        let mut map = HashMap::new();
        map.insert(Expression::CString("chave1".to_string()), Expression::CInt(10));
        map.insert(Expression::CString("chave2".to_string()), Expression::CInt(20));

        let hash = Expression::Hash(Some(map));

        let key = Expression::CString("chave1".to_string());
        let result = eval_remove_hash(hash, key, &HashMap::new());

        let mut expected_map = HashMap::new();
        expected_map.insert(Expression::CString("chave2".to_string()), Expression::CInt(20));

        assert_eq!(result, Ok(Expression::Hash(Some(expected_map))));
    }

    #[test]
    fn eval_create_tuple(){
        let tuple = Tuple(vec![CString("Rust".to_string()),CInt(5),CReal(5.5)]);
        let env = HashMap::new();
        assert_eq!(eval(tuple,&env).unwrap(),
        Tuple(vec![CString("Rust".to_string()),CInt(5),CReal(5.5)]));
    }

    #[test]
    fn eval_len_tuple(){
        let env = HashMap::new();
        let tuple = Expression::Tuple(vec![
            Expression::CString("Rust".to_string()),
            Expression::CString("Java".to_string()),
            Expression::CString("Python".to_string())]);
        let tam_tuple = eval(Expression::Len(Box::new(tuple)),&env).unwrap();

        assert_eq!(tam_tuple,Expression::CInt(3));
    }

    #[test]
    fn eval_get_element_tuple(){
        let env = HashMap::new();
        let idx = Expression::CInt(1);
        let tuple = Expression::Tuple(vec!
            [Expression::CInt(5),Expression::CInt(8)]);

        let elem = eval(Expression::Get(Box::new(tuple),Box::new(idx)),&env).unwrap();

        assert_eq!(elem,Expression::CInt(8));
    }

    #[test]
    fn eval_append_list() {
        let mut list = Expression::List(vec![
            Expression::CInt(5), Expression::CInt(10), Expression::CInt(15)]);
        
        let elem = Expression::CInt(20);
        let env = HashMap::new();
        
        list = eval(Expression::Append(Box::new(list), Box::new(elem)), &env).unwrap();
        
        assert_eq!(
            list,
            Expression::List(vec![
                Expression::CInt(5), 
                Expression::CInt(10), 
                Expression::CInt(15), 
                Expression::CInt(20)
            ])
        );
    }

    #[test]
    fn eval_append_real_list() {

        let mut list = Expression::List(
            vec![Expression::CReal(5.85),]);
        
        let elem = Expression::CReal(1.55);
        let env = HashMap::new();
        
        list = eval(Expression::Append(Box::new(list), Box::new(elem)), &env).unwrap();
        
        assert_eq!(
            list,
            Expression::List(vec![
                Expression::CReal(5.85), 
                Expression::CReal(1.55), 
            ])
        );
    }

    #[test]
    fn eval_append_list_different_types(){

        let list = Expression::List(vec![Expression::CInt(5)]);
        let env = HashMap::new();
        let elem = Expression::CReal(5.1);
        let result = eval(Expression::Append(Box::new(list), Box::new(elem)),&env);

        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err(),
            "Type CReal(5.1) does not match type CInt(5)"
        );
    }

    #[test]
    fn eval_append_list_str(){
        let mut list = List(vec![]);
        let env = HashMap::new();
        let elem = Expression::CString("Rust".to_string());
        list = eval(Append(Box::new(list), Box::new(elem)),&env).unwrap();

        assert_eq!(
            list,
            Expression::List(vec![
                Expression::CString("Rust".to_string()), 
            ])
        );
    }

    #[test]
    fn eval_concat_valid_list_with_list(){
        let env = HashMap::new();
        let list1 = List(vec![CInt(5)]);
        let list2 = List(vec![CInt(10)]);
        let list3 = eval(Concat(Box::new(list1),Box::new(list2)),&env).unwrap();

        assert_eq!(list3,List(vec![CInt(5),CInt(10)]));
    }

    #[test]
    fn eval_concat_valid_list_with_empty_list(){
        let env = HashMap::new();
        let list1 = List(vec![CInt(5)]);
        let list2 = List(vec![]);
        let list3 = eval(Concat(Box::new(list1),Box::new(list2)),&env).unwrap();

        assert_eq!(list3,List(vec![CInt(5)]));
    }

    #[test]
    fn eval_get_element_list(){
        let env = HashMap::new();
        let idx = Expression::CInt(1);
        let list = Expression::List(vec!
            [Expression::CInt(5),Expression::CInt(8)]);

        let elem = eval(Expression::Get(Box::new(list),Box::new(idx)),&env).unwrap();

        assert_eq!(elem,Expression::CInt(8));
    }

    #[test]
    fn eval_get_invalid_element_list(){
        let env = HashMap::new();
        let idx = Expression::CInt(4);
        let list = Expression::List(vec!
            [Expression::CInt(5),Expression::CInt(8)]);

        let result = eval(Expression::Get(Box::new(list),Box::new(idx)),&env);

        assert!(result.is_err());
        assert_eq!(result.unwrap_err(),
        "Index out of bounds".to_string())
    }

    #[test]
    fn eval_get_not_integer_index(){
        let env = HashMap::new();
        let idx = Expression::CReal(0.5);
        let list = Expression::List(vec!
            [Expression::CInt(5),Expression::CInt(8)]);

        let result = eval(Expression::Get(Box::new(list),Box::new(idx)),&env);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(),
        "Index must be an integer".to_string())
    }

    #[test]
    fn eval_len_list_valid(){
        let env = HashMap::new();
        let list = Expression::List(vec![
            Expression::CString("Rust".to_string()),
            Expression::CString("Java".to_string()),
            Expression::CString("Python".to_string())]);
        let tam_list = eval(Expression::Len(Box::new(list)),&env).unwrap();

        assert_eq!(tam_list,Expression::CInt(3));
    }

    #[test]
    fn pop_back_valid_list(){
        let env = HashMap::new();

        let mut list = List(vec![CInt(5),CInt(10)]);
        
        list = eval(PopBack(Box::new(list.clone())),&env).unwrap();

        assert_eq!(list,List(vec![CInt(5)]));
    }

    #[test]
    fn pop_back_valid_empty_list(){
        let env = HashMap::new();

        let list = List(vec![]);
        
        let result = eval(PopBack(Box::new(list.clone())),&env);

        assert!(result.is_err());
    }

    #[test]
    fn eval_pop_front_valid_list(){
        let mut list = List(vec![CInt(5),CInt(10)]);
        let env = HashMap::new();
        list = eval(PopFront(Box::new(list.clone())),&env).unwrap();
        assert_eq!(list,List(vec![CInt(10)]));
    }

    #[test]
    fn eval_constant() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let c20 = CInt(20);

        assert_eq!(eval(c10, &env), Ok(CInt(10)));
        assert_eq!(eval(c20, &env), Ok(CInt(20)));
    }

    #[test]
    fn eval_add_expression1() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let c20 = CInt(20);
        let add1 = Add(Box::new(c10), Box::new(c20));
        assert_eq!(eval(add1, &env), Ok(CInt(30)));
    }

    #[test]
    fn eval_add_expression2() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let c20 = CInt(20);
        let c30 = CInt(30);
        let add1 = Add(Box::new(c10), Box::new(c20));
        let add2 = Add(Box::new(add1), Box::new(c30));
        assert_eq!(eval(add2, &env), Ok(CInt(60)));
    }

    #[test]
    fn eval_add_expression3() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let c20 = CReal(20.5);
        let add1 = Add(Box::new(c10), Box::new(c20));
        assert_eq!(eval(add1, &env), Ok(CReal(30.5)));
    }

    #[test]
    fn eval_sub_expression1() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let c20 = CInt(20);
        let sub1 = Sub(Box::new(c20), Box::new(c10));
        assert_eq!(eval(sub1, &env), Ok(CInt(10)));
    }

    #[test]
    fn eval_sub_expression2() {
        let env = HashMap::new();
        let c100 = CInt(100);
        let c200 = CInt(300);
        let sub1 = Sub(Box::new(c200), Box::new(c100));
        assert_eq!(eval(sub1, &env), Ok(CInt(200)));
    }

    #[test]
    fn eval_sub_expression3() {
        let env = HashMap::new();
        let c100 = CReal(100.5);
        let c300 = CInt(300);
        let sub1 = Sub(Box::new(c300), Box::new(c100));
        assert_eq!(eval(sub1, &env), Ok(CReal(199.5)));
    }

    #[test]
    fn eval_mul_expression1() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let c20 = CInt(20);
        let mul1 = Mul(Box::new(c10), Box::new(c20));
        assert_eq!(eval(mul1, &env), Ok(CInt(200)));
    }

    #[test]
    fn eval_mul_expression2() {
        let env = HashMap::new();
        let c10 = CReal(10.5);
        let c20 = CInt(20);
        let mul1 = Mul(Box::new(c10), Box::new(c20));
        assert_eq!(eval(mul1, &env), Ok(CReal(210.0)));
    }

    #[test]
    fn eval_div_expression1() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let c20 = CInt(20);
        let div1 = Div(Box::new(c20), Box::new(c10));
        assert_eq!(eval(div1, &env), Ok(CInt(2)));
    }

    #[test]
    fn eval_div_expression2() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let c3 = CInt(3);
        let div1 = Div(Box::new(c10), Box::new(c3));
        assert_eq!(eval(div1, &env), Ok(CInt(3)));
    }

    #[test]
    fn eval_div_expression3() {
        let env = HashMap::new();
        let c3 = CInt(3);
        let c21 = CInt(21);
        let div1 = Div(Box::new(c21), Box::new(c3));
        assert_eq!(eval(div1, &env), Ok(CInt(7)));
    }

    #[test]
    fn eval_div_expression4() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let c3 = CReal(3.0);
        let div1 = Div(Box::new(c10), Box::new(c3));
        let res = eval(div1, &env);
        match res {
            Ok(CReal(v)) => assert!(relative_eq!(v, 3.3333333333333335, epsilon = f64::EPSILON)),
            Err(msg) => assert!(false, "{}", msg),
            _ => assert!(false, "Not expected."),
        }
    }

    #[test]
    fn eval_variable() {
        let env = HashMap::from([(String::from("x"), CInt(10)), (String::from("y"), CInt(20))]);
        let v1 = Var(String::from("x"));
        let v2 = Var(String::from("y"));
        assert_eq!(eval(v1, &env), Ok(CInt(10)));
        assert_eq!(eval(v2, &env), Ok(CInt(20)));
    }

    #[test]
    fn eval_expression_with_variables() {
        let env = HashMap::from([(String::from("a"), CInt(5)), (String::from("b"), CInt(3))]);
        let expr = Mul(
            Box::new(Var(String::from("a"))),
            Box::new(Add(Box::new(Var(String::from("b"))), Box::new(CInt(2)))),
        );
        assert_eq!(eval(expr, &env), Ok(CInt(25)));
    }

    #[test]
    fn eval_nested_expressions() {
        let env = HashMap::new();
        let expr = Add(
            Box::new(Mul(Box::new(CInt(2)), Box::new(CInt(3)))),
            Box::new(Sub(Box::new(CInt(10)), Box::new(CInt(4)))),
        );
        assert_eq!(eval(expr, &env), Ok(CInt(12)));
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
        let assign_stmt = Assignment(String::from("x"), Box::new(CInt(42)));

        match execute(assign_stmt, env) {
            Ok(new_env) => assert_eq!(new_env.get("x"), Some(&CInt(42))),
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn eval_summation() {
        /*
         * (a test case for the following program)
         *
         * > x = 10
         * > y = 0
         * > while x >= 0:
         * >   y = y + x
         * >   x = x - 1
         *
         * After executing this program, 'x' must be zero and
         * 'y' must be 55.
         */
        let env = HashMap::new();

        let a1 = Assignment(String::from("x"), Box::new(CInt(10)));
        let a2 = Assignment(String::from("y"), Box::new(CInt(0)));
        let a3 = Assignment(
            String::from("y"),
            Box::new(Add(
                Box::new(Var(String::from("y"))),
                Box::new(Var(String::from("x"))),
            )),
        );
        let a4 = Assignment(
            String::from("x"),
            Box::new(Sub(Box::new(Var(String::from("x"))), Box::new(CInt(1)))),
        );

        let seq1 = Sequence(Box::new(a3), Box::new(a4));

        let while_statement = While(
            Box::new(GT(Box::new(Var(String::from("x"))), Box::new(CInt(0)))),
            Box::new(seq1),
        );

        let seq2 = Sequence(Box::new(a2), Box::new(while_statement));
        let program = Sequence(Box::new(a1), Box::new(seq2));

        match execute(program, env) {
            Ok(new_env) => {
                assert_eq!(new_env.get("y"), Some(&CInt(55)));
                assert_eq!(new_env.get("x"), Some(&CInt(0)));
            }
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn eval_simple_if_then_else() {
        /*
         * Test for simple if-then-else statement
         *
         * > x = 10
         * > if x > 5:
         * >   y = 1
         * > else:
         * >   y = 0
         *
         * After executing, 'y' should be 1.
         */
        let env = HashMap::new();

        let condition = GT(Box::new(Var(String::from("x"))), Box::new(CInt(5)));
        let then_stmt = Assignment(String::from("y"), Box::new(CInt(1)));
        let else_stmt = Assignment(String::from("y"), Box::new(CInt(0)));

        let if_statement = IfThenElse(
            Box::new(condition),
            Box::new(then_stmt),
            Some(Box::new(else_stmt)),
        );

        let setup_stmt = Assignment(String::from("x"), Box::new(CInt(10)));
        let program = Sequence(Box::new(setup_stmt), Box::new(if_statement));

        match execute(program, env) {
            Ok(new_env) => assert_eq!(new_env.get("y"), Some(&CInt(1))),
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn eval_if_then_optional_else() {
        /*
         * Test for simple if-then-else statement
         *
         * > x = 1
         * > y = 0
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
        let second_then_stmt = Assignment(String::from("y"), Box::new(CInt(5)));

        let second_if_stmt =
            IfThenElse(Box::new(second_condition), Box::new(second_then_stmt), None);

        let else_setup_stmt = Assignment(String::from("y"), Box::new(CInt(2)));
        let else_stmt = Sequence(Box::new(else_setup_stmt), Box::new(second_if_stmt));

        let first_condition = EQ(
            Box::new(Var(String::from("x"))),
            Box::new(Var(String::from("y"))),
        );
        let first_then_stmt = Assignment(String::from("y"), Box::new(CInt(1)));

        let first_if_stmt = IfThenElse(
            Box::new(first_condition),
            Box::new(first_then_stmt),
            Some(Box::new(else_stmt)),
        );

        let second_assignment = Assignment(String::from("y"), Box::new(CInt(0)));
        let setup_stmt = Sequence(Box::new(second_assignment), Box::new(first_if_stmt));

        let first_assignment = Assignment(String::from("x"), Box::new(CInt(1)));
        let program = Sequence(Box::new(first_assignment), Box::new(setup_stmt));

        match execute(program, env) {
            Ok(new_env) => assert_eq!(new_env.get("y"), Some(&CInt(2))),
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn eval_while_using_rmd() {
        /*
         *   Test for remainder operator using while
         *
         *   x = 1
         *   y = 1800
         *   z = 0
         *   while x*x <= y:
         *       if y % x == 0:
         *           if x % 2 == 0:
         *               z = z + 1
         *           if (y / x) % 2 == 0:
         *               z = z + 1
         *       x = x + 1
         *
         *   After processing 'x' must be 43
         *   and 'z' must be 27
         */

        let env = HashMap::new();
        let a1 = Assignment(String::from("x"), Box::new(CInt(1)));
        let a2 = Assignment(String::from("y"), Box::new(CInt(1800)));
        let a3 = Assignment(String::from("z"), Box::new(CInt(0)));
        let a4 = Assignment(
            String::from("z"),
            Box::new(Add(Box::new(Var(String::from("z"))), Box::new(CInt(1)))),
        );
        let a5 = Assignment(
            String::from("z"),
            Box::new(Add(Box::new(Var(String::from("z"))), Box::new(CInt(1)))),
        );
        let a6 = Assignment(
            String::from("x"),
            Box::new(Add(Box::new(Var(String::from("x"))), Box::new(CInt(1)))),
        );

        let if_statement1 = IfThenElse(
            Box::new(EQ(
                Box::new(Rmd(Box::new(Var(String::from("x"))), Box::new(CInt(2)))),
                Box::new(CInt(0)),
            )),
            Box::new(a4),
            None,
        );
        let if_statement2 = IfThenElse(
            Box::new(EQ(
                Box::new(Rmd(
                    Box::new(Div(
                        Box::new(Var(String::from("y"))),
                        Box::new(Var(String::from("x"))),
                    )),
                    Box::new(CInt(2)),
                )),
                Box::new(CInt(0)),
            )),
            Box::new(a5),
            None,
        );

        let seq = Sequence(Box::new(if_statement1), Box::new(if_statement2));
        let if_statement = IfThenElse(
            Box::new(EQ(
                Box::new(Rmd(
                    Box::new(Var(String::from("y"))),
                    Box::new(Var(String::from("x"))),
                )),
                Box::new(CInt(0)),
            )),
            Box::new(seq),
            None,
        );

        let seq1 = Sequence(Box::new(if_statement), Box::new(a6));

        let while_statement = While(
            Box::new(LTE(
                Box::new(Mul(
                    Box::new(Var(String::from("x"))),
                    Box::new(Var(String::from("x"))),
                )),
                Box::new(Var(String::from("y"))),
            )),
            Box::new(seq1),
        );

        let seq2 = Sequence(Box::new(Sequence(Box::new(a1), Box::new(a2))), Box::new(a3));

        let program = Sequence(Box::new(seq2), Box::new(while_statement));

        match execute(program, env) {
            Ok(new_env) => {
                assert_eq!(new_env.get("x"), Some(&CInt(43)));
                assert_eq!(new_env.get("z"), Some(&CInt(27)));
            }
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn eval_while_with_if() {
        /*
         *   Test for more complex while statement
         *
         *   x = 1
         *   y = 16
         *   z = 16
         *   a = 0
         *   while x <= y && a*a != z:
         *       m = (x + y) / 2
         *       if m*m <= z:
         *          a = m
         *          x = mid + 1
         *       else:
         *          y = mid - 1
         *
         *   After executing this program, 'x' must be 5,
         *   'y' must be 7 and 'a' must be 4
         */

        let env = HashMap::new();

        let a1 = Assignment(String::from("x"), Box::new(CInt(1)));
        let a2 = Assignment(String::from("y"), Box::new(CInt(16)));
        let a3 = Assignment(String::from("z"), Box::new(CInt(16)));
        let a4 = Assignment(String::from("a"), Box::new(CInt(0)));
        let a5 = Assignment(
            String::from("m"),
            Box::new(Div(
                Box::new(Add(
                    Box::new(Var(String::from("x"))),
                    Box::new(Var(String::from("y"))),
                )),
                Box::new(CInt(2)),
            )),
        );
        let a6 = Assignment(String::from("a"), Box::new(Var(String::from("m"))));
        let a7 = Assignment(
            String::from("x"),
            Box::new(Add(Box::new(Var(String::from("m"))), Box::new(CInt(1)))),
        );
        let a8 = Assignment(
            String::from("y"),
            Box::new(Sub(Box::new(Var(String::from("m"))), Box::new(CInt(1)))),
        );

        let seq = Sequence(Box::new(a6), Box::new(a7));

        let if_statement: Statement = IfThenElse(
            Box::new(LTE(
                Box::new(Mul(
                    Box::new(Var(String::from("m"))),
                    Box::new(Var(String::from("m"))),
                )),
                Box::new(Var(String::from("z"))),
            )),
            Box::new(seq),
            Some(Box::new(a8)),
        );

        let while_statement = While(
            Box::new(And(
                Box::new(LTE(
                    Box::new(Var(String::from("x"))),
                    Box::new(Var(String::from("y"))),
                )),
                Box::new(Not(Box::new(EQ(
                    Box::new(Mul(
                        Box::new(Var(String::from("a"))),
                        Box::new(Var(String::from("a"))),
                    )),
                    Box::new(Var(String::from("z"))),
                )))),
            )),
            Box::new(Sequence(Box::new(a5), Box::new(if_statement))),
        );

        let seq1 = Sequence(
            Box::new(a1),
            Box::new(Sequence(
                Box::new(a2),
                Box::new(Sequence(Box::new(a3), Box::new(a4))),
            )),
        );

        let program = Sequence(Box::new(seq1), Box::new(while_statement));

        match execute(program, env) {
            Ok(new_env) => {
                assert_eq!(new_env.get("x"), Some(&CInt(5)));
                assert_eq!(new_env.get("y"), Some(&CInt(7)));
                assert_eq!(new_env.get("a"), Some(&CInt(4)));
            }
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn eval_while_with_boolean() {
        /*  Test for while statement using booleans
         *
         *   x = true
         *   y = 1
         *   while x:
         *       if y > 1e9:
         *           x = false
         *       else:
         *           y = y * 2
         *
         *   After executing this program 'y' must be equal 1073741824
         *   and 'x' must be equal false
         */

        let env = HashMap::new();
        let a1 = Assignment(String::from("x"), Box::new(CTrue));
        let a2 = Assignment(String::from("y"), Box::new(CInt(1)));
        let a3 = Assignment(String::from("x"), Box::new(CFalse));
        let a4 = Assignment(
            String::from("y"),
            Box::new(Mul(Box::new(Var(String::from("y"))), Box::new(CInt(2)))),
        );

        let if_then_else_statement = IfThenElse(
            Box::new(GT(
                Box::new(Var(String::from("y"))),
                Box::new(CInt(1000000000)),
            )),
            Box::new(a3),
            Some(Box::new(a4)),
        );

        let while_statement = While(
            Box::new(Var(String::from("x"))),
            Box::new(if_then_else_statement),
        );

        let program = Sequence(
            Box::new(a1),
            Box::new(Sequence(Box::new(a2), Box::new(while_statement))),
        );

        match execute(program, env) {
            Ok(new_env) => {
                assert_eq!(new_env.get("x"), Some(&CFalse));
                assert_eq!(new_env.get("y"), Some(&CInt(1073741824)));
            }
            Err(s) => assert!(false, "{}", s),
        }
    }

    #[test]
    fn eval_complex_sequence() {
        /*
         * Sequence with multiple assignments and expressions
         *
         * > x = 5
         * > y = 0
         * > z = 2 * x + 3
         *
         * After executing, 'x' should be 5, 'y' should be 0, and 'z' should be 13.
         */
        let env = HashMap::new();

        let a1 = Assignment(String::from("x"), Box::new(CInt(5)));
        let a2 = Assignment(String::from("y"), Box::new(CInt(0)));
        let a3 = Assignment(
            String::from("z"),
            Box::new(Add(
                Box::new(Mul(Box::new(CInt(2)), Box::new(Var(String::from("x"))))),
                Box::new(CInt(3)),
            )),
        );

        let program = Sequence(Box::new(a1), Box::new(Sequence(Box::new(a2), Box::new(a3))));

        match execute(program, env) {
            Ok(new_env) => {
                assert_eq!(new_env.get("x"), Some(&CInt(5)));
                assert_eq!(new_env.get("y"), Some(&CInt(0)));
                assert_eq!(new_env.get("z"), Some(&CInt(13)));
            }
            Err(s) => assert!(false, "{}", s),
        }
    }
}