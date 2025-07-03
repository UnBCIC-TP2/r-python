use crate::ir::ast::{Expression, Function, Type, FormalArgument};
use crate::interpreter::expression_eval::ExpressionResult;
use std::collections::HashMap;
use std::io::{self, Write};

pub fn register_builtins(map: &mut HashMap<String, Function>) {
    map.insert("input".to_string(), Function {
        name: "input".to_string(),
        kind: Type::TFunction(Box::new(Some(Type::TString)), vec![Type::TMaybe(Box::new(Type::TString))]),
        params: vec![FormalArgument::new("prompt".to_string(), Type::TMaybe(Box::new(Type::TString)))],
        body: None,
        builtin: Some(builtin_input),
    });
    map.insert("print".to_string(), Function {
        name: "print".to_string(),
        kind: Type::TFunction(Box::new(Some(Type::TVoid)), vec![Type::TAny]),
        params: vec![FormalArgument::new("value".to_string(), Type::TAny)],
        body: None,
        builtin: Some(builtin_print),
    });
}

pub fn builtin_input(args: &[Expression]) -> Result<ExpressionResult, String> {
    let prompt = if let Some(Expression::CString(msg)) = args.get(0) {
        msg.as_str()
    } else {
        ""
    };
    print!("{}", prompt);
    io::stdout().flush().unwrap();
    let mut input = String::new();
    io::stdin().read_line(&mut input).map_err(|e| e.to_string())?;
    let input = input.trim_end_matches(['\n', '\r']).to_string();
    Ok(ExpressionResult::Value(Expression::CString(input)))
}

pub fn builtin_print(args: &[Expression]) -> Result<ExpressionResult, String> {
    let message = args.get(0).cloned().unwrap_or(Expression::CString("".to_string()));
    if let Expression::CString(s) = &message {
        println!("{}", s);
    } else {
        println!("{:?}", message);
    }
    Ok(ExpressionResult::Value(Expression::CVoid))
}
