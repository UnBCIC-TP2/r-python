use crate::ir::ast::{EnvValue, Expression, Function, Statement, Type};
use std::collections::HashMap;

pub fn load_string_stdlib() -> HashMap<String, Function> {
    let mut string_stdlib = HashMap::new();

    string_stdlib.insert(
        "str_upper".to_string(),
        Function {
            kind: Type::TString,
            params: Some(vec![("s".to_string(), Type::TString)]),
            body: Box::new(Statement::Return(Box::new(Expression::MetaExp(
                str_upper_impl,
                vec![Expression::Var("s".to_string())],
                Type::TString,
            )))),
        },
    );

    string_stdlib.insert(
        "str_lower".to_string(),
        Function {
            kind: Type::TString,
            params: Some(vec![("s".to_string(), Type::TString)]),
            body: Box::new(Statement::Return(Box::new(Expression::MetaExp(
                str_lower_impl,
                vec![Expression::Var("s".to_string())],
                Type::TString,
            )))),
        },
    );

    string_stdlib.insert(
        "str_length".to_string(),
        Function {
            kind: Type::TInteger,
            params: Some(vec![("s".to_string(), Type::TString)]),
            body: Box::new(Statement::Return(Box::new(Expression::MetaExp(
                str_length_impl,
                vec![Expression::Var("s".to_string())],
                Type::TInteger,
            )))),
        },
    );

    string_stdlib.insert(
        "str_reverse".to_string(),
        Function {
            kind: Type::TString,
            params: Some(vec![("s".to_string(), Type::TString)]),
            body: Box::new(Statement::Return(Box::new(Expression::MetaExp(
                str_reverse_impl,
                vec![Expression::Var("s".to_string())],
                Type::TString,
            )))),
        },
    );

    string_stdlib.insert(
        "cont_chars".to_string(),
        Function {
            kind: Type::TInteger,
            params: Some(vec![
                ("s".to_string(), Type::TString),
                ("c".to_string(), Type::TString),
            ]),
            body: Box::new(Statement::Return(Box::new(Expression::MetaExp(
                cont_chars_impl,
                vec![
                    Expression::Var("s".to_string()),
                    Expression::Var("c".to_string()),
                ],
                Type::TInteger,
            )))),
        },
    );

    string_stdlib.insert(
        "filter_out_char".to_string(),
        Function {
            kind: Type::TString,
            params: Some(vec![
                ("s".to_string(), Type::TString),
                ("c".to_string(), Type::TString),
            ]),
            body: Box::new(Statement::Return(Box::new(Expression::MetaExp(
                filter_out_char_impl,
                vec![
                    Expression::Var("s".to_string()),
                    Expression::Var("c".to_string()),
                ],
                Type::TString,
            )))),
        },
    );

    string_stdlib.insert(
        "replace".to_string(),
        Function {
            kind: Type::TString,
            params: Some(vec![
                ("s".to_string(), Type::TString),
                ("old".to_string(), Type::TString),
                ("new".to_string(), Type::TString),
                ("count".to_string(), Type::TInteger),
            ]),
            body: Box::new(Statement::Return(Box::new(Expression::MetaExp(
                replace_impl,
                vec![
                    Expression::Var("s".to_string()),
                    Expression::Var("old".to_string()),
                    Expression::Var("new".to_string()),
                    Expression::Var("count".to_string()),
                ],
                Type::TString,
            )))),
        },
    );

    string_stdlib
}

pub fn str_upper_impl(args: Vec<EnvValue>) -> Result<EnvValue, String> {
    if args.len() != 1 {
        return Err("str_upper expects exactly one argument".to_string());
    }

    if let EnvValue::Exp(Expression::CString(s)) = &args[0] {
        Ok(EnvValue::Exp(Expression::CString(s.to_uppercase())))
    } else {
        Err("str_upper expects a string argument".to_string())
    }
}

pub fn str_lower_impl(args: Vec<EnvValue>) -> Result<EnvValue, String> {
    if args.len() != 1 {
        return Err("str_lower expects exactly one argument".to_string());
    }
    if let EnvValue::Exp(Expression::CString(s)) = &args[0] {
        Ok(EnvValue::Exp(Expression::CString(s.to_lowercase())))
    } else {
        Err("str_lower expects a string argument".to_string())
    }
}

pub fn str_length_impl(args: Vec<EnvValue>) -> Result<EnvValue, String> {
    if args.len() != 1 {
        return Err("str_length expects exactly one argument".to_string());
    }
    if let EnvValue::Exp(Expression::CString(s)) = &args[0] {
        Ok(EnvValue::Exp(Expression::CInt(s.chars().count() as i32)))
    } else {
        Err("str_length expects a string argument".to_string())
    }
}

pub fn str_reverse_impl(args: Vec<EnvValue>) -> Result<EnvValue, String> {
    if args.len() != 1 {
        return Err("str_reverse expects exactly one argument".to_string());
    }
    if let EnvValue::Exp(Expression::CString(s)) = &args[0] {
        Ok(EnvValue::Exp(Expression::CString(
            s.chars().rev().collect(),
        )))
    } else {
        Err("str_reverse expects a string argument".to_string())
    }
}

pub fn cont_chars_impl(args: Vec<EnvValue>) -> Result<EnvValue, String> {
    if args.len() != 2 {
        return Err("cont_chars expects exactly two arguments".to_string());
    }
    if let (EnvValue::Exp(Expression::CString(s)), EnvValue::Exp(Expression::CString(c))) =
        (&args[0], &args[1])
    {
        if c.len() != 1 {
            return Err("cont_chars expects a single character as the second argument".to_string());
        }
        let target = c.chars().next().unwrap();
        Ok(EnvValue::Exp(Expression::CInt(
            s.chars().filter(|&ch| ch == target).count() as i32,
        )))
    } else {
        Err("cont_chars expects a string and a character as arguments".to_string())
    }
}

pub fn filter_out_char_impl(args: Vec<EnvValue>) -> Result<EnvValue, String> {
    if args.len() != 2 {
        return Err("filter_out_char expects exactly two arguments".to_string());
    }
    if let (EnvValue::Exp(Expression::CString(s)), EnvValue::Exp(Expression::CString(c))) =
        (&args[0], &args[1])
    {
        if c.len() != 1 {
            return Err(
                "filter_out_char expects a single character as the second argument".to_string(),
            );
        }
        let target = c.chars().next().unwrap();
        Ok(EnvValue::Exp(Expression::CString(
            s.chars().filter(|&ch| ch != target).collect(),
        )))
    } else {
        Err("filter_out_char expects a string and a character as arguments".to_string())
    }
}

pub fn replace_impl(args: Vec<EnvValue>) -> Result<EnvValue, String> {
    if args.len() < 3 || args.len() > 4 {
        return Err("replace expects between 3 and 4 arguments".to_string());
    }
    if let (
        EnvValue::Exp(Expression::CString(s)),
        EnvValue::Exp(Expression::CString(old)),
        EnvValue::Exp(Expression::CString(new)),
    ) = (&args[0], &args[1], &args[2])
    {
        let count = if args.len() == 4 {
            if let EnvValue::Exp(Expression::CInt(n)) = &args[3] {
                *n
            } else {
                return Err("replace expects an integer as the fourth argument (count)".to_string());
            }
        } else {
            -1
        };

        let result = if count < 0 {
            s.replace(old, new)
        } else {
            let mut result = s.clone();
            let mut occurrences = 0;
            while occurrences < count {
                if let Some(pos) = result.find(old) {
                    result = format!("{}{}{}", &result[..pos], new, &result[pos + old.len()..]);
                    occurrences += 1;
                } else {
                    break;
                }
            }
            result
        };

        Ok(EnvValue::Exp(Expression::CString(result)))
    } else {
        Err("replace expects three string arguments and an optional integer".to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_str_lower_valid_strings() {
        let result = str_lower_impl(vec![EnvValue::Exp(Expression::CString(String::from(
            "HELLO",
        )))]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CString(res_value))) = result {
            assert_eq!(res_value, "hello");
        }
    }

    #[test]
    fn test_str_length_valid_string() {
        let result = str_length_impl(vec![EnvValue::Exp(Expression::CString(String::from(
            "hello",
        )))]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CInt(len))) = result {
            assert_eq!(len, 5);
        }
    }

    #[test]
    fn test_str_reverse_valid_string() {
        let result = str_reverse_impl(vec![EnvValue::Exp(Expression::CString(String::from(
            "hello",
        )))]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CString(res_value))) = result {
            assert_eq!(res_value, "olleh");
        }
    }

    #[test]
    fn test_cont_chars_valid_input() {
        let result = cont_chars_impl(vec![
            EnvValue::Exp(Expression::CString(String::from("banana"))),
            EnvValue::Exp(Expression::CString(String::from("a"))),
        ]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CInt(count))) = result {
            assert_eq!(count, 3);
        }
    }

    #[test]
    fn test_filter_out_char_valid_input() {
        let result = filter_out_char_impl(vec![
            EnvValue::Exp(Expression::CString(String::from("banana"))),
            EnvValue::Exp(Expression::CString(String::from("a"))),
        ]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CString(res_value))) = result {
            assert_eq!(res_value, "bnn");
        }
    }

    #[test]
    fn test_replace_valid_input() {
        let result = replace_impl(vec![
            EnvValue::Exp(Expression::CString(String::from("banana"))),
            EnvValue::Exp(Expression::CString(String::from("a"))),
            EnvValue::Exp(Expression::CString(String::from("o"))),
        ]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CString(res_value))) = result {
            assert_eq!(res_value, "bonono");
        }
    }
}
