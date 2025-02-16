use crate::ir::ast::{EnvValue, Expression};

pub fn str_upper(args: Vec<EnvValue>) -> Result<EnvValue, String> {
    if args.len() != 1 {
        return Err("str_upper expects exactly one argument".to_string());
    }

    if let EnvValue::Exp(Expression::CString(s)) = &args[0] {
        Ok(EnvValue::Exp(Expression::CString(s.to_uppercase())))
    } else {
        Err("str_upper expects a string argument".to_string())
    }
}

pub fn str_lower(args: Vec<EnvValue>) -> Result<EnvValue, String> {
    if args.len() != 1 {
        return Err("str_lower expects exactly one argument".to_string());
    }
    if let EnvValue::Exp(Expression::CString(s)) = &args[0] {
        Ok(EnvValue::Exp(Expression::CString(s.to_lowercase())))
    } else {
        Err("str_lower expects a string argument".to_string())
    }
}

pub fn str_length(args: Vec<EnvValue>) -> Result<EnvValue, String> {
    if args.len() != 1 {
        return Err("str_length expects exactly one argument".to_string());
    }
    if let EnvValue::Exp(Expression::CString(s)) = &args[0] {
        Ok(EnvValue::Exp(Expression::CInt(s.chars().count() as i32)))
    } else {
        Err("str_length expects a string argument".to_string())
    }
}

pub fn str_reverse(args: Vec<EnvValue>) -> Result<EnvValue, String> {
    if args.len() != 1 {
        return Err("str_reverse expects exactly one argument".to_string());
    }
    if let EnvValue::Exp(Expression::CString(s)) = &args[0] {
        Ok(EnvValue::Exp(Expression::CString(s.chars().rev().collect())))
    } else {
        Err("str_reverse expects a string argument".to_string())
    }
}

pub fn cont_chars(args: Vec<EnvValue>) -> Result<EnvValue, String> {
    if args.len() != 2 {
        return Err("cont_chars expects exactly two arguments".to_string());
    }
    if let (EnvValue::Exp(Expression::CString(s)), EnvValue::Exp(Expression::CString(c))) = (&args[0], &args[1]) {
        if c.len() != 1 {
            return Err("cont_chars expects a single character as the second argument".to_string());
        }
        let target = c.chars().next().unwrap();
        Ok(EnvValue::Exp(Expression::CInt(s.chars().filter(|&ch| ch == target).count() as i32)))
    } else {
        Err("cont_chars expects a string and a character as arguments".to_string())
    }
}

pub fn filter_out_char(args: Vec<EnvValue>) -> Result<EnvValue, String> {
    if args.len() != 2 {
        return Err("filter_out_char expects exactly two arguments".to_string());
    }
    if let (EnvValue::Exp(Expression::CString(s)), EnvValue::Exp(Expression::CString(c))) = (&args[0], &args[1]) {
        if c.len() != 1 {
            return Err("filter_out_char expects a single character as the second argument".to_string());
        }
        let target = c.chars().next().unwrap();
        Ok(EnvValue::Exp(Expression::CString(s.chars().filter(|&ch| ch != target).collect())))
    } else {
        Err("filter_out_char expects a string and a character as arguments".to_string())
    }
}

pub fn replace(args: Vec<EnvValue>) -> Result<EnvValue, String> {
    if args.len() < 3 || args.len() > 4 {
        return Err("replace expects between 3 and 4 arguments".to_string());
    }
    if let (EnvValue::Exp(Expression::CString(s)), EnvValue::Exp(Expression::CString(old)), EnvValue::Exp(Expression::CString(new))) = (&args[0], &args[1], &args[2]) {
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
        let result = str_lower(vec![EnvValue::Exp(Expression::CString(String::from("HELLO")))]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CString(res_value))) = result {
            assert_eq!(res_value, "hello");
        }
    }

    #[test]
    fn test_str_length_valid_string() {
        let result = str_length(vec![EnvValue::Exp(Expression::CString(String::from("hello")))]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CInt(len))) = result {
            assert_eq!(len, 5);
        }
    }

    #[test]
    fn test_str_reverse_valid_string() {
        let result = str_reverse(vec![EnvValue::Exp(Expression::CString(String::from("hello")))]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CString(res_value))) = result {
            assert_eq!(res_value, "olleh");
        }
    }

    #[test]
    fn test_cont_chars_valid_input() {
        let result = cont_chars(vec![
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
        let result = filter_out_char(vec![
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
        let result = replace(vec![
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