use crate::ir::ast::{EnvValue, Expression};

pub fn str_upper(args: Vec<EnvValue>) -> EnvValue {
    if args.len() != 1 {
        panic!("str_upper expects exactly one argument");
    }

    if let EnvValue::Exp(Expression::CString(s)) = &args[0] {
        EnvValue::Exp(Expression::CString(s.to_uppercase()))
    } else {
        panic!("str_upper expects a string argument");
    }
}    

pub fn str_lower(args: Vec<EnvValue>) -> EnvValue {
    if args.len() != 1 {
        panic!("str_lower expects exactly one argument");
    }
    if let EnvValue::Exp(Expression::CString(s)) = &args[0] {
        EnvValue::Exp(Expression::CString(s.to_lowercase()))
    } else {
        panic!("str_lower expects a string argument");
    }
}

pub fn str_length(args: Vec<EnvValue>) -> EnvValue {
    if args.len() != 1 {
        panic!("str_length expects exactly one argument");
    }
    if let EnvValue::Exp(Expression::CString(s)) = &args[0] {
        EnvValue::Exp(Expression::CInt(s.chars().count() as i32))
    } else {
        panic!("str_length expects a string argument");
    }
}

pub fn str_reverse(args: Vec<EnvValue>) -> EnvValue {
    if args.len() != 1 {
        panic!("str_reverse expects exactly one argument");
    }
    if let EnvValue::Exp(Expression::CString(s)) = &args[0] {
        EnvValue::Exp(Expression::CString(s.chars().rev().collect()))
    } else {
        panic!("str_reverse expects a string argument");
    }
}

//conta quantas vezes aparece um char especifico
pub fn cont_chars(args: Vec<EnvValue>) -> EnvValue {
    if args.len() != 2 {
        panic!("cont_chars expects exactly two arguments");
    }
    if let (EnvValue::Exp(Expression::CString(s)), EnvValue::Exp(Expression::CString(c))) = (&args[0], &args[1]) {
        if c.len() != 1 {
            panic!("cont_chars expects a single character as the second argument");
        }
        let target = c.chars().next().unwrap();
        EnvValue::Exp(Expression::CInt(s.chars().filter(|&ch| ch == target).count() as i32))
    } else {
        panic!("cont_chars expects a string and a character as arguments");
    }
}

//retira todas as aparicoes de um char especifico
pub fn filter_out_char(args: Vec<EnvValue>) -> EnvValue {
    if args.len() != 2 {
        panic!("filter_out_char expects exactly two arguments");
    }
    if let (EnvValue::Exp(Expression::CString(s)), EnvValue::Exp(Expression::CString(c))) = (&args[0], &args[1]) {
        if c.len() != 1 {
            panic!("filter_out_char expects a single character as the second argument");
        }
        let target = c.chars().next().unwrap();
        EnvValue::Exp(Expression::CString(s.chars().filter(|&ch| ch != target).collect()))
    } else {
        panic!("filter_out_char expects a string and a character as arguments");
    }
}

// substitui os N primeiros caracteres "old" por um novo caracter "new" 
pub fn replace(args: Vec<EnvValue>) -> EnvValue {
    if args.len() < 3 || args.len() > 4 {
        panic!("replace expects between 3 and 4 arguments");
    }
    if let (EnvValue::Exp(Expression::CString(s)), EnvValue::Exp(Expression::CString(old)), EnvValue::Exp(Expression::CString(new))) = (&args[0], &args[1], &args[2]) {
        let count = if args.len() == 4 {
            if let EnvValue::Exp(Expression::CInt(n)) = &args[3] {
                *n
            } else {
                panic!("replace expects an integer as the fourth argument (count)");
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

        EnvValue::Exp(Expression::CString(result))
    } else {
        panic!("replace expects three string arguments and an optional integer");
    }
}

//centraliza a string e preenche o resto com um char
pub fn center(args: Vec<EnvValue>) -> EnvValue {
    if args.len() != 3 {
        panic!("center expects exactly three arguments");
    }

    if let (
        EnvValue::Exp(Expression::CString(s)),
        EnvValue::Exp(Expression::CInt(width)),
        EnvValue::Exp(Expression::CString(fillchar)),
    ) = (&args[0], &args[1], &args[2])
    {
        if fillchar.len() != 1 {
            panic!("center expects a single character as the fill character");
        }

        let fill = fillchar.chars().next().unwrap();
        let pad = (*width as usize).saturating_sub(s.len());
        let left = pad / 2;
        let right = pad - left;

        let centered = format!(
            "{}{}{}",
            fill.to_string().repeat(left),
            s,
            fill.to_string().repeat(right)
        );

        EnvValue::Exp(Expression::FuncCall(
            "center".to_string(),
            vec![Expression::CString(centered)],
        ))
    } else {
        panic!("center expects a string, an integer width, and a character as arguments");
    }
}

//acha uma substring
pub fn find(args: Vec<EnvValue>) -> EnvValue {
    if args.len() < 2 || args.len() > 4 {
        panic!("find expects between 2 and 4 arguments");
    }

    if let (EnvValue::Exp(Expression::CString(s)), EnvValue::Exp(Expression::CString(sub))) =
        (&args[0], &args[1])
    {
        let start = if args.len() > 2 {
            if let EnvValue::Exp(Expression::CInt(n)) = &args[2] {
                *n as usize
            } else {
                panic!("find expects an integer as the third argument (start index)");
            }
        } else {
            0
        };

        let end = if args.len() > 3 {
            if let EnvValue::Exp(Expression::CInt(n)) = &args[3] {
                Some(*n as usize)
            } else {
                panic!("find expects an integer as the fourth argument (end index)");
            }
        } else {
            None
        };

        let end = end.unwrap_or(s.len());
        let result = match s.get(start..end) {
            Some(substring) => substring.find(sub).map(|i| i + start),
            None => None, 
        };

        let retorno = match result {
            Some(index) => EnvValue::Exp(Expression::CInt(index as i32)),
            None => EnvValue::Exp(Expression::CInt(-1)),
        };

        if let EnvValue::Exp(expr) = retorno {
            EnvValue::Exp(Expression::FuncCall("find".to_string(), vec![expr]))
        } else {
            panic!("invalid return value type");
        }
        
    } else {
        panic!("find expects two strings as first arguments");
    }
}

//recebe uma lista de strings e junta elas em uma string so
pub fn join(args: Vec<EnvValue>) -> EnvValue {
    if args.len() != 2 {
        panic!("join expects exactly two arguments");
    }

    if let (
        EnvValue::Exp(Expression::CString(sep)),
        EnvValue::Exp(Expression::FuncCall(_, lista)),
    ) = (&args[0], &args[1])
    {
        let strings: Vec<String> = lista
            .iter()
            .map(|expr| {
                if let Expression::CString(s) = expr {
                    s.clone()
                } else {
                    panic!("join expects a list of strings as the second argument");
                }
            })
            .collect();

        EnvValue::Exp(Expression::CString(strings.join(sep)))
    } else {
        panic!("join expects a string and a list of strings as arguments");
    }
}

pub fn partition(args: Vec<EnvValue>) -> EnvValue {
    if args.len() != 2 {
        panic!("partition expects exactly two arguments");
    }

    if let ( 
        EnvValue::Exp(Expression::CString(s)),
        EnvValue::Exp(Expression::CString(sep)),
    ) = (&args[0], &args[1])
    {
        match s.find(sep) {
            Some(index) => EnvValue::Exp(Expression::FuncCall(
                "tuple".to_string(),
                vec![
                    Expression::CString(s[..index].to_string()),
                    Expression::CString(sep.clone()),
                    Expression::CString(s[index + sep.len()..].to_string()),
                ],
            )),
            None => EnvValue::Exp(Expression::FuncCall(
                "tuple".to_string(),
                vec![
                    Expression::CString(s.to_string()),
                    Expression::CString(String::new()),
                    Expression::CString(String::new()),
                ],
            )),
        }
    } else {
        panic!("partition expects two string arguments");
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_str_lower_valid_strings() {
        let result = str_lower(vec![EnvValue::Exp(Expression::CString(String::from("HELLO")))]);
        match result {
            EnvValue::Exp(Expression::CString(res_value)) => assert_eq!(res_value, "hello"),
            _ => panic!("Incorrect result for str_lower('HELLO')"),
        }
    }

    #[test]
    fn test_str_length_valid_string() {
        let result = str_length(vec![EnvValue::Exp(Expression::CString(String::from("hello")))]);
        match result {
            EnvValue::Exp(Expression::CInt(len)) => assert_eq!(len, 5),
            _ => panic!("Incorrect result for str_length('hello')"),
        }
    }

    #[test]
    fn test_str_reverse_valid_string() {
        let result = str_reverse(vec![EnvValue::Exp(Expression::CString(String::from("hello")))]);
        match result {
            EnvValue::Exp(Expression::CString(res_value)) => assert_eq!(res_value, "olleh"),
            _ => panic!("Incorrect result for str_reverse('hello')"),
        }
    }

    #[test]
    fn test_cont_chars_valid_input() {
        let result = cont_chars(vec![
            EnvValue::Exp(Expression::CString(String::from("banana"))),
            EnvValue::Exp(Expression::CString(String::from("a"))),
        ]);
        match result {
            EnvValue::Exp(Expression::CInt(count)) => assert_eq!(count, 3),
            _ => panic!("Incorrect result for cont_chars('banana', 'a')"),
        }
    }

    #[test]
    fn test_filter_out_char_valid_input() {
        let result = filter_out_char(vec![
            EnvValue::Exp(Expression::CString(String::from("banana"))),
            EnvValue::Exp(Expression::CString(String::from("a"))),
        ]);
        match result {
            EnvValue::Exp(Expression::CString(res_value)) => assert_eq!(res_value, "bnn"),
            _ => panic!("Incorrect result for filter_out_char('banana', 'a')"),
        }
    }

    #[test]
    fn test_replace_valid_input() {
        let result = replace(vec![
            EnvValue::Exp(Expression::CString(String::from("banana"))),
            EnvValue::Exp(Expression::CString(String::from("a"))),
            EnvValue::Exp(Expression::CString(String::from("o"))),
        ]);
        match result {
            EnvValue::Exp(Expression::CString(res_value)) => assert_eq!(res_value, "bonono"),
            _ => panic!("Incorrect result for replace('banana', 'a', 'o')"),
        }
    }

    #[test]
    fn test_center_valid_input() {
        let result = center(vec![
            EnvValue::Exp(Expression::CString(String::from("hello"))),
            EnvValue::Exp(Expression::CInt(11)),
            EnvValue::Exp(Expression::CString(String::from("*"))),
        ]);
        match result {
            EnvValue::Exp(Expression::FuncCall(_, args)) => {
                if let Expression::CString(centered) = &args[0] {
                    assert_eq!(centered, "***hello***");
                } else {
                    panic!("Incorrect result for center('hello', 11, '*')");
                }
            }
            _ => panic!("Incorrect return type for center"),
        }
    }

    #[test]
    fn test_find_valid_input() {
        let result = find(vec![
            EnvValue::Exp(Expression::CString(String::from("hello world"))),
            EnvValue::Exp(Expression::CString(String::from("world"))),
        ]);
        match result {
            EnvValue::Exp(Expression::FuncCall(_, args)) => {
                if let Expression::CInt(index) = &args[0] {
                    assert_eq!(*index, 6);
                } else {
                    panic!("Incorrect result for find('hello world', 'world')");
                }
            }
            _ => panic!("Incorrect return type for find"),
        }
    }

    #[test]
    fn test_join_valid_input() {
        let result = join(vec![
            EnvValue::Exp(Expression::CString(String::from(","))),
            EnvValue::Exp(Expression::FuncCall(
                "list".to_string(),
                vec![
                    Expression::CString(String::from("a")),
                    Expression::CString(String::from("b")),
                    Expression::CString(String::from("c")),
                ],
            )),
        ]);
        match result {
            EnvValue::Exp(Expression::CString(res_value)) => assert_eq!(res_value, "a,b,c"),
            _ => panic!("Incorrect result for join(',', ['a', 'b', 'c'])"),
        }
    }

    #[test]
    fn test_partition_valid_input() {
        let result = partition(vec![
            EnvValue::Exp(Expression::CString(String::from("banana"))),
            EnvValue::Exp(Expression::CString(String::from("n"))),
        ]);
        match result {
            EnvValue::Exp(Expression::FuncCall(_, args)) => {
                if let (Expression::CString(left), Expression::CString(sep), Expression::CString(right)) = (&args[0], &args[1], &args[2]) {
                    assert_eq!(left, "ba");
                    assert_eq!(sep, "n");
                    assert_eq!(right, "ana");
                } else {
                    panic!("Incorrect result for partition('banana', 'n')");
                }
            }
            _ => panic!("Incorrect return type for partition"),
        }
    }
}