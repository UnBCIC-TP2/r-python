use crate::ir::ast::{EnvValue, Expression};
use crate::ir::ast::Environment; 
use std::collections::HashMap;

pub fn sqrt(args: Vec<EnvValue>) -> EnvValue {
    if args.len() != 1 {
        panic!("sqrt expects exactly one argument");
    }

    if let EnvValue::Exp(Expression::CReal(x)) = &args[0] {
        EnvValue::Exp(Expression::CReal(x.sqrt()))
    } else {
        panic!("sqrt expects a real number argument");
    }
}

pub fn len(args: Vec<EnvValue>) -> EnvValue {
    if args.len() != 1 {
        panic!("len espera exatamente um argumento");
    }

    if let EnvValue::Exp(Expression::FuncCall(_, lista)) = &args[0] {
        let mut contador = 0;
        for _ in lista {
            contador += 1;
        }
        return EnvValue::Exp(Expression::CInt(contador as i32));
    } else {
        panic!("len espera um argumento do tipo lista");
    }
}

pub fn max(args: Vec<EnvValue>) -> EnvValue {
    if args.len() != 1 {
        panic!("max espera exatamente um argumento");
    }

    if let EnvValue::Exp(Expression::FuncCall(_, lista)) = &args[0] {


        // Inicializar o valor máximo com o primeiro elemento da lista
        let mut achei_max = match lista[0] {
            Expression::CInt(val) => val,
            _ => panic!("max espera uma lista de inteiros"),
        };

        // Iterar sobre a lista e encontrar o valor máximo
        for expr in &lista[1..] {
            if let Expression::CInt(val) = expr {
                if *val > achei_max {
                    achei_max = *val; // Atualiza achei_max com o valor do inteiro
                }
            } else {
                panic!("max espera uma lista de inteiros");
            }
        }

        return EnvValue::Exp(Expression::CInt(achei_max));
    } else {
        panic!("max espera um argumento do tipo lista");
    }
}

pub fn min(args: Vec<EnvValue>) -> EnvValue {
    if args.len() != 1 {
        panic!("max espera exatamente um argumento");
    }

    if let EnvValue::Exp(Expression::FuncCall(_, lista)) = &args[0] {


        // Inicializar o valor máximo com o primeiro elemento da lista
        let mut achei_min = match lista[0] {
            Expression::CInt(val) => val,
            _ => panic!("min espera uma lista de inteiros"),
        };

        // Iterar sobre a lista e encontrar o valor máximo
        for expr in &lista[1..] {
            if let Expression::CInt(val) = expr {
                if *val == achei_min {
                    achei_min = *val; // Atualiza achei_max com o valor do inteiro
                }
            } else {
                panic!("min espera uma lista de inteiros");
            }
        }

        return EnvValue::Exp(Expression::CInt(achei_min));
    } else {
        panic!("min espera um argumento do tipo lista");
    }
}

pub fn index(args: Vec<EnvValue>, target: i32) -> EnvValue {
    if args.len() != 1 {
        panic!("index espera exatamente um argumento");
    }

    if let EnvValue::Exp(Expression::FuncCall(_, lista)) = &args[0] {
        let mut contador = 0;

        // Iterar sobre a lista e encontrar o índice
        for expr in lista {
            if let Expression::CInt(val) = expr {
                if *val == target {
                    return EnvValue::Exp(Expression::CInt(contador));
                }
                contador +=1;
            } else {
                panic!("index espera uma lista de inteiros");
            }
        }
        return EnvValue::Exp(Expression::CInt(-1));

    } else {
        panic!("index espera um argumento do tipo lista");
    }
}

pub fn remove(args: Vec<EnvValue>, target: i32) -> EnvValue {
    if args.len() != 1 {
        panic!("remove espera exatamente um argumento");
    }

    if let EnvValue::Exp(Expression::FuncCall(name, lista)) = &args[0] {
        let mut lista_nova = vec![];

        // Iterar sobre a lista e remover o valor alvo (target)
        for expr in lista {
            if let Expression::CInt(val) = expr {
                if *val != target {
                    lista_nova.push(Expression::CInt(*val));
                }
            } else {
                panic!("remove espera uma lista de inteiros");
            }
        }

        return EnvValue::Exp(Expression::FuncCall(name.clone(), lista_nova));
    } else {
        panic!("remove espera um argumento do tipo lista");
    }
}

pub fn zip(args: Vec<EnvValue>) -> EnvValue {
    if args.len() != 2 {
        panic!("zip espera exatamente dois argumentos");
    }

    if let (EnvValue::Exp(Expression::FuncCall(name1, lista1)), EnvValue::Exp(Expression::FuncCall(name2, lista2))) = (&args[0], &args[1]) {
        let zipped_list: Vec<Expression> = lista1.iter().zip(lista2.iter())
            .map(|(a, b)| Expression::FuncCall("pair".to_string(), vec![a.clone(), b.clone()]))
            .collect();
        return EnvValue::Exp(Expression::FuncCall("zipped_list".to_string(), zipped_list));
    } else {
        panic!("zip espera argumentos do tipo lista");
    }
}

pub fn enumerate(args: Vec<EnvValue>) -> EnvValue {
    if args.len() != 1 {
        panic!("enumerate espera exatamente um argumento");
    }

    if let EnvValue::Exp(Expression::FuncCall(name, lista)) = &args[0] {
        let mut tupla = vec![];

        // Iterar sobre a lista e criar tuplas (index, item)
        for (index, item) in lista.iter().enumerate() {
            if let Expression::CInt(val) = item {
                tupla.push(Expression::FuncCall("pair".to_string(), vec![
                    Expression::CInt(index as i32),
                    Expression::CInt(*val),
                ]));
            } else {
                panic!("enumerate espera uma lista de inteiros");
            }
        }
        return EnvValue::Exp(Expression::FuncCall("tupla_list".to_string(), tupla));
    } else {
        panic!("enumerate espera um argumento do tipo lista");
    }
}

pub fn sort(args: Vec<EnvValue>) -> EnvValue {
    if args.len() != 1 {
        panic!("sort espera exatamente um argumento");
    }

    if let EnvValue::Exp(Expression::FuncCall(name, lista)) = &args[0] {
        let mut result: Vec<Expression> = lista.clone();
        let len = result.len();

        for i in 0..len {
            for j in 0..len - i - 1 {
                if let (Expression::CInt(val1), Expression::CInt(val2)) = (&result[j], &result[j + 1]) {
                    if val1 > val2 {
                        let temp = result[j].clone();
                        result[j] = result[j + 1].clone();
                        result[j + 1] = temp;
                    }
                } else {
                    panic!("sort espera uma lista de inteiros");
                }
            }
        }

        return EnvValue::Exp(Expression::FuncCall(name.clone(), result));
    } else {
        panic!("sort espera um argumento do tipo lista");
    }
}

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

pub fn factorial(args: Vec<EnvValue>) -> EnvValue {
    if args.len() != 1 {
        panic!("factorial expects exactly one argument");
    }
    if let EnvValue::Exp(Expression::CInt(n)) = &args[0] {
        if *n < 0 {
            panic!("factorial expects a non-negative integer argument");
        }
        let mut prod: i32 = 1;
        for i in 1..=*n {
            prod *= i;
        }
        EnvValue::Exp(Expression::CInt(prod))
    } else {
        panic!("factorial expects a integer argument");
    }
}


pub fn gcd(args: Vec<EnvValue>) -> EnvValue {
    if args.len() != 2 {
        panic!("gcd expects exactly two arguments");
    }
    if let (EnvValue::Exp(Expression::CInt(a)), EnvValue::Exp(Expression::CInt(b))) =
        (&args[0], &args[1])
    {
        let mut a = *a;
        let mut b = *b;
        while b != 0 {
            let t = b;
            b = a % b;
            a = t;
        }
        EnvValue::Exp(Expression::CInt(a.abs()))
    } else {
        panic!("gcd expects two integer arguments");
    }
}

pub fn lcm(args: Vec<EnvValue>) -> EnvValue {
    if args.len() != 2 {
        panic!("lcm expects exactly two arguments");
    }
    if let (EnvValue::Exp(Expression::CInt(a)), EnvValue::Exp(Expression::CInt(b))) =
        (&args[0], &args[1])
    {
        let gcd_val = match gcd(args.clone()) {
            EnvValue::Exp(Expression::CInt(val)) => val,
            _ => panic!("Error calculating gcd"),
        };
        
        let lcm_val = (a * b).abs() / gcd_val;
        EnvValue::Exp(Expression::CInt(lcm_val))
    } else {
        panic!("lcm expects two integer arguments");
    }
}

pub fn comb(args: Vec<EnvValue>) -> EnvValue {
    if args.len() != 2 {
        panic!("comb expects exactly two arguments");
    }
    if let (EnvValue::Exp(Expression::CInt(n)), EnvValue::Exp(Expression::CInt(k))) = (&args[0], &args[1]) {
        if *n < 0 || *k < 0 {
            panic!("comb expects non-negative integers");
        }
        let n = *n;
        let mut k = *k;
        if k > n {
            return EnvValue::Exp(Expression::CInt(0));
        }
        if k > n - k {
            k = n - k;
        }
        let result = (0..k).fold(1, |acc, i| acc * (n - i) / (i + 1));
        EnvValue::Exp(Expression::CInt(result))
    } else {
        panic!("comb expects two integer arguments");
    }
}

pub fn perm(args: Vec<EnvValue>) -> EnvValue {
    if args.len() != 2 {
        panic!("perm expects exactly two arguments");
    }
    if let (EnvValue::Exp(Expression::CInt(n)), EnvValue::Exp(Expression::CInt(k))) = (&args[0], &args[1]) {
        if *n < 0 || *k < 0 {
            panic!("perm expects non-negative integers");
        }
        let n = *n;
        let k = *k;
        if k > n {
            return EnvValue::Exp(Expression::CInt(0));
        }
        let mut result: i32 = 1;
        for i in 0..k {
            result *= n - i;
        }
        EnvValue::Exp(Expression::CInt(result))
    } else {
        panic!("perm expects two integer arguments");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sqrt_positive_real() {
        let result = sqrt(vec![EnvValue::Exp(Expression::CReal(9.0))]);
        match result {
            EnvValue::Exp(Expression::CReal(res_value)) => assert_eq!(res_value, 3.0),
            _ => panic!("Incorrect result for sqrt of 9.0"),
        }
        
        let result = sqrt(vec![EnvValue::Exp(Expression::CReal(49.0))]);
        match result {
            EnvValue::Exp(Expression::CReal(res_value)) => assert_eq!(res_value, 7.0),
            _ => panic!("Incorrect result for sqrt of 49.0"),
        }

        let result = sqrt(vec![EnvValue::Exp(Expression::CReal(121.0))]);
        match result {
            EnvValue::Exp(Expression::CReal(res_value)) => assert_eq!(res_value, 11.0),
            _ => panic!("Incorrect result for sqrt of 121.0"),
        }
    }

    #[test]
    fn test_len() {
        let lista = vec![
            Expression::CInt(1),
            Expression::CInt(2),
            Expression::CInt(3),
        ];
    
        let result = len(vec![EnvValue::Exp(Expression::FuncCall("test".to_string(), lista))]);
    
        match result {
            EnvValue::Exp(Expression::CInt(res_value)) => assert_eq!(res_value, 3),
            _ => panic!("Resultado incorreto para len([1, 2, 3])"),
        }
    }

    #[test]
    fn test_max() {
        let lista = vec![
            Expression::CInt(1),
            Expression::CInt(2),
            Expression::CInt(5),
        ];
    
        let result = max(vec![EnvValue::Exp(Expression::FuncCall("test".to_string(), lista))]);
    
        match result {
            EnvValue::Exp(Expression::CInt(res_value)) => assert_eq!(res_value, 5),
            _ => panic!("Resultado incorreto para len([1, 2, 5])"),
        }
    }

    #[test]
    fn test_min() {
        let lista = vec![
            Expression::CInt(1),
            Expression::CInt(2),
            Expression::CInt(5),
        ];
    
        let result = min(vec![EnvValue::Exp(Expression::FuncCall("test".to_string(), lista))]);
    
        match result {
            EnvValue::Exp(Expression::CInt(res_value)) => assert_eq!(res_value, 1),
            _ => panic!("Resultado incorreto para len([1, 2, 5])"),
        }
    }

    #[test]
    fn test_index() {
        let lista = vec![
            Expression::CInt(1),
            Expression::CInt(2),
            Expression::CInt(5),
        ];

        let result = index(vec![EnvValue::Exp(Expression::FuncCall("test".to_string(), lista))], 5);

        match result {
            EnvValue::Exp(Expression::CInt(res_value)) => assert_eq!(res_value, 2), // O índice do valor 5 é 2
            _ => panic!("Resultado incorreto para index([1, 2, 5], 5)"),
        }
    }

    
    #[test]
    fn test_remove() {
        let lista = vec![
            Expression::CInt(1),
            Expression::CInt(2),
            Expression::CInt(5),
        ];

        let result = remove(vec![EnvValue::Exp(Expression::FuncCall("test".to_string(), lista))], 2);

        match result {
            EnvValue::Exp(Expression::FuncCall(_, nova_lista)) => {
                let expected = vec![
                    Expression::CInt(1),
                    Expression::CInt(5),
                ];
                assert_eq!(nova_lista, expected);
            },
            _ => panic!("Resultado incorreto para remove([1, 2, 5], 2)"),
        }
    }   

    #[test]
    fn test_zip() {
        let lista1 = vec![
            Expression::CInt(1),
            Expression::CInt(2),
            Expression::CInt(3),
        ];
        let lista2 = vec![
            Expression::CInt(4),
            Expression::CInt(5),
            Expression::CInt(6),
        ];

        let result = zip(vec![
            EnvValue::Exp(Expression::FuncCall("lista1".to_string(), lista1)),
            EnvValue::Exp(Expression::FuncCall("lista2".to_string(), lista2))
        ]);

        match result {
            EnvValue::Exp(Expression::FuncCall(_, zipped_list)) => {
                let expected = vec![
                    Expression::FuncCall("pair".to_string(), vec![Expression::CInt(1), Expression::CInt(4)]),
                    Expression::FuncCall("pair".to_string(), vec![Expression::CInt(2), Expression::CInt(5)]),
                    Expression::FuncCall("pair".to_string(), vec![Expression::CInt(3), Expression::CInt(6)]),
                ];
                assert_eq!(zipped_list, expected);
                },
            _ => panic!("Resultado incorreto para zip([1, 2, 3], [4, 5, 6])"),
        }
    }

    #[test]
    fn test_enumerate() {
        let lista = vec![
            Expression::CInt(10),
            Expression::CInt(20),
            Expression::CInt(30),
        ];

        let result = enumerate(vec![EnvValue::Exp(Expression::FuncCall("lista".to_string(), lista))]);

        match result {
            EnvValue::Exp(Expression::FuncCall(_, tupla_list)) => {
                let expected = vec![
                    Expression::FuncCall("pair".to_string(), vec![Expression::CInt(0), Expression::CInt(10)]),
                    Expression::FuncCall("pair".to_string(), vec![Expression::CInt(1), Expression::CInt(20)]),
                    Expression::FuncCall("pair".to_string(), vec![Expression::CInt(2), Expression::CInt(30)]),
                ];
                assert_eq!(tupla_list, expected);
            },
            _ => panic!("Resultado incorreto para enumerate([10, 20, 30])"),
        }
    }

    #[test]
    fn test_sort() {
        let lista = vec![
            Expression::CInt(3),
            Expression::CInt(1),
            Expression::CInt(4),
            Expression::CInt(1),
            Expression::CInt(5),
            Expression::CInt(9),
            Expression::CInt(2),
            Expression::CInt(6),
            Expression::CInt(5),
        ];

        let result = sort(vec![EnvValue::Exp(Expression::FuncCall("lista".to_string(), lista))]);

        match result {
            EnvValue::Exp(Expression::FuncCall(_, sorted_list)) => {
                let expected = vec![
                    Expression::CInt(1),
                    Expression::CInt(1),
                    Expression::CInt(2),
                    Expression::CInt(3),
                    Expression::CInt(4),
                    Expression::CInt(5),
                    Expression::CInt(5),
                    Expression::CInt(6),
                    Expression::CInt(9),
                ];
                assert_eq!(sorted_list, expected);
            },
            _ => panic!("Resultado incorreto para sort([3, 1, 4, 1, 5, 9, 2, 6, 5])"),
        }
    }
    
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

    #[test]
    fn test_sqrt_zero() {
        let result = sqrt(vec![EnvValue::Exp(Expression::CReal(0.0))]);
        match result {
            EnvValue::Exp(Expression::CReal(res_value)) => assert_eq!(res_value, 0.0),
            _ => panic!("Incorrect result for sqrt of 0.0"),
        }
    }

    #[test]
    #[should_panic(expected = "sqrt expects exactly one argument")]
    fn test_sqrt_invalid_number_of_arguments() {
        sqrt(vec![]);
    }

    #[test]
    #[should_panic(expected = "sqrt expects exactly one argument")]
    fn test_sqrt_invalid_number_of_arguments_multiple() {
        sqrt(vec![
            EnvValue::Exp(Expression::CReal(25.0)),
            EnvValue::Exp(Expression::CReal(9.0)),
        ]);
    }

    #[test]
    #[should_panic(expected = "sqrt expects a real number argument")]
    fn test_sqrt_invalid_argument_type() {
        sqrt(vec![EnvValue::Exp(Expression::CInt(25))]);
    }
    
//====================================================================================================
    #[test]
    fn test_factorial_valid_inputs() {
        let result = factorial(vec![EnvValue::Exp(Expression::CInt(0))]);
        match result {
            EnvValue::Exp(Expression::CInt(value)) => assert_eq!(value, 1),
            _ => panic!("Incorrect result for factorial of 0"),
        }
        
        let result = factorial(vec![EnvValue::Exp(Expression::CInt(1))]);
        match result {
            EnvValue::Exp(Expression::CInt(value)) => assert_eq!(value, 1),
            _ => panic!("Incorrect result for factorial of 1"),
        }
        
        let result = factorial(vec![EnvValue::Exp(Expression::CInt(5))]);
        match result {
            EnvValue::Exp(Expression::CInt(value)) => assert_eq!(value, 120),
            _ => panic!("Incorrect result for factorial of 5"),
        }
        
        let result = factorial(vec![EnvValue::Exp(Expression::CInt(10))]);
        match result {
            EnvValue::Exp(Expression::CInt(value)) => assert_eq!(value, 3628800),
            _ => panic!("Incorrect result for factorial of 10"),
        }
    }

    #[test]
    #[should_panic(expected = "factorial expects exactly one argument")]
    fn test_factorial_invalid_number_of_arguments() {
        factorial(vec![]);
    }

    #[test]
    #[should_panic(expected = "factorial expects exactly one argument")]
    fn test_factorial_invalid_number_of_arguments_multiple() {
        factorial(vec![EnvValue::Exp(Expression::CInt(1)), EnvValue::Exp(Expression::CInt(2))]);
    }

    #[test]
    #[should_panic(expected = "factorial expects a integer argument")]
    fn test_factorial_invalid_argument_type() {
        factorial(vec![EnvValue::Exp(Expression::CReal(3.5))]);
    }

    #[test]
    #[should_panic(expected = "factorial expects a non-negative integer argument")]
    fn test_factorial_negative_argument() {
        factorial(vec![EnvValue::Exp(Expression::CInt(-1))]);
    }

//====================================================================================================
    #[test]
    fn test_gcd_valid_inputs() {
        let result = gcd(vec![EnvValue::Exp(Expression::CInt(48)), EnvValue::Exp(Expression::CInt(18))]);
        match result {
            EnvValue::Exp(Expression::CInt(value)) => assert_eq!(value, 6),
            _ => panic!("Incorrect result for gcd of 48 and 18"),
        }

        let result = gcd(vec![EnvValue::Exp(Expression::CInt(7)), EnvValue::Exp(Expression::CInt(3))]);
        match result {
            EnvValue::Exp(Expression::CInt(value)) => assert_eq!(value, 1),
            _ => panic!("Incorrect result for gcd of 7 and 3"),
        }

        let result = gcd(vec![EnvValue::Exp(Expression::CInt(-48)), EnvValue::Exp(Expression::CInt(18))]);
        match result {
            EnvValue::Exp(Expression::CInt(value)) => assert_eq!(value, 6),
            _ => panic!("Incorrect result for gcd of -48 and 18"),
        }

        let result = gcd(vec![EnvValue::Exp(Expression::CInt(0)), EnvValue::Exp(Expression::CInt(18))]);
        match result {
            EnvValue::Exp(Expression::CInt(value)) => assert_eq!(value, 18),
            _ => panic!("Incorrect result for gcd of 0 and 18"),
        }
    }

    #[test]
    #[should_panic(expected = "gcd expects exactly two arguments")]
    fn test_gcd_invalid_number_of_arguments() {
        gcd(vec![EnvValue::Exp(Expression::CInt(48))]);
    }

    #[test]
    #[should_panic(expected = "gcd expects exactly two arguments")]
    fn test_gcd_invalid_number_of_arguments_multiple() {
        gcd(vec![
            EnvValue::Exp(Expression::CInt(48)),
            EnvValue::Exp(Expression::CInt(18)),
            EnvValue::Exp(Expression::CInt(6)),
        ]);
    }

    #[test]
    #[should_panic(expected = "gcd expects two integer arguments")]
    fn test_gcd_invalid_argument_type() {
        gcd(vec![EnvValue::Exp(Expression::CReal(48.0)), EnvValue::Exp(Expression::CInt(18))]);
    }

//====================================================================================================
    #[test]
    fn test_lcm_valid_inputs() {
        let result = lcm(vec![EnvValue::Exp(Expression::CInt(48)), EnvValue::Exp(Expression::CInt(18))]);
        match result {
            EnvValue::Exp(Expression::CInt(value)) => assert_eq!(value, 144),
            _ => panic!("Incorrect result for lcm of 48 and 18"),
        }

        let result = lcm(vec![EnvValue::Exp(Expression::CInt(7)), EnvValue::Exp(Expression::CInt(3))]);
        match result {
            EnvValue::Exp(Expression::CInt(value)) => assert_eq!(value, 21),
            _ => panic!("Incorrect result for lcm of 7 and 3"),
        }

        let result = lcm(vec![EnvValue::Exp(Expression::CInt(-48)), EnvValue::Exp(Expression::CInt(18))]);
        match result {
            EnvValue::Exp(Expression::CInt(value)) => assert_eq!(value, 144),
            _ => panic!("Incorrect result for lcm of -48 and 18"),
        }

        let result = lcm(vec![EnvValue::Exp(Expression::CInt(0)), EnvValue::Exp(Expression::CInt(18))]);
        match result {
            EnvValue::Exp(Expression::CInt(value)) => assert_eq!(value, 0),
            _ => panic!("Incorrect result for lcm of 0 and 18"),
        }
    }

    #[test]
    #[should_panic(expected = "lcm expects exactly two arguments")]
    fn test_lcm_invalid_number_of_arguments() {
        lcm(vec![EnvValue::Exp(Expression::CInt(48))]);
    }

    #[test]
    #[should_panic(expected = "lcm expects exactly two arguments")]
    fn test_lcm_invalid_number_of_arguments_multiple() {
        lcm(vec![
            EnvValue::Exp(Expression::CInt(48)),
            EnvValue::Exp(Expression::CInt(18)),
            EnvValue::Exp(Expression::CInt(6)),
        ]);
    }

    #[test]
    #[should_panic(expected = "lcm expects two integer arguments")]
    fn test_lcm_invalid_argument_type() {
        lcm(vec![EnvValue::Exp(Expression::CReal(48.0)), EnvValue::Exp(Expression::CInt(18))]);

    }

//====================================================================================================
    #[test]
    fn test_comb_valid_inputs() {
        let result = comb(vec![EnvValue::Exp(Expression::CInt(5)), EnvValue::Exp(Expression::CInt(2))]);
        match result {
            EnvValue::Exp(Expression::CInt(value)) => assert_eq!(value, 10),
            _ => panic!("Incorrect result for comb(5, 2)"),
        }

        let result = comb(vec![EnvValue::Exp(Expression::CInt(10)), EnvValue::Exp(Expression::CInt(3))]);
        match result {
            EnvValue::Exp(Expression::CInt(value)) => assert_eq!(value, 120),
            _ => panic!("Incorrect result for comb(10, 3)"),
        }

        let result = comb(vec![EnvValue::Exp(Expression::CInt(5)), EnvValue::Exp(Expression::CInt(6))]);
        match result {
            EnvValue::Exp(Expression::CInt(value)) => assert_eq!(value, 0),
            _ => panic!("Incorrect result for comb(5, 6)"),
        }
    }

    #[test]
    #[should_panic(expected = "comb expects exactly two arguments")]
    fn test_comb_invalid_number_of_arguments() {
        comb(vec![EnvValue::Exp(Expression::CInt(5))]);
    }

    #[test]
    #[should_panic(expected = "comb expects exactly two arguments")]
    fn test_comb_invalid_number_of_arguments_multiple() {
        comb(vec![
            EnvValue::Exp(Expression::CInt(5)),
            EnvValue::Exp(Expression::CInt(2)),
            EnvValue::Exp(Expression::CInt(1)),
        ]);
    }

    #[test]
    #[should_panic(expected = "comb expects two integer arguments")]
    fn test_comb_invalid_argument_type() {
        comb(vec![EnvValue::Exp(Expression::CReal(5.0)), EnvValue::Exp(Expression::CInt(2))]);
    }

    #[test]
    #[should_panic(expected = "comb expects non-negative integers")]
    fn test_comb_negative_arguments() {
        comb(vec![EnvValue::Exp(Expression::CInt(5)), EnvValue::Exp(Expression::CInt(-2))]);
    }

//====================================================================================================
    #[test]
    fn test_perm_valid_inputs() {
        let result = perm(vec![EnvValue::Exp(Expression::CInt(5)), EnvValue::Exp(Expression::CInt(2))]);
        match result {
            EnvValue::Exp(Expression::CInt(value)) => assert_eq!(value, 20),
            _ => panic!("Incorrect result for perm(5, 2)"),
        }

        let result = perm(vec![EnvValue::Exp(Expression::CInt(10)), EnvValue::Exp(Expression::CInt(3))]);
        match result {
            EnvValue::Exp(Expression::CInt(value)) => assert_eq!(value, 720),
            _ => panic!("Incorrect result for perm(10, 3)"),
        }

        let result = perm(vec![EnvValue::Exp(Expression::CInt(5)), EnvValue::Exp(Expression::CInt(6))]);
        match result {
            EnvValue::Exp(Expression::CInt(value)) => assert_eq!(value, 0),
            _ => panic!("Incorrect result for perm(5, 6)"),
        }
    }

    #[test]
    #[should_panic(expected = "perm expects exactly two arguments")]
    fn test_perm_invalid_number_of_arguments() {
        perm(vec![EnvValue::Exp(Expression::CInt(5))]);
    }

    #[test]
    #[should_panic(expected = "perm expects exactly two arguments")]
    fn test_perm_invalid_number_of_arguments_multiple() {
        perm(vec![
            EnvValue::Exp(Expression::CInt(5)),
            EnvValue::Exp(Expression::CInt(2)),
            EnvValue::Exp(Expression::CInt(1)),
        ]);
    }

    #[test]
    #[should_panic(expected = "perm expects two integer arguments")]
    fn test_perm_invalid_argument_type() {
        perm(vec![EnvValue::Exp(Expression::CReal(5.0)), EnvValue::Exp(Expression::CInt(2))]);
    }

    #[test]
    #[should_panic(expected = "perm expects non-negative integers")]
    fn test_perm_negative_arguments() {
        perm(vec![EnvValue::Exp(Expression::CInt(5)), EnvValue::Exp(Expression::CInt(-2))]);
    }
} 