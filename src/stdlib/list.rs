use crate::ir::ast::{EnvValue, Expression};
use crate::ir::ast::Environment; 
use std::collections::HashMap;

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

#[cfg(test)]
mod tests {
    use super::*;
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

}