use crate::ir::ast::{EnvValue, Expression, Function, Statement, Type};
use std::collections::HashMap;

pub fn load_math_stdlib() -> HashMap<String, Function> {
    let mut math_stdlib = HashMap::new();

    math_stdlib.insert(
        "sqrt".to_string(),
        Function {
            kind: Type::TReal,
            params: Some(vec![("x".to_string(), Type::TReal)]),
            body: Box::new(Statement::Return(Box::new(Expression::MetaExp(
                sqrt_impl,
                vec![Expression::Var("x".to_string())],
                Type::TReal,
            )))),
        },
    );

    math_stdlib.insert(
        "factorial".to_string(),
        Function {
            kind: Type::TInteger,
            params: Some(vec![("n".to_string(), Type::TInteger)]),
            body: Box::new(Statement::Return(Box::new(Expression::MetaExp(
                factorial_impl,
                vec![Expression::Var("x".to_string())],
                Type::TInteger,
            )))),
        },
    );

    math_stdlib.insert(
        "gcd".to_string(),
        Function {
            kind: Type::TInteger,
            params: Some(vec![
                ("a".to_string(), Type::TInteger),
                ("b".to_string(), Type::TInteger),
            ]),
            body: Box::new(Statement::Return(Box::new(Expression::MetaExp(
                gcd_impl,
                vec![
                    Expression::Var("a".to_string()),
                    Expression::Var("b".to_string()),
                ],
                Type::TInteger,
            )))),
        },
    );

    math_stdlib.insert(
        "lcm".to_string(),
        Function {
            kind: Type::TInteger,
            params: Some(vec![
                ("a".to_string(), Type::TInteger),
                ("b".to_string(), Type::TInteger),
            ]),
            body: Box::new(Statement::Return(Box::new(Expression::MetaExp(
                lcm_impl,
                vec![
                    Expression::Var("a".to_string()),
                    Expression::Var("b".to_string()),
                ],
                Type::TInteger,
            )))),
        },
    );

    math_stdlib.insert(
        "is_prime".to_string(),
        Function {
            kind: Type::TBool,
            params: Some(vec![("x".to_string(), Type::TInteger)]),
            body: Box::new(Statement::Return(Box::new(Expression::MetaExp(
                is_prime_impl,
                vec![Expression::Var("x".to_string())],
                Type::TBool,
            )))),
        },
    );

    math_stdlib.insert(
        "comb".to_string(),
        Function {
            kind: Type::TInteger,
            params: Some(vec![
                ("n".to_string(), Type::TInteger),
                ("k".to_string(), Type::TInteger),
            ]),
            body: Box::new(Statement::Return(Box::new(Expression::MetaExp(
                comb_impl,
                vec![
                    Expression::Var("n".to_string()),
                    Expression::Var("r".to_string()),
                ],
                Type::TInteger,
            )))),
        },
    );

    math_stdlib.insert(
        "perm".to_string(),
        Function {
            kind: Type::TInteger,
            params: Some(vec![
                ("n".to_string(), Type::TInteger),
                ("k".to_string(), Type::TInteger),
            ]),
            body: Box::new(Statement::Return(Box::new(Expression::MetaExp(
                perm_impl,
                vec![
                    Expression::Var("n".to_string()),
                    Expression::Var("r".to_string()),
                ],
                Type::TInteger,
            )))),
        },
    );

    math_stdlib.insert(
        "log".to_string(),
        Function {
            kind: Type::TReal,
            params: Some(vec![
                ("x".to_string(), Type::TReal),
                ("base".to_string(), Type::TReal),
            ]),
            body: Box::new(Statement::Return(Box::new(Expression::MetaExp(
                log_impl,
                vec![
                    Expression::Var("x".to_string()),
                    Expression::Var("base".to_string()),
                ],
                Type::TReal,
            )))),
        },
    );

    math_stdlib
}

pub fn sqrt_impl(args: Vec<EnvValue>) -> Result<EnvValue, String> {
    if args.len() != 1 {
        return Err("sqrt expects exactly one argument".to_string());
    }

    if let EnvValue::Exp(Expression::CReal(x)) = &args[0] {
        Ok(EnvValue::Exp(Expression::CReal(x.sqrt())))
    } else {
        Err("sqrt expects a real number argument".to_string())
    }
}

pub fn factorial_impl(args: Vec<EnvValue>) -> Result<EnvValue, String> {
    if args.len() != 1 {
        return Err("factorial expects exactly one argument".to_string());
    }

    if let EnvValue::Exp(Expression::CInt(n)) = &args[0] {
        if *n < 0 {
            return Err("factorial expects a non-negative integer argument".to_string());
        }
        let mut prod: i32 = 1;
        for i in 1..=*n {
            prod *= i;
        }
        Ok(EnvValue::Exp(Expression::CInt(prod)))
    } else {
        Err("factorial expects an integer argument".to_string())
    }
}

pub fn gcd_impl(args: Vec<EnvValue>) -> Result<EnvValue, String> {
    if args.len() != 2 {
        return Err("gcd expects exactly two arguments".to_string());
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
        Ok(EnvValue::Exp(Expression::CInt(a.abs())))
    } else {
        Err("gcd expects two integer arguments".to_string())
    }
}

pub fn lcm_impl(args: Vec<EnvValue>) -> Result<EnvValue, String> {
    if args.len() != 2 {
        return Err("lcm expects exactly two arguments".to_string());
    }

    if let (EnvValue::Exp(Expression::CInt(a)), EnvValue::Exp(Expression::CInt(b))) =
        (&args[0], &args[1])
    {
        let gcd_val = match gcd_impl(args.clone()) {
            Ok(EnvValue::Exp(Expression::CInt(val))) => val,
            Err(err) => return Err(format!("Error calculating gcd: {}", err)),
            _ => return Err("Unexpected error in gcd calculation".to_string()),
        };

        let lcm_val = (a * b).abs() / gcd_val;
        Ok(EnvValue::Exp(Expression::CInt(lcm_val)))
    } else {
        Err("lcm expects two integer arguments".to_string())
    }
}

pub fn comb_impl(args: Vec<EnvValue>) -> Result<EnvValue, String> {
    if args.len() != 2 {
        return Err("comb expects exactly two arguments".to_string());
    }

    if let (EnvValue::Exp(Expression::CInt(n)), EnvValue::Exp(Expression::CInt(k))) =
        (&args[0], &args[1])
    {
        if *n < 0 || *k < 0 {
            return Err("comb expects non-negative integers".to_string());
        }
        let n = *n;
        let mut k = *k;
        if k > n {
            return Ok(EnvValue::Exp(Expression::CInt(0)));
        }
        if k > n - k {
            k = n - k;
        }
        let result = (0..k).fold(1, |acc, i| acc * (n - i) / (i + 1));
        Ok(EnvValue::Exp(Expression::CInt(result)))
    } else {
        Err("comb expects two integer arguments".to_string())
    }
}

pub fn perm_impl(args: Vec<EnvValue>) -> Result<EnvValue, String> {
    if args.len() != 2 {
        return Err("perm expects exactly two arguments".to_string());
    }

    if let (EnvValue::Exp(Expression::CInt(n)), EnvValue::Exp(Expression::CInt(k))) =
        (&args[0], &args[1])
    {
        if *n < 0 || *k < 0 {
            return Err("perm expects non-negative integers".to_string());
        }
        let n = *n;
        let k = *k;
        if k > n {
            return Ok(EnvValue::Exp(Expression::CInt(0)));
        }
        let mut result: i32 = 1;
        for i in 0..k {
            result *= n - i;
        }
        Ok(EnvValue::Exp(Expression::CInt(result)))
    } else {
        Err("perm expects two integer arguments".to_string())
    }
}

pub fn is_prime_impl(args: Vec<EnvValue>) -> Result<EnvValue, String> {
    if args.len() != 1 {
        return Err("is_prime expects exactly one argument".to_string());
    }

    if let EnvValue::Exp(Expression::CInt(n)) = &args[0] {
        if *n < 0 {
            return Err("is_prime expects a non-negative integer".to_string());
        }
        let n = *n;
        let result = {
            if n <= 1 {
                false
            } else if n <= 3 {
                true
            } else if n % 2 == 0 || n % 3 == 0 {
                false
            } else {
                let mut i = 5;
                let mut prime = true;
                while i * i <= n {
                    if n % i == 0 || n % (i + 2) == 0 {
                        prime = false;
                        break;
                    }
                    i += 6;
                }
                prime
            }
        };
        if result {
            Ok(EnvValue::Exp(Expression::CTrue))
        } else {
            Ok(EnvValue::Exp(Expression::CFalse))
        }
    } else {
        Err("is_prime expects an integer argument".to_string())
    }
}

pub fn log_impl(args: Vec<EnvValue>) -> Result<EnvValue, String> {
    if args.len() != 2 {
        return Err("log expects exactly two arguments".to_string());
    }
    if let (EnvValue::Exp(Expression::CReal(base)), EnvValue::Exp(Expression::CReal(x))) =
        (&args[0], &args[1])
    {
        Ok(EnvValue::Exp(Expression::CReal(x.log(*base))))
    } else {
        Err("log expects two real arguments".to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    //TESTES FUNCAO SQRT
    #[test]
    fn test_sqrt_positive_real() {
        let result = sqrt_impl(vec![EnvValue::Exp(Expression::CReal(9.0))]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CReal(res_value))) = result {
            assert_eq!(res_value, 3.0);
        }

        let result = sqrt_impl(vec![EnvValue::Exp(Expression::CReal(49.0))]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CReal(res_value))) = result {
            assert_eq!(res_value, 7.0);
        }

        let result = sqrt_impl(vec![EnvValue::Exp(Expression::CReal(121.0))]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CReal(res_value))) = result {
            assert_eq!(res_value, 11.0);
        }
    }

    #[test]
    fn test_sqrt_zero() {
        let result = sqrt_impl(vec![EnvValue::Exp(Expression::CReal(0.0))]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CReal(res_value))) = result {
            assert_eq!(res_value, 0.0);
        }
    }

    #[test]
    fn test_sqrt_invalid_number_of_arguments() {
        let result = sqrt_impl(vec![]);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "sqrt expects exactly one argument");
    }

    #[test]
    fn test_sqrt_invalid_number_of_arguments_multiple() {
        let result = sqrt_impl(vec![
            EnvValue::Exp(Expression::CReal(25.0)),
            EnvValue::Exp(Expression::CReal(9.0)),
        ]);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "sqrt expects exactly one argument");
    }

    #[test]
    fn test_sqrt_invalid_argument_type() {
        let result = sqrt_impl(vec![EnvValue::Exp(Expression::CInt(25))]);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "sqrt expects a real number argument");
    }
    //TESTES FUNCAO FACTORIAL
    #[test]
    fn test_factorial_valid_inputs() {
        let result = factorial_impl(vec![EnvValue::Exp(Expression::CInt(0))]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CInt(value))) = result {
            assert_eq!(value, 1);
        }

        let result = factorial_impl(vec![EnvValue::Exp(Expression::CInt(1))]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CInt(value))) = result {
            assert_eq!(value, 1);
        }

        let result = factorial_impl(vec![EnvValue::Exp(Expression::CInt(5))]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CInt(value))) = result {
            assert_eq!(value, 120);
        }

        let result = factorial_impl(vec![EnvValue::Exp(Expression::CInt(10))]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CInt(value))) = result {
            assert_eq!(value, 3628800);
        }
    }

    #[test]
    fn test_factorial_invalid_number_of_arguments() {
        let result = factorial_impl(vec![]);
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err(),
            "factorial expects exactly one argument"
        );
    }

    #[test]
    fn test_factorial_invalid_number_of_arguments_multiple() {
        let result = factorial_impl(vec![
            EnvValue::Exp(Expression::CInt(1)),
            EnvValue::Exp(Expression::CInt(2)),
        ]);
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err(),
            "factorial expects exactly one argument"
        );
    }

    #[test]
    fn test_factorial_invalid_argument_type() {
        let result = factorial_impl(vec![EnvValue::Exp(Expression::CReal(3.5))]);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "factorial expects an integer argument");
    }

    #[test]
    fn test_factorial_negative_argument() {
        let result = factorial_impl(vec![EnvValue::Exp(Expression::CInt(-1))]);
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err(),
            "factorial expects a non-negative integer argument"
        );
    }
    //TESTES FUNCAO GCD
    #[test]
    fn test_gcd_valid_inputs() {
        let result = gcd_impl(vec![
            EnvValue::Exp(Expression::CInt(48)),
            EnvValue::Exp(Expression::CInt(18)),
        ]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CInt(value))) = result {
            assert_eq!(value, 6);
        }

        let result = gcd_impl(vec![
            EnvValue::Exp(Expression::CInt(7)),
            EnvValue::Exp(Expression::CInt(3)),
        ]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CInt(value))) = result {
            assert_eq!(value, 1);
        }

        let result = gcd_impl(vec![
            EnvValue::Exp(Expression::CInt(-48)),
            EnvValue::Exp(Expression::CInt(18)),
        ]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CInt(value))) = result {
            assert_eq!(value, 6);
        }

        let result = gcd_impl(vec![
            EnvValue::Exp(Expression::CInt(0)),
            EnvValue::Exp(Expression::CInt(18)),
        ]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CInt(value))) = result {
            assert_eq!(value, 18);
        }
    }

    #[test]
    fn test_gcd_invalid_number_of_arguments() {
        let result = gcd_impl(vec![EnvValue::Exp(Expression::CInt(48))]);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "gcd expects exactly two arguments");
    }

    #[test]
    fn test_gcd_invalid_number_of_arguments_multiple() {
        let result = gcd_impl(vec![
            EnvValue::Exp(Expression::CInt(48)),
            EnvValue::Exp(Expression::CInt(18)),
            EnvValue::Exp(Expression::CInt(6)),
        ]);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "gcd expects exactly two arguments");
    }

    #[test]
    fn test_gcd_invalid_argument_type() {
        let result = gcd_impl(vec![
            EnvValue::Exp(Expression::CReal(48.0)),
            EnvValue::Exp(Expression::CInt(18)),
        ]);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "gcd expects two integer arguments");
    }
    //TESTES PARA LCM
    #[test]
    fn test_lcm_valid_inputs() {
        let result = lcm_impl(vec![
            EnvValue::Exp(Expression::CInt(48)),
            EnvValue::Exp(Expression::CInt(18)),
        ]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CInt(value))) = result {
            assert_eq!(value, 144);
        }

        let result = lcm_impl(vec![
            EnvValue::Exp(Expression::CInt(7)),
            EnvValue::Exp(Expression::CInt(3)),
        ]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CInt(value))) = result {
            assert_eq!(value, 21);
        }

        let result = lcm_impl(vec![
            EnvValue::Exp(Expression::CInt(-48)),
            EnvValue::Exp(Expression::CInt(18)),
        ]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CInt(value))) = result {
            assert_eq!(value, 144);
        }

        let result = lcm_impl(vec![
            EnvValue::Exp(Expression::CInt(0)),
            EnvValue::Exp(Expression::CInt(18)),
        ]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CInt(value))) = result {
            assert_eq!(value, 0);
        }
    }

    #[test]
    fn test_lcm_invalid_number_of_arguments() {
        let result = lcm_impl(vec![EnvValue::Exp(Expression::CInt(48))]);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "lcm expects exactly two arguments");
    }

    #[test]
    fn test_lcm_invalid_number_of_arguments_multiple() {
        let result = lcm_impl(vec![
            EnvValue::Exp(Expression::CInt(48)),
            EnvValue::Exp(Expression::CInt(18)),
            EnvValue::Exp(Expression::CInt(6)),
        ]);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "lcm expects exactly two arguments");
    }

    #[test]
    fn test_lcm_invalid_argument_type() {
        let result = lcm_impl(vec![
            EnvValue::Exp(Expression::CReal(48.0)),
            EnvValue::Exp(Expression::CInt(18)),
        ]);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "lcm expects two integer arguments");
    }

    //TESTES PARA COMB
    #[test]
    fn test_comb_valid_inputs() {
        let result = comb_impl(vec![
            EnvValue::Exp(Expression::CInt(5)),
            EnvValue::Exp(Expression::CInt(2)),
        ]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CInt(value))) = result {
            assert_eq!(value, 10);
        }

        let result = comb_impl(vec![
            EnvValue::Exp(Expression::CInt(10)),
            EnvValue::Exp(Expression::CInt(3)),
        ]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CInt(value))) = result {
            assert_eq!(value, 120);
        }

        let result = comb_impl(vec![
            EnvValue::Exp(Expression::CInt(5)),
            EnvValue::Exp(Expression::CInt(6)),
        ]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CInt(value))) = result {
            assert_eq!(value, 0);
        }
    }

    #[test]
    fn test_comb_invalid_number_of_arguments() {
        let result = comb_impl(vec![EnvValue::Exp(Expression::CInt(5))]);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "comb expects exactly two arguments");
    }

    #[test]
    fn test_comb_invalid_number_of_arguments_multiple() {
        let result = comb_impl(vec![
            EnvValue::Exp(Expression::CInt(5)),
            EnvValue::Exp(Expression::CInt(2)),
            EnvValue::Exp(Expression::CInt(1)),
        ]);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "comb expects exactly two arguments");
    }

    #[test]
    fn test_comb_invalid_argument_type() {
        let result = comb_impl(vec![
            EnvValue::Exp(Expression::CReal(5.0)),
            EnvValue::Exp(Expression::CInt(2)),
        ]);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "comb expects two integer arguments");
    }

    #[test]
    fn test_comb_negative_arguments() {
        let result = comb_impl(vec![
            EnvValue::Exp(Expression::CInt(5)),
            EnvValue::Exp(Expression::CInt(-2)),
        ]);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "comb expects non-negative integers");
    }

    //TESTES PARA PERM
    #[test]
    fn test_perm_valid_inputs() {
        let result = perm_impl(vec![
            EnvValue::Exp(Expression::CInt(5)),
            EnvValue::Exp(Expression::CInt(2)),
        ]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CInt(value))) = result {
            assert_eq!(value, 20);
        }

        let result = perm_impl(vec![
            EnvValue::Exp(Expression::CInt(10)),
            EnvValue::Exp(Expression::CInt(3)),
        ]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CInt(value))) = result {
            assert_eq!(value, 720);
        }

        let result = perm_impl(vec![
            EnvValue::Exp(Expression::CInt(5)),
            EnvValue::Exp(Expression::CInt(6)),
        ]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CInt(value))) = result {
            assert_eq!(value, 0);
        }
    }

    #[test]
    fn test_perm_invalid_number_of_arguments() {
        let result = perm_impl(vec![EnvValue::Exp(Expression::CInt(5))]);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "perm expects exactly two arguments");
    }

    #[test]
    fn test_perm_invalid_number_of_arguments_multiple() {
        let result = perm_impl(vec![
            EnvValue::Exp(Expression::CInt(5)),
            EnvValue::Exp(Expression::CInt(2)),
            EnvValue::Exp(Expression::CInt(1)),
        ]);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "perm expects exactly two arguments");
    }

    #[test]
    fn test_perm_invalid_argument_type() {
        let result = perm_impl(vec![
            EnvValue::Exp(Expression::CReal(5.0)),
            EnvValue::Exp(Expression::CInt(2)),
        ]);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "perm expects two integer arguments");
    }

    #[test]
    fn test_perm_negative_arguments() {
        let result = perm_impl(vec![
            EnvValue::Exp(Expression::CInt(5)),
            EnvValue::Exp(Expression::CInt(-2)),
        ]);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "perm expects non-negative integers");
    }

    //=================================================================================================
    #[test]
    fn test_is_prime_valid_inputs() {
        let result = is_prime_impl(vec![EnvValue::Exp(Expression::CInt(2))]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CTrue)) = result {
            assert!(true);
        }

        let result = is_prime_impl(vec![EnvValue::Exp(Expression::CInt(3))]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CTrue)) = result {
            assert!(true);
        }

        let result = is_prime_impl(vec![EnvValue::Exp(Expression::CInt(7))]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CTrue)) = result {
            assert!(true);
        }

        let result = is_prime_impl(vec![EnvValue::Exp(Expression::CInt(13))]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CTrue)) = result {
            assert!(true);
        }

        let result = is_prime_impl(vec![EnvValue::Exp(Expression::CInt(17))]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CTrue)) = result {
            assert!(true);
        }
    }

    #[test]
    fn test_is_prime_invalid_number_of_arguments() {
        let result = is_prime_impl(vec![]);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "is_prime expects exactly one argument");
    }

    #[test]
    fn test_is_prime_invalid_number_of_arguments_multiple() {
        let result = is_prime_impl(vec![
            EnvValue::Exp(Expression::CInt(2)),
            EnvValue::Exp(Expression::CInt(3)),
        ]);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "is_prime expects exactly one argument");
    }

    #[test]
    fn test_is_prime_invalid_argument_type() {
        let result = is_prime_impl(vec![EnvValue::Exp(Expression::CReal(2.0))]);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "is_prime expects an integer argument");
    }

    #[test]
    fn test_is_prime_negative_argument() {
        let result = is_prime_impl(vec![EnvValue::Exp(Expression::CInt(-1))]);
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err(),
            "is_prime expects a non-negative integer"
        );
    }

    //=================================================================================================
    #[test]
    fn test_log_valid_inputs() {
        let result = log_impl(vec![
            EnvValue::Exp(Expression::CReal(10.0)),
            EnvValue::Exp(Expression::CReal(100.0)),
        ]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CReal(value))) = result {
            assert_eq!(value, 2.0);
        }

        let result = log_impl(vec![
            EnvValue::Exp(Expression::CReal(2.0)),
            EnvValue::Exp(Expression::CReal(8.0)),
        ]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CReal(value))) = result {
            assert_eq!(value, 3.0);
        }

        let result = log_impl(vec![
            EnvValue::Exp(Expression::CReal(10.0)),
            EnvValue::Exp(Expression::CReal(10000.0)),
        ]);
        assert!(result.is_ok());
        if let Ok(EnvValue::Exp(Expression::CReal(value))) = result {
            assert_eq!(value, 4.0);
        }
    }

    #[test]
    fn test_log_invalid_number_of_arguments() {
        let result = log_impl(vec![EnvValue::Exp(Expression::CReal(10.0))]);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "log expects exactly two arguments");
    }

    #[test]
    fn test_log_invalid_number_of_arguments_multiple() {
        let result = log_impl(vec![
            EnvValue::Exp(Expression::CReal(10.0)),
            EnvValue::Exp(Expression::CReal(100.0)),
            EnvValue::Exp(Expression::CReal(10.0)),
        ]);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "log expects exactly two arguments");
    }

    #[test]
    fn test_log_invalid_argument_type() {
        let result = log_impl(vec![
            EnvValue::Exp(Expression::CInt(10)),
            EnvValue::Exp(Expression::CReal(100.0)),
        ]);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "log expects two real arguments");
    }
}
