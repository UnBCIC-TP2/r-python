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