use crate::ir::ast::{EnvValue, Expression};

pub fn sqrt(args: Vec<EnvValue>) -> EnvValue {
    if let EnvValue::Exp(Expression::CReal(x)) = &args[0] {
        EnvValue::Exp(Expression::CReal(x.sqrt()))
    } else {
        EnvValue::Exp(Expression::CReal(f64::NAN))
    }
}

pub fn factorial(n: &u64) -> u64{
    match *n{
        0 | 1 => 1,
        _ => (1..=*n).product()
    }
}

pub fn factorial_checked(n: &u64) -> Option<u64> {
    (1..=*n).try_fold(1u64, |acc, x| acc.checked_mul(x))
}

pub fn gcd(a: &i64, b: &i64) -> i64 {
    let mut a: i64 = *a;
    let mut b: i64 = *b;

    while b != 0 {
        let t: i64 = b;
        b = a % b;
        a = t;
    }
    a.abs()
}


pub fn lcm(a: &i64, b: &i64) -> i64{
    let a: i64 = *a;
    let b: i64 = *b;

    (a * b).abs() / gcd(&a, &b)
}

pub fn comb(n: &u64, k: &u64) -> u64 {
    let mut k: u64 = *k;
    if k > *n {
        return 0;
    }
    if k > *n - k {
        k = *n - k; 
    }
    (0..k).fold(1, |result, i| result * (n - i) / (i + 1))
}

pub fn perm(n: &u64, k: &u64) -> u64 {
    if *k > *n {
        return 0;
    }

    let mut result = 1;
    for i in 0..*k {
        result *= *n - i;
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sqrt_positive_real() {
        let args = vec![EnvValue::Exp(Expression::CReal(9.0))];
        let result = sqrt(args);

        if let EnvValue::Exp(Expression::CReal(res_value)) = result {
            assert_eq!(res_value, 3.0);
        } else {
            panic!("O resultado não é um número real");
        }
    }

    #[test]
    fn test_sqrt_zero() {
        let args = vec![EnvValue::Exp(Expression::CReal(0.0))];
        let result = sqrt(args);

        if let EnvValue::Exp(Expression::CReal(res_value)) = result {
            assert_eq!(res_value, 0.0);
        } else {
            panic!("O resultado não é um número real");
        }
    }

    #[test]
    fn test_sqrt_negative_real() {
        let args = vec![EnvValue::Exp(Expression::CReal(-4.0))];
        let result = sqrt(args);

        if let EnvValue::Exp(Expression::CReal(res_value)) = result {
            assert!(res_value.is_nan(), "O resultado deveria ser NaN para números negativos");
        } else {
            panic!("O resultado não é um número real");
        }
    }

    #[test]
    fn test_sqrt_invalid_argument() {
        let args = vec![EnvValue::Exp(Expression::CString("invalid".to_string()))];

        let result = sqrt(args);

        if let EnvValue::Exp(Expression::CReal(res_value)) = result {
            assert!(res_value.is_nan(), "O resultado deveria ser NaN para argumentos inválidos");
        } else {
            panic!("O resultado não é um número real");
        }
    }

    #[test]
    #[should_panic(expected = "index out of bounds")]
    fn test_sqrt_no_arguments() {
        let args = vec![];

        let _ = sqrt(args);
    }


    #[test]
    fn test_factorial_base_cases() {
        assert_eq!(factorial(&0), 1); 
        assert_eq!(factorial(&1), 1);
    }

    #[test]
    fn test_factorial_small_values(){
        assert_eq!(factorial(&2), 2);
        assert_eq!(factorial(&3), 6);
        assert_eq!(factorial(&4), 24);
        assert_eq!(factorial(&5), 120);
        assert_eq!(factorial(&6), 720);
    }

    #[test]
    fn test_factorial_medium_values(){
        assert_eq!(factorial(&9), 362_880);
        assert_eq!(factorial(&10), 3_628_800);
    }

    #[test]
    fn test_factorial_maximum_value() {
        assert_eq!(factorial(&20), 2_432_902_008_176_640_000); 
        assert!(factorial_checked(&21).is_none()); 
    }

    #[test]
    fn test_gcd_with_zero() {
        assert_eq!(gcd(&0, &0), 0);
        assert_eq!(gcd(&15, &0), 15);
        assert_eq!(gcd(&0, &79), 79);
        assert_eq!(gcd(&0, &123_456), 123_456); 
        assert_eq!(gcd(&987_654, &0), 987_654);
    }

    #[test]
    fn test_gcd_large_values() {
        assert_eq!(gcd(&1_000_000, &500_000), 500_000);
        assert_eq!(gcd(&2_147_483_647, &1_073_741_824), 1);
    }

    #[test]
    fn test_gcd_general_cases() {
        assert_eq!(gcd(&5, &7), 1);
        assert_eq!(gcd(&100, &40), 20);
        assert_eq!(gcd(&18, &90), 18);
        assert_eq!(gcd(&18, &70), 2);
    }

    #[test]
    fn test_gcd_two_negatives() {
        assert_eq!(gcd(&-54, &-9), 9);
        assert_eq!(gcd(&-111, &-72), 3);
        assert_eq!(gcd(&-37, &-54), 1);
        assert_eq!(gcd(&-10, &-10), 10);
    }

    #[test]
    fn test_gcd_positive_negative() {
        assert_eq!(gcd(&48, &-18), 6);
        assert_eq!(gcd(&100, &-25), 25);
        assert_eq!(gcd(&14, &-14), 14);
    }
    
    #[test]
    fn test_gcd_common_multiples() {
        assert_eq!(gcd(&50, &25), 25);
        assert_eq!(gcd(&81, &27), 27);
    }

    #[test]
    fn test_lcm_with_zero() {
        assert_eq!(lcm(&5, &0), 0); 
        assert_eq!(lcm(&0, &7), 0);
    }

    #[test]
    fn test_lcm_general_cases() {
        assert_eq!(lcm(&12, &18), 36); 
        assert_eq!(lcm(&4, &5), 20); 
        assert_eq!(lcm(&7, &3), 21); 
    }

    #[test]
    fn test_lcm_positive_negative() {
        assert_eq!(lcm(&12, &-18), 36);
        assert_eq!(lcm(&-4, &5), 20);
        assert_eq!(lcm(&-7, &3), 21);
    }
    #[test]
    fn test_lcm_two_negatives() {
        assert_eq!(lcm(&-12, &-18), 36);
        assert_eq!(lcm(&-4, &-5), 20);
        assert_eq!(lcm(&-7, &-3), 21);
    }

    #[test]
    fn test_lcm_large_values() {
        assert_eq!(lcm(&1_000_000, &2_000_000), 2_000_000);
        assert_eq!(lcm(&1_234_567, &2_345_678), 2_895_896_651_426); 
    }

    #[test]
    fn test_lcm_common_multiples() {
        assert_eq!(lcm(&50, &25), 50);
        assert_eq!(lcm(&81, &27), 81);
    }

    #[test]
    fn test_comb_basic_cases() {
        assert_eq!(comb(&5, &3), 10);
        assert_eq!(comb(&5, &0), 1);
        assert_eq!(comb(&0, &0), 1);
    }

    #[test]
    fn test_comb_large_values() {
        assert_eq!(comb(&20, &10), 184_756);
        assert_eq!(comb(&30, &15), 155_117_520);
    }

    #[test]
    fn test_comb_general_cases() {
        assert_eq!(comb(&6, &2), 15); 
        assert_eq!(comb(&10, &5), 252); 
        assert_eq!(comb(&4, &4), 1); 
    }

    #[test]
    fn test_perm_basic_cases() {
        assert_eq!(perm(&5, &3), 60); 
        assert_eq!(perm(&5, &0), 1);
    }

    #[test]
    fn test_perm_large_values() {
        assert_eq!(perm(&20, &5), 1_860_480); 
        assert_eq!(perm(&10, &10), 3_628_800); 
    }

    #[test]
    fn test_perm_general_cases() {
        assert_eq!(perm(&6, &2), 30);
        assert_eq!(perm(&10, &3), 720);
        assert_eq!(perm(&4, &4), 24);
    }

}