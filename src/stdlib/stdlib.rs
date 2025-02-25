use std::collections::HashMap;
use crate::ir::ast::Function;

use crate::stdlib::math::load_math_stdlib;
use crate::stdlib::string::load_string_stdlib;

pub fn load_stdlib() -> HashMap<String, Function> {
    let mut stdlib = HashMap::new();

    let math_stdlib = load_math_stdlib();
    for (name, func) in math_stdlib {
        stdlib.insert(name, func);
    }

    let string_stdlib = load_string_stdlib();
    for (name, func) in string_stdlib {
        stdlib.insert(name, func);
    }

    stdlib
}

#[cfg(test)]
mod stdlib_tests {
    use crate::ir::ast::{Statement, Expression, Type};
    use crate::stdlib::math::load_math_stdlib;
    use crate::stdlib::string::load_string_stdlib;

    #[test]
    fn test_load_math_stdlib_contains_sqrt() {
        let math_stdlib = load_math_stdlib();
        let sqrt_func = math_stdlib.get("sqrt").expect("Function 'sqrt' not found in math stdlib");

        assert_eq!(sqrt_func.kind, Type::TReal);

        let params = sqrt_func.params.as_ref().expect("Expected parameters for sqrt");
        assert_eq!(params.len(), 1);
        assert_eq!(params[0].0, "x");
        assert_eq!(params[0].1, Type::TReal);

        match *sqrt_func.body {
            Statement::Return(ref inner) => {
                match **inner {
                    Expression::MetaExp(_, ref args, ref ret_type) => {
                        assert_eq!(args.len(), 1);
                        assert_eq!(*ret_type, Type::TReal);
                    },
                    _ => panic!("Expected MetaExp inside Return for sqrt"),
                }
            },
            _ => panic!("Expected Return statement for sqrt body"),
        }
    }

    #[test]
    fn test_load_math_stdlib_contains_factorial(){
        let math_stdlib = load_math_stdlib();
        let factorial_func = math_stdlib.get("factorial").expect("Function 'factorial' not found in math stdlib");

        assert_eq!(factorial_func.kind, Type::TInteger);

        let params = factorial_func.params.as_ref().expect("Expected parameters for factorial");
        assert_eq!(params.len(), 1);
        assert_eq!(params[0].0, "n");
        assert_eq!(params[0].1, Type::TInteger);

        match *factorial_func.body {
            Statement::Return(ref inner) => {
                match **inner {
                    Expression::MetaExp(_, ref args, ref ret_type) => {
                        assert_eq!(args.len(), 1);
                        assert_eq!(*ret_type, Type::TInteger);
                    },
                    _ => panic!("Expected MetaExp inside Return for factorial"),
                }
            },
            _ => panic!("Expected Return statement for factorial body"),
        }
    }

    #[test]
    fn test_load_math_stdlib_contains_gcd(){
        let math_stdlib = load_math_stdlib();
        let gcd_func = math_stdlib.get("gcd").expect("Function 'gcd' not found in math stdlib");

        assert_eq!(gcd_func.kind, Type::TInteger);

        let params = gcd_func.params.as_ref().expect("Expected parameters for gcd");
        assert_eq!(params.len(), 2);
        assert_eq!(params[0].0, "a");
        assert_eq!(params[0].1, Type::TInteger);
        assert_eq!(params[1].0, "b");
        assert_eq!(params[1].1, Type::TInteger);

        match *gcd_func.body {
            Statement::Return(ref inner) => {
                match **inner {
                    Expression::MetaExp(_, ref args, ref ret_type) => {
                        assert_eq!(args.len(), 2);
                        assert_eq!(*ret_type, Type::TInteger);
                    },
                    _ => panic!("Expected MetaExp inside Return for gcd"),
                }
            },
            _ => panic!("Expected Return statement for gcd body"),
        }
    }

    #[test]
    fn test_load_math_stdlib_contains_lcm(){
        let math_stdlib = load_math_stdlib();
        let lcm_func = math_stdlib.get("lcm").expect("Function 'lcm' not found in math stdlib");

        assert_eq!(lcm_func.kind, Type::TInteger);

        let params = lcm_func.params.as_ref().expect("Expected parameters for lcm");
        assert_eq!(params.len(), 2);
        assert_eq!(params[0].0, "a");
        assert_eq!(params[0].1, Type::TInteger);
        assert_eq!(params[1].0, "b");
        assert_eq!(params[1].1, Type::TInteger);

        match *lcm_func.body {
            Statement::Return(ref inner) => {
                match **inner {
                    Expression::MetaExp(_, ref args, ref ret_type) => {
                        assert_eq!(args.len(), 2);
                        assert_eq!(*ret_type, Type::TInteger);
                    },
                    _ => panic!("Expected MetaExp inside Return for lcm"),
                }
            },
            _ => panic!("Expected Return statement for lcm body"),
        }
    }

    #[test]
    fn test_load_math_stdlib_contains_comb(){
        let math_stdlib = load_math_stdlib();
        let comb_func = math_stdlib.get("comb").expect("Function 'comb' not found in math stdlib");

        assert_eq!(comb_func.kind, Type::TInteger);

        let params = comb_func.params.as_ref().expect("Expected parameters for comb");
        assert_eq!(params.len(), 2);
        assert_eq!(params[0].0, "n");
        assert_eq!(params[0].1, Type::TInteger);
        assert_eq!(params[1].0, "k");
        assert_eq!(params[1].1, Type::TInteger);

        match *comb_func.body {
            Statement::Return(ref inner) => {
                match **inner {
                    Expression::MetaExp(_, ref args, ref ret_type) => {
                        assert_eq!(args.len(), 2);
                        assert_eq!(*ret_type, Type::TInteger);
                    },
                    _ => panic!("Expected MetaExp inside Return for comb"),
                }
            },
            _ => panic!("Expected Return statement for comb body"),
        }
    }

    #[test]
    fn test_load_math_stdlib_contains_perm(){
        let math_stdlib = load_math_stdlib();
        let perm_func = math_stdlib.get("perm").expect("Function 'perm' not found in math stdlib");

        assert_eq!(perm_func.kind, Type::TInteger);

        let params = perm_func.params.as_ref().expect("Expected parameters for perm");
        assert_eq!(params.len(), 2);
        assert_eq!(params[0].0, "n");
        assert_eq!(params[0].1, Type::TInteger);
        assert_eq!(params[1].0, "k");
        assert_eq!(params[1].1, Type::TInteger);

        match *perm_func.body {
            Statement::Return(ref inner) => {
                match **inner {
                    Expression::MetaExp(_, ref args, ref ret_type) => {
                        assert_eq!(args.len(), 2);
                        assert_eq!(*ret_type, Type::TInteger);
                    },
                    _ => panic!("Expected MetaExp inside Return for perm"),
                }
            },
            _ => panic!("Expected Return statement for perm body"),
        }
    }

    #[test]
    fn test_load_math_stdlib_contains_is_prime(){
        let math_stdlib = load_math_stdlib();
        let is_prime_func = math_stdlib.get("is_prime").expect("Function 'is_prime' not found in math stdlib");

        assert_eq!(is_prime_func.kind, Type::TBool);

        let params = is_prime_func.params.as_ref().expect("Expected parameters for is_prime");
        assert_eq!(params.len(), 1);
        assert_eq!(params[0].0, "x");
        assert_eq!(params[0].1, Type::TInteger);

        match *is_prime_func.body {
            Statement::Return(ref inner) => {
                match **inner {
                    Expression::MetaExp(_, ref args, ref ret_type) => {
                        assert_eq!(args.len(), 1);
                        assert_eq!(*ret_type, Type::TBool);
                    },
                    _ => panic!("Expected MetaExp inside Return for is_prime"),
                }
            },
            _ => panic!("Expected Return statement for is_prime body"),
        }
    }

    #[test]
    fn test_load_math_stdlib_contains_log(){
        let math_stdlib = load_math_stdlib();
        let log_func = math_stdlib.get("log").expect("Function 'log' not found in math stdlib");

        assert_eq!(log_func.kind, Type::TReal);

        let params = log_func.params.as_ref().expect("Expected parameters for log");
        assert_eq!(params.len(), 2);
        assert_eq!(params[0].0, "x");
        assert_eq!(params[0].1, Type::TReal);
        assert_eq!(params[1].0, "base");
        assert_eq!(params[1].1, Type::TReal);

        match *log_func.body {
            Statement::Return(ref inner) => {
                match **inner {
                    Expression::MetaExp(_, ref args, ref ret_type) => {
                        assert_eq!(args.len(), 2);
                        assert_eq!(*ret_type, Type::TReal);
                    },
                    _ => panic!("Expected MetaExp inside Return for log"),
                }
            },
            _ => panic!("Expected Return statement for log body"),
        }
    }
    
    #[test]
    fn test_load_math_stdlib_contains_degrees(){
        let math_stdlib = load_math_stdlib();
        let degrees_func = math_stdlib.get("degrees").expect("Function 'degrees' not found in math stdlib");

        assert_eq!(degrees_func.kind, Type::TReal);

        let params = degrees_func.params.as_ref().expect("Expected parameters for degrees");
        assert_eq!(params.len(), 1);
        assert_eq!(params[0].0, "rad");
        assert_eq!(params[0].1, Type::TReal);

        match *degrees_func.body {
            Statement::Return(ref inner) => {
                match **inner {
                    Expression::MetaExp(_, ref args, ref ret_type) => {
                        assert_eq!(args.len(), 1);
                        assert_eq!(*ret_type, Type::TReal);
                    },
                    _ => panic!("Expected MetaExp inside Return for degrees"),
                }
            },
            _ => panic!("Expected Return statement for degrees body"),
        }
    }

    #[test]
    fn test_load_math_stdlib_contains_radians(){
        let math_stdlib = load_math_stdlib();
        let radians_func = math_stdlib.get("radians").expect("Function 'radians' not found in math stdlib");

        assert_eq!(radians_func.kind, Type::TReal);

        let params = radians_func.params.as_ref().expect("Expected parameters for radians");
        assert_eq!(params.len(), 1);
        assert_eq!(params[0].0, "deg");
        assert_eq!(params[0].1, Type::TReal);

        match *radians_func.body {
            Statement::Return(ref inner) => {
                match **inner {
                    Expression::MetaExp(_, ref args, ref ret_type) => {
                        assert_eq!(args.len(), 1);
                        assert_eq!(*ret_type, Type::TReal);
                    },
                    _ => panic!("Expected MetaExp inside Return for radians"),
                }
            },
            _ => panic!("Expected Return statement for radians body"),
        }
    }

    #[test]
    fn test_load_string_stdlib_contains_str_upper() {
        let string_stdlib = load_string_stdlib();
        let str_upper_func = string_stdlib.get("str_upper").expect("Function 'str_upper' not found in string stdlib");
        
        assert_eq!(str_upper_func.kind, Type::TString);
        
        let params = str_upper_func.params.as_ref().expect("Expected parameters for str_upper");
        assert_eq!(params.len(), 1);
        assert_eq!(params[0].0, "s");
        assert_eq!(params[0].1, Type::TString);
        
        match *str_upper_func.body {
            Statement::Return(ref inner) => {
                match **inner {
                    Expression::MetaExp(_, ref args, ref ret_type) => {
                        assert_eq!(args.len(), 1);
                        assert_eq!(*ret_type, Type::TString);
                    },
                    _ => panic!("Expected MetaExp inside Return for str_upper"),
                }
            },
            _ => panic!("Expected Return statement for str_upper body"),
        }
    }

    #[test]
    fn test_load_math_stdlib_contains_cos(){
        let math_stdlib = load_math_stdlib();
        let cos_func = math_stdlib.get("cos").expect("Function 'cos' not found in math stdlib");

        assert_eq!(cos_func.kind, Type::TReal);

        let params = cos_func.params.as_ref().expect("Expected parameters for cos");
        assert_eq!(params.len(), 1);
        assert_eq!(params[0].0, "x");
        assert_eq!(params[0].1, Type::TReal);

        match *cos_func.body {
            Statement::Return(ref inner) => {
                match **inner {
                    Expression::MetaExp(_, ref args, ref ret_type) => {
                        assert_eq!(args.len(), 1);
                        assert_eq!(*ret_type, Type::TReal);
                    },
                    _ => panic!("Expected MetaExp inside Return for cos"),
                }
            },
            _ => panic!("Expected Return statement for cos body"),
        }
    }
    
    #[test]
    fn test_load_math_stdlib_contains_sin(){
        let math_stdlib = load_math_stdlib();
        let sin_func = math_stdlib.get("sin").expect("Function 'sin' not found in math stdlib");

        assert_eq!(sin_func.kind, Type::TReal);

        let params = sin_func.params.as_ref().expect("Expected parameters for sin");
        assert_eq!(params.len(), 1);
        assert_eq!(params[0].0, "x");
        assert_eq!(params[0].1, Type::TReal);

        match *sin_func.body {
            Statement::Return(ref inner) => {
                match **inner {
                    Expression::MetaExp(_, ref args, ref ret_type) => {
                        assert_eq!(args.len(), 1);
                        assert_eq!(*ret_type, Type::TReal);
                    },
                    _ => panic!("Expected MetaExp inside Return for sin"),
                }
            },
            _ => panic!("Expected Return statement for sin body"),
        }
    }
    
    #[test]
    fn test_load_math_stdlib_contains_tan(){
        let math_stdlib = load_math_stdlib();
        let tan_func = math_stdlib.get("tan").expect("Function 'tan' not found in math stdlib");

        assert_eq!(tan_func.kind, Type::TReal);

        let params = tan_func.params.as_ref().expect("Expected parameters for tan");
        assert_eq!(params.len(), 1);
        assert_eq!(params[0].0, "x");
        assert_eq!(params[0].1, Type::TReal);

        match *tan_func.body {
            Statement::Return(ref inner) => {
                match **inner {
                    Expression::MetaExp(_, ref args, ref ret_type) => {
                        assert_eq!(args.len(), 1);
                        assert_eq!(*ret_type, Type::TReal);
                    },
                    _ => panic!("Expected MetaExp inside Return for tan"),
                }
            },
            _ => panic!("Expected Return statement for tan body"),
        }
    }

    #[test]
    fn test_load_string_stdlib_contains_str_lower() {
        let string_stdlib = load_string_stdlib();
        let str_lower_func = string_stdlib.get("str_lower").expect("Function 'str_lower' not found in string stdlib");

        assert_eq!(str_lower_func.kind, Type::TString);
        
        let params = str_lower_func.params.as_ref().expect("Expected parameters for str_lower");
        assert_eq!(params.len(), 1);
        assert_eq!(params[0].0, "s");
        assert_eq!(params[0].1, Type::TString);
        
        match *str_lower_func.body {
            Statement::Return(ref inner) => {
                match **inner {
                    Expression::MetaExp(_, ref args, ref ret_type) => {
                        assert_eq!(args.len(), 1);
                        assert_eq!(*ret_type, Type::TString);
                    },
                    _ => panic!("Expected MetaExp inside Return for str_lower"),
                }
            },
            _ => panic!("Expected Return statement for str_lower body"),
        }
    }
    #[test]
    fn test_load_string_stdlib_contains_str_length() {
        let string_stdlib = load_string_stdlib();
        let str_length_func = string_stdlib.get("str_length").expect("Function 'str_length' not found in string stdlib");
        
        assert_eq!(str_length_func.kind, Type::TInteger);
        
        let params = str_length_func.params.as_ref().expect("Expected parameters for str_length");
        assert_eq!(params.len(), 1);
        assert_eq!(params[0].0, "s");
        assert_eq!(params[0].1, Type::TString);
        
        match *str_length_func.body {
            Statement::Return(ref inner) => {
                match **inner {
                    Expression::MetaExp(_, ref args, ref ret_type) => {
                        assert_eq!(args.len(), 1);
                        assert_eq!(*ret_type, Type::TInteger);
                    },
                    _ => panic!("Expected MetaExp inside Return for str_length"),
                }
            },
            _ => panic!("Expected Return statement for str_length body"),
        }
    }

    #[test]
    fn test_load_string_stdlib_contains_str_reverse(){
        let string_stdlib = load_string_stdlib();
        let str_reverse_func = string_stdlib.get("str_reverse").expect("Function 'str_reverse' not found in string stdlib");

        assert_eq!(str_reverse_func.kind, Type::TString);

        let params = str_reverse_func.params.as_ref().expect("Expected parameters for str_reverse");
        assert_eq!(params.len(), 1);
        assert_eq!(params[0].0, "s");
        assert_eq!(params[0].1, Type::TString);

        match *str_reverse_func.body {
            Statement::Return(ref inner) => {
                match **inner {
                    Expression::MetaExp(_, ref args, ref ret_type) => {
                        assert_eq!(args.len(), 1);
                        assert_eq!(*ret_type, Type::TString);
                    },
                    _ => panic!("Expected MetaExp inside Return for str_reverse"),
                }
            },
            _ => panic!("Expected Return statement for str_reverse body"),
        }
    }   

    #[test]
    fn test_load_string_stdlib_contains_cont_chars(){
        let string_stdlib = load_string_stdlib();
        let cont_chars_func = string_stdlib.get("cont_chars").expect("Function 'cont_chars' not found in string stdlib");

        assert_eq!(cont_chars_func.kind, Type::TInteger);

        let params = cont_chars_func.params.as_ref().expect("Expected parameters for cont_chars");
        assert_eq!(params.len(), 2);
        assert_eq!(params[0].0, "s");
        assert_eq!(params[0].1, Type::TString);
        assert_eq!(params[1].0, "c");
        assert_eq!(params[1].1, Type::TString);

        match *cont_chars_func.body {
            Statement::Return(ref inner) => {
                match **inner {
                    Expression::MetaExp(_, ref args, ref ret_type) => {
                        assert_eq!(args.len(), 2);
                        assert_eq!(*ret_type, Type::TInteger);
                    },
                    _ => panic!("Expected MetaExp inside Return for cont_chars"),
                }
            },
            _ => panic!("Expected Return statement for cont_chars body"),
        }
    }

    #[test]
    fn test_load_string_stdlib_contains_filter_out_char(){
        let string_stdlib = load_string_stdlib();
        let filter_out_char_func = string_stdlib.get("filter_out_char").expect("Function 'filter_out_char' not found in string stdlib");

        assert_eq!(filter_out_char_func.kind, Type::TString);

        let params = filter_out_char_func.params.as_ref().expect("Expected parameters for filter_out_char");
        assert_eq!(params.len(), 2);
        assert_eq!(params[0].0, "s");
        assert_eq!(params[0].1, Type::TString);
        assert_eq!(params[1].0, "c");
        assert_eq!(params[1].1, Type::TString);

        match *filter_out_char_func.body {
            Statement::Return(ref inner) => {
                match **inner {
                    Expression::MetaExp(_, ref args, ref ret_type) => {
                        assert_eq!(args.len(), 2);
                        assert_eq!(*ret_type, Type::TString);
                    },
                    _ => panic!("Expected MetaExp inside Return for filter_out_char"),
                }
            },
            _ => panic!("Expected Return statement for filter_out_char body"),
        }
    }

    #[test]
    fn test_load_string_stdlib_contains_replace(){
        let string_stdlib = load_string_stdlib();
        let replace_func = string_stdlib.get("replace").expect("Function 'replace' not found in string stdlib");

        assert_eq!(replace_func.kind, Type::TString);

        let params = replace_func.params.as_ref().expect("Expected parameters for replace");
        assert_eq!(params.len(), 4);
        assert_eq!(params[0].0, "s");
        assert_eq!(params[0].1, Type::TString);
        assert_eq!(params[1].0, "old");
        assert_eq!(params[1].1, Type::TString);
        assert_eq!(params[2].0, "new");
        assert_eq!(params[2].1, Type::TString);
        assert_eq!(params[3].0, "count");
        assert_eq!(params[3].1, Type::TInteger);

        match *replace_func.body {
            Statement::Return(ref inner) => {
                match **inner {
                    Expression::MetaExp(_, ref args, ref ret_type) => {
                        assert_eq!(args.len(), 4);
                        assert_eq!(*ret_type, Type::TString);
                    },
                    _ => panic!("Expected MetaExp inside Return for replace"),
                }
            },
            _ => panic!("Expected Return statement for replace body"),
        }
    }
}