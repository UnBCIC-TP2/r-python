use r_python::ir::ast::*;
use r_python::parser::{parse, parse_expression, parse_statement};

// Basic Expression Tests
mod expression_tests {
    use super::*;

    #[test]
    fn test_literals() {
        let cases = vec![
            ("42", Expression::CInt(42)),
            ("3.14", Expression::CReal(3.14)),
            ("\"hello\"", Expression::CString("hello".to_string())),
            ("True", Expression::CTrue),
            ("False", Expression::CFalse),
        ];

        for (input, expected) in cases {
            let (rest, result) = parse_expression(input).unwrap();
            assert_eq!(rest, "");
            assert_eq!(result, expected);
        }
    }

    #[test]
    fn test_arithmetic_operations() {
        let cases = vec![
            (
                "1 + 2",
                Expression::Add(Box::new(Expression::CInt(1)), Box::new(Expression::CInt(2))),
            ),
            (
                "3 * 4",
                Expression::Mul(Box::new(Expression::CInt(3)), Box::new(Expression::CInt(4))),
            ),
            (
                "2 + 3 * 4", // Tests precedence
                Expression::Add(
                    Box::new(Expression::CInt(2)),
                    Box::new(Expression::Mul(
                        Box::new(Expression::CInt(3)),
                        Box::new(Expression::CInt(4)),
                    )),
                ),
            ),
            (
                "(1 + 2) * (3 + 4)", // Tests grouping
                Expression::Mul(
                    Box::new(Expression::Add(
                        Box::new(Expression::CInt(1)),
                        Box::new(Expression::CInt(2)),
                    )),
                    Box::new(Expression::Add(
                        Box::new(Expression::CInt(3)),
                        Box::new(Expression::CInt(4)),
                    )),
                ),
            ),
        ];

        for (input, expected) in cases {
            let (rest, result) = parse_expression(input).unwrap();
            assert_eq!(rest, "");
            assert_eq!(result, expected);
        }
    }

    #[test]
    #[ignore]
    fn test_boolean_operations() {
        let cases = vec![
            (
                "True and False",
                Expression::And(Box::new(Expression::CTrue), Box::new(Expression::CFalse)),
            ),
            (
                "True or False",
                Expression::Or(Box::new(Expression::CTrue), Box::new(Expression::CFalse)),
            ),
            ("not True", Expression::Not(Box::new(Expression::CTrue))),
            (
                "not (True and False) or True",
                Expression::Or(
                    Box::new(Expression::Not(Box::new(Expression::And(
                        Box::new(Expression::CTrue),
                        Box::new(Expression::CFalse),
                    )))),
                    Box::new(Expression::CTrue),
                ),
            ),
        ];

        for (input, expected) in cases {
            let (rest, result) = parse_expression(input).unwrap();
            assert_eq!(rest, "");
            assert_eq!(result, expected);
        }
    }

    #[test]
    fn test_function_calls() {
        let input = "add(5, 3)";
        let expected = Expression::FuncCall(
            "add".to_string(),
            vec![Expression::CInt(5), Expression::CInt(3)],
        );

        let (rest, result) = parse_expression(input).unwrap();
        assert_eq!(rest, "");
        assert_eq!(result, expected);
    }
}

// Statement Tests
mod statement_tests {
    use super::*;

    #[test]
    fn test_assignments() {
        let cases = vec![
            (
                "x = 42",
                Statement::Assignment("x".to_string(), Box::new(Expression::CInt(42))),
            ),
            (
                "result = add(5, 3)",
                Statement::Assignment(
                    "result".to_string(),
                    Box::new(Expression::FuncCall(
                        "add".to_string(),
                        vec![Expression::CInt(5), Expression::CInt(3)],
                    )),
                ),
            ),
        ];

        for (input, expected) in cases {
            let (rest, result) = parse_statement(input).unwrap();
            assert_eq!(rest, "");
            assert_eq!(result, expected);
        }
    }

    #[test]
    #[ignore]
    fn test_if_statements() {
        let input = "if x > 0: y = 1; end";
        let expected = Statement::IfThenElse(
            Box::new(Expression::GT(
                Box::new(Expression::Var("x".to_string())),
                Box::new(Expression::CInt(0)),
            )),
            Box::new(Statement::Block(vec![Statement::Assignment(
                "y".to_string(),
                Box::new(Expression::CInt(1)),
            )])),
            None,
        );

        let (rest, result) = parse_statement(input).unwrap();
        assert_eq!(rest, "");
        assert_eq!(result, expected);

        // Test with else
        let input = "if x > 0: y = 1; end else: y = 2; end";
        let expected = Statement::IfThenElse(
            Box::new(Expression::GT(
                Box::new(Expression::Var("x".to_string())),
                Box::new(Expression::CInt(0)),
            )),
            Box::new(Statement::Block(vec![Statement::Assignment(
                "y".to_string(),
                Box::new(Expression::CInt(1)),
            )])),
            Some(Box::new(Statement::Block(vec![Statement::Assignment(
                "y".to_string(),
                Box::new(Expression::CInt(2)),
            )]))),
        );

        let (rest, result) = parse_statement(input).unwrap();
        assert_eq!(rest, "");
        assert_eq!(result, expected);
    }

    #[test]
    #[ignore]
    fn test_for_statements() {
        let input = "for x in range: x = x + 1; end";
        let expected = Statement::For(
            "x".to_string(),
            Box::new(Expression::Var("range".to_string())),
            Box::new(Statement::Block(vec![Statement::Assignment(
                "x".to_string(),
                Box::new(Expression::Add(
                    Box::new(Expression::Var("x".to_string())),
                    Box::new(Expression::CInt(1)),
                )),
            )])),
        );

        let (rest, result) = parse_statement(input).unwrap();
        assert_eq!(rest, "");
        assert_eq!(result, expected);
    }

    #[test]
    #[ignore]
    fn test_function_definitions() {
        let input = "def add(x: Int, y: Int) -> Int: return x + y; end";
        let expected = Statement::FuncDef(Function {
            name: "add".to_string(),
            kind: Type::TInteger,
            params: vec![
                FormalArgument::new("x".to_string(), Type::TInteger),
                FormalArgument::new("y".to_string(), Type::TInteger),
            ],
            body: Some(Box::new(Statement::Block(vec![Statement::Return(
                Box::new(Expression::Add(
                    Box::new(Expression::Var("x".to_string())),
                    Box::new(Expression::Var("y".to_string())),
                )),
            )]))),
            builtin: None,
        });

        let (rest, result) = parse_statement(input).unwrap();
        assert_eq!(rest, "");
        assert_eq!(result, expected);
    }

    #[test]
    #[ignore]
    fn test_var_declarations() {
        let cases = vec![
            (
                "var x = 42",
                Statement::VarDeclaration("x".to_string(), Box::new(Expression::CInt(42))),
            ),
            // (
            //     "var result = add(5, 3)",
            //     Statement::VarDeclaration(
            //         "result".to_string(),
            //         Box::new(Expression::FuncCall(
            //             "add".to_string(),
            //             vec![Expression::CInt(5), Expression::CInt(3)],
            //         )),
            //     ),
            // ),
            // (
            //     "var name = \"John\"",
            //     Statement::VarDeclaration(
            //         "name".to_string(),
            //         Box::new(Expression::CString("John".to_string())),
            //     ),
            // ),
            // (
            //     "var is_valid = True",
            //     Statement::VarDeclaration(
            //         "is_valid".to_string(),
            //         Box::new(Expression::CTrue),
            //     ),
            // ),
        ];

        for (input, expected) in cases {
            let (rest, result) = parse_statement(input).unwrap();
            assert_eq!(rest, "");
            assert_eq!(result, expected);
        }
    }

    #[test]
    #[ignore]
    fn test_val_declarations() {
        let cases = vec![
            (
                "val x = 42",
                Statement::ValDeclaration("x".to_string(), Box::new(Expression::CInt(42))),
            ),
            (
                "val result = add(5, 3)",
                Statement::ValDeclaration(
                    "result".to_string(),
                    Box::new(Expression::FuncCall(
                        "add".to_string(),
                        vec![Expression::CInt(5), Expression::CInt(3)],
                    )),
                ),
            ),
            (
                "val name = \"John\"",
                Statement::ValDeclaration(
                    "name".to_string(),
                    Box::new(Expression::CString("John".to_string())),
                ),
            ),
            (
                "val is_valid = True",
                Statement::ValDeclaration("is_valid".to_string(), Box::new(Expression::CTrue)),
            ),
        ];

        for (input, expected) in cases {
            let (rest, result) = parse_statement(input).unwrap();
            assert_eq!(rest, "");
            assert_eq!(result, expected);
        }
    }
}

// ADT Tests
mod adt_tests {
    use super::*;

    #[test]
    #[ignore]
    fn test_adt_declarations() {
        let input = "data Shape = Circle Int | Rectangle Int Int";
        let expected = Statement::TypeDeclaration(
            "Shape".to_string(),
            vec![
                ValueConstructor::new("Circle".to_string(), vec![Type::TInteger]),
                ValueConstructor::new(
                    "Rectangle".to_string(),
                    vec![Type::TInteger, Type::TInteger],
                ),
            ],
        );

        let (rest, result) = parse_statement(input).unwrap();
        assert_eq!(rest, "");
        assert_eq!(result, expected);
    }
}

// Error Handling Tests
mod error_tests {
    use super::*;

    #[test]
    fn test_invalid_keywords() {
        let invalid_cases = vec![
            "def if(x: Int) -> Int:\n    return x",
            "def while(x: Int) -> Int:\n    return x",
            "if = 10",
            "while = 10",
        ];

        for input in invalid_cases {
            assert!(parse_statement(input).is_err());
        }
    }

    #[test]
    #[ignore]
    fn test_invalid_expressions() {
        let invalid_cases = vec![
            "1 + ",    // Incomplete expression
            "* 2",     // Missing left operand
            "1 + + 2", // Double operator
            "(1 + 2",  // Unclosed parenthesis
            "1 + 2)",  // Extra closing parenthesis
        ];

        for input in invalid_cases {
            assert!(parse_expression(input).is_err());
        }
    }
}

// Complete Program Tests
mod program_tests {
    use super::*;

    #[test]
    #[ignore]
    fn test_complete_program() {
        let input = r#"
def factorial(n: Int) -> Int:
    if n <= 1:
        return 1;
    end
    else:
        return n * factorial(n - 1);
    end
end;

x = factorial(5);
assert(x == 120, "factorial of 5 should be 120");"#;
        let result = parse(input);
        assert!(result.is_ok());
        let (rest, statements) = result.unwrap();
        assert_eq!(rest.trim(), "");
        assert!(statements.len() >= 3); // Function definition, assignment, and assert
    }

    #[test]
    fn test_program_with_adt() {
        let input = r#"
data Shape = Circle Int | Rectangle Int Int;

def area(shape: Shape) -> Int:
    if isCircle(shape):
        r = getCircleRadius(shape);
        return r * r * 3;
    end
    else:
        w = getRectangleWidth(shape);
        h = getRectangleHeight(shape);
        return w * h;
    end
end;

c = Circle(5);
area_c = area(c);
assert(area_c == 75, "area of circle with radius 5 should be 75");"#;
        let result = parse(input);
        assert!(result.is_ok());
    }
}
