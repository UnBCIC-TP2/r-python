use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, multispace0, multispace1},
    combinator::{map, opt},
    error::Error,
    multi::separated_list0,
    sequence::{delimited, preceded, tuple},
    IResult,
};

use crate::ir::ast::Type;
use crate::ir::ast::{FormalArgument, Function, Statement};
use crate::parser::parser_common::{
    identifier, keyword, ASSERTEQ_KEYWORD, ASSERTFALSE_KEYWORD, ASSERTNEQ_KEYWORD,
    ASSERTTRUE_KEYWORD, ASSERT_KEYWORD, COLON_CHAR, COMMA_CHAR, DEF_KEYWORD, ELSE_KEYWORD,
    END_KEYWORD, EQUALS_CHAR, FOR_KEYWORD, FUNCTION_ARROW, IF_KEYWORD, IN_KEYWORD, LEFT_PAREN,
    RIGHT_PAREN, SEMICOLON_CHAR, VAL_KEYWORD, VAR_KEYWORD, WHILE_KEYWORD,
};
use crate::parser::parser_expr::parse_expression;
use crate::parser::parser_type::parse_type;

pub fn parse_statement(input: &str) -> IResult<&str, Statement> {
    alt((
        parse_var_declaration_statement,
        parse_val_declaration_statement,
        parse_assignment_statement,
        parse_if_else_statement,
        parse_while_statement,
        parse_for_statement,
        parse_assert_statement,
        parse_asserteq_statement,
        parse_assertneq_statement,
        parse_assertfalse_statement,
        parse_asserttrue_statement,
        parse_test_function_definition_statement,
        parse_function_definition_statement,
    ))(input)
}

fn parse_var_declaration_statement(input: &str) -> IResult<&str, Statement> {
    map(
        tuple((
            keyword(VAR_KEYWORD),
            identifier,
            delimited(
                multispace0,
                char::<&str, Error<&str>>(EQUALS_CHAR),
                multispace0,
            ),
            parse_expression,
        )),
        |(_, var, _, expr)| Statement::VarDeclaration(var.to_string(), Box::new(expr)),
    )(input)
}

fn parse_val_declaration_statement(input: &str) -> IResult<&str, Statement> {
    map(
        tuple((
            keyword(VAL_KEYWORD),
            identifier,
            delimited(
                multispace0,
                char::<&str, Error<&str>>(EQUALS_CHAR),
                multispace0,
            ),
            parse_expression,
        )),
        |(_, var, _, expr)| Statement::ValDeclaration(var.to_string(), Box::new(expr)),
    )(input)
}

fn parse_assignment_statement(input: &str) -> IResult<&str, Statement> {
    map(
        tuple((
            identifier,
            delimited(
                multispace0,
                char::<&str, Error<&str>>(EQUALS_CHAR),
                multispace0,
            ),
            parse_expression,
        )),
        |(var, _, expr)| Statement::Assignment(var.to_string(), Box::new(expr)),
    )(input)
}

fn parse_if_else_statement(input: &str) -> IResult<&str, Statement> {
    map(
        tuple((
            keyword(IF_KEYWORD),
            preceded(multispace1, parse_expression),
            parse_block,
            opt(preceded(
                tuple((multispace0, keyword(ELSE_KEYWORD))),
                parse_block,
            )),
        )),
        |(_, cond, then_block, else_block)| {
            Statement::IfThenElse(
                Box::new(cond),
                Box::new(then_block),
                else_block.map(Box::new),
            )
        },
    )(input)
}

fn parse_while_statement(input: &str) -> IResult<&str, Statement> {
    map(
        tuple((
            keyword(WHILE_KEYWORD),
            preceded(multispace1, parse_expression),
            parse_block,
        )),
        |(_, cond, block)| Statement::While(Box::new(cond), Box::new(block)),
    )(input)
}

fn parse_for_statement(input: &str) -> IResult<&str, Statement> {
    map(
        tuple((
            keyword(FOR_KEYWORD),
            preceded(multispace1, identifier),
            preceded(multispace0, keyword(IN_KEYWORD)),
            preceded(multispace1, parse_expression),
            parse_block,
        )),
        |(_, var, _, expr, block)| Statement::For(var.to_string(), Box::new(expr), Box::new(block)),
    )(input)
}

//TODO: Apresentar Asserts
fn parse_assert_statement(input: &str) -> IResult<&str, Statement> {
    map(
        tuple((
            keyword(ASSERT_KEYWORD),
            delimited(
                char::<&str, Error<&str>>(LEFT_PAREN),
                separated_list0(
                    tuple((
                        multispace0,
                        char::<&str, Error<&str>>(COMMA_CHAR),
                        multispace0,
                    )),
                    parse_expression,
                ),
                char::<&str, Error<&str>>(RIGHT_PAREN),
            ),
        )),
        |(_, args)| {
            if args.len() != 2 {
                panic!("Assert statement requires exactly 2 arguments");
            }
            Statement::Assert(Box::new(args[0].clone()), Box::new(args[1].clone()))
        },
    )(input)
}

fn parse_asserteq_statement(input: &str) -> IResult<&str, Statement> {
    map(
        tuple((
            keyword(ASSERTEQ_KEYWORD),
            delimited(
                char::<&str, Error<&str>>(LEFT_PAREN),
                separated_list0(
                    tuple((
                        multispace0,
                        char::<&str, Error<&str>>(COMMA_CHAR),
                        multispace0,
                    )),
                    parse_expression,
                ),
                char::<&str, Error<&str>>(RIGHT_PAREN),
            ),
        )),
        |(_, args)| {
            if args.len() != 3 {
                panic!("AssertEQ statement requires exactly 3 arguments");
            }
            Statement::AssertEQ(
                Box::new(args[0].clone()),
                Box::new(args[1].clone()),
                Box::new(args[2].clone()),
            )
        },
    )(input)
}

fn parse_assertneq_statement(input: &str) -> IResult<&str, Statement> {
    map(
        tuple((
            keyword(ASSERTNEQ_KEYWORD),
            delimited(
                char::<&str, Error<&str>>(LEFT_PAREN),
                separated_list0(
                    tuple((
                        multispace0,
                        char::<&str, Error<&str>>(COMMA_CHAR),
                        multispace0,
                    )),
                    parse_expression,
                ),
                char::<&str, Error<&str>>(RIGHT_PAREN),
            ),
        )),
        |(_, args)| {
            if args.len() != 3 {
                panic!("AssertNEQ statement requires exactly 3 arguments");
            }
            Statement::AssertNEQ(
                Box::new(args[0].clone()),
                Box::new(args[1].clone()),
                Box::new(args[2].clone()),
            )
        },
    )(input)
}

fn parse_asserttrue_statement(input: &str) -> IResult<&str, Statement> {
    map(
        tuple((
            keyword(ASSERTTRUE_KEYWORD),
            delimited(
                char::<&str, Error<&str>>(LEFT_PAREN),
                separated_list0(
                    tuple((
                        multispace0,
                        char::<&str, Error<&str>>(COMMA_CHAR),
                        multispace0,
                    )),
                    parse_expression,
                ),
                char::<&str, Error<&str>>(RIGHT_PAREN),
            ),
        )),
        |(_, args)| {
            if args.len() != 2 {
                panic!("AssertTrue statement requires exactly 2 arguments");
            }
            Statement::AssertTrue(Box::new(args[0].clone()), Box::new(args[1].clone()))
        },
    )(input)
}

fn parse_assertfalse_statement(input: &str) -> IResult<&str, Statement> {
    map(
        tuple((
            keyword(ASSERTFALSE_KEYWORD),
            delimited(
                char::<&str, Error<&str>>(LEFT_PAREN),
                separated_list0(
                    tuple((
                        multispace0,
                        char::<&str, Error<&str>>(COMMA_CHAR),
                        multispace0,
                    )),
                    parse_expression,
                ),
                char::<&str, Error<&str>>(RIGHT_PAREN),
            ),
        )),
        |(_, args)| {
            if args.len() != 2 {
                panic!("AssertFalse statement requires exactly 2 arguments");
            }
            Statement::AssertFalse(Box::new(args[0].clone()), Box::new(args[1].clone()))
        },
    )(input)
}

fn parse_function_definition_statement(input: &str) -> IResult<&str, Statement> {
    map(
        tuple((
            keyword(DEF_KEYWORD),
            preceded(multispace1, identifier),
            delimited(
                char::<&str, Error<&str>>(LEFT_PAREN),
                separated_list0(
                    tuple((
                        multispace0,
                        char::<&str, Error<&str>>(COMMA_CHAR),
                        multispace0,
                    )),
                    parse_formal_argument,
                ),
                char::<&str, Error<&str>>(RIGHT_PAREN),
            ),
            preceded(multispace0, tag(FUNCTION_ARROW)),
            preceded(multispace0, parse_type),
            parse_block,
        )),
        |(_, name, args, _, t, block)| {
            Statement::FuncDef(Function {
                name: name.to_string(),
                kind: t,
                params: args,
                body: Some(Box::new(block)),
            })
        },
    )(input)
}

//TODO: Apresentar TestDef
fn parse_test_function_definition_statement(input: &str) -> IResult<&str, Statement> {
    map(
        tuple((
            //keyword(TEST_KEYWORD),
            tag("test"),
            preceded(multispace1, identifier),
            delimited(
                char::<&str, Error<&str>>(LEFT_PAREN),
                multispace0,
                char::<&str, Error<&str>>(RIGHT_PAREN),
            ),
            parse_block,
        )),
        |(_, name, _, block)| {
            Statement::TestDef(Function {
                name: name.to_string(),
                kind: Type::TVoid,  // Sempre void
                params: Vec::new(), // Nenhum argumento
                body: Some(Box::new(block)),
            })
        },
    )(input)
}

fn parse_block(input: &str) -> IResult<&str, Statement> {
    map(
        tuple((
            char::<&str, Error<&str>>(COLON_CHAR),
            multispace0,
            separated_list0(
                delimited(
                    multispace0,
                    char::<&str, Error<&str>>(SEMICOLON_CHAR),
                    multispace0,
                ),
                parse_statement,
            ),
            opt(preceded(
                multispace0,
                char::<&str, Error<&str>>(SEMICOLON_CHAR),
            )),
            delimited(multispace0, keyword(END_KEYWORD), multispace0),
        )),
        |(_, _, stmts, _, _)| Statement::Block(stmts),
    )(input)
}

fn parse_formal_argument(input: &str) -> IResult<&str, FormalArgument> {
    map(
        tuple((
            preceded(multispace0, identifier),
            preceded(multispace0, char::<&str, Error<&str>>(COLON_CHAR)),
            preceded(multispace0, parse_type),
        )),
        |(name, _, t)| FormalArgument::new(name.to_string(), t),
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::ast::{Expression, FormalArgument, Function, Statement, Type};

    #[test]
    fn test_parse_assignment_statement() {
        let input = "x = 42";
        let expected = Statement::Assignment("x".to_string(), Box::new(Expression::CInt(42)));
        let parsed = parse_assignment_statement(input).unwrap().1;
        assert_eq!(parsed, expected);
    }

    #[test]
    #[ignore]
    fn test_parse_if_else_statement() {
        let input = "if True: x = 1; end";
        let expected = Statement::IfThenElse(
            Box::new(Expression::CTrue),
            Box::new(Statement::Block(vec![Statement::Assignment(
                "x".to_string(),
                Box::new(Expression::CInt(1)),
            )])),
            None,
        );
        let parsed = parse_if_else_statement(input).unwrap().1;
        assert_eq!(parsed, expected);
    }

    #[test]
    #[ignore]
    fn test_parse_while_statement() {
        let input = "while True: x = 1; end";
        let expected = Statement::While(
            Box::new(Expression::CTrue),
            Box::new(Statement::Block(vec![Statement::Assignment(
                "x".to_string(),
                Box::new(Expression::CInt(1)),
            )])),
        );
        let parsed = parse_while_statement(input).unwrap().1;
        assert_eq!(parsed, expected);
    }

    #[test]
    #[ignore]
    fn test_parse_for_statement() {
        let input = "for x in y: x = 1; end";
        let expected = Statement::For(
            "x".to_string(),
            Box::new(Expression::Var("y".to_string())),
            Box::new(Statement::Block(vec![Statement::Assignment(
                "x".to_string(),
                Box::new(Expression::CInt(1)),
            )])),
        );
        let parsed = parse_for_statement(input).unwrap().1;
        assert_eq!(parsed, expected);
    }

    #[test]
    #[ignore]
    fn test_parse_function_definition_statement() {
        let input = "def f(x: Int) -> Int: x = 1; end";
        let expected = Statement::FuncDef(Function {
            name: "f".to_string(),
            kind: Type::TInteger,
            params: vec![FormalArgument::new("x".to_string(), Type::TInteger)],
            body: Some(Box::new(Statement::Block(vec![Statement::Assignment(
                "x".to_string(),
                Box::new(Expression::CInt(1)),
            )]))),
        });
        let parsed = parse_function_definition_statement(input).unwrap().1;
        assert_eq!(parsed, expected);
    }

    #[test]
    fn test_parse_block() {
        let input = ": x = 1; end";
        let expected = Statement::Block(vec![Statement::Assignment(
            "x".to_string(),
            Box::new(Expression::CInt(1)),
        )]);
        let (rest, parsed) = parse_block(input).unwrap();
        assert_eq!(rest, "");
        assert_eq!(parsed, expected);

        let input = ": x = 1; y = x + 1; end";
        let expected = Statement::Block(vec![
            Statement::Assignment("x".to_string(), Box::new(Expression::CInt(1))),
            Statement::Assignment(
                "y".to_string(),
                Box::new(Expression::Add(
                    Box::new(Expression::Var("x".to_string())),
                    Box::new(Expression::CInt(1)),
                )),
            ),
        ]);
        let (rest, parsed) = parse_block(input).unwrap();
        assert_eq!(rest, "");
        assert_eq!(parsed, expected);
    }

    #[test]
    fn test_parse_formal_argument() {
        let input = "x: Int";
        let expected = FormalArgument {
            argument_name: "x".to_string(),
            argument_type: Type::TInteger,
        };
        let parsed = parse_formal_argument(input).unwrap().1;
        assert_eq!(parsed, expected);
    }

    //TODO: Apresentar Parser de TestDef (Testes)
    mod testdef_tests {
        use super::*;

        #[test]
        fn test_parse_test_function_definition_statement_valid() {
            let input = "test test_example(): x = 1; end";
            let expected = Statement::TestDef(Function {
                name: "test_example".to_string(),
                kind: Type::TVoid,
                params: vec![],
                body: Some(Box::new(Statement::Block(vec![Statement::Assignment(
                    "x".to_string(),
                    Box::new(Expression::CInt(1)),
                )]))),
            });
            let parsed = parse_test_function_definition_statement(input).unwrap().1;
            assert_eq!(parsed, expected);
        }

        #[test]
        fn test_parse_test_function_definition_statement_valid_multiple_statements() {
            let input = r#"test test_example():
                x = 1;
                y = 2;
                assert(x == 1, "x deveria ser 1");
            end"#;
            let expected = Statement::TestDef(Function {
                name: "test_example".to_string(),
                kind: Type::TVoid,
                params: vec![],
                body: Some(Box::new(Statement::Block(vec![
                    Statement::Assignment("x".to_string(), Box::new(Expression::CInt(1))),
                    Statement::Assignment("y".to_string(), Box::new(Expression::CInt(2))),
                    Statement::Assert(
                        Box::new(Expression::EQ(
                            Box::new(Expression::Var("x".to_string())),
                            Box::new(Expression::CInt(1)),
                        )),
                        Box::new(Expression::CString("x deveria ser 1".to_string())),
                    ),
                ]))),
            });
            let parsed = parse_test_function_definition_statement(input).unwrap().1;
            assert_eq!(parsed, expected);
        }

        #[test]
        fn test_parse_test_function_definition_statement_with_spaces() {
            let input = "test test_spaces(   ): x = 2; end";
            let expected = Statement::TestDef(Function {
                name: "test_spaces".to_string(),
                kind: Type::TVoid,
                params: vec![],
                body: Some(Box::new(Statement::Block(vec![Statement::Assignment(
                    "x".to_string(),
                    Box::new(Expression::CInt(2)),
                )]))),
            });
            let parsed = parse_test_function_definition_statement(input).unwrap().1;
            assert_eq!(parsed, expected);
        }

        #[test]
        fn test_parse_test_function_definition_statement_args() {
            let input = "test test_with_args(x: Int, y: Int): x = y; end";

            // O parser deve falhar, pois funções de teste não podem ter argumentos.
            let parsed = parse_test_function_definition_statement(input);

            assert!(
                parsed.is_err(),
                "Funções de teste com argumentos devem ser rejeitadas"
            );
        }

        #[test]
        fn test_parse_test_function_definition_statement_invalid_return() {
            let input = "test test_with_invalid_return() -> Int: x = 2; end";

            // O parser deve falhar, pois funções de teste não podem ter argumentos.
            let parsed = parse_test_function_definition_statement(input);

            assert!(
                parsed.is_err(),
                "Funções de teste não devem especificar tipo de retorno"
            );
        }

        #[test]
        fn test_parse_test_function_definition_statement_valid_return_type() {
            let input = "test test_with_valid_return() -> Boolean: x = 2; end";

            let parsed = parse_test_function_definition_statement(input);
            assert!(
                parsed.is_err(),
                "Funções de teste não devem especificar tipo de retorno"
            );
        }
    }

    //TODO: Apresentar Parser de Asserts (Testes)
    mod assert_tests {
        use super::*;

        #[test]
        fn test_parse_assert_statement() {
            let input = "assert(1 == 2, \"expecting an error\")";
            let expected = Statement::Assert(
                Box::new(Expression::EQ(
                    Box::new(Expression::CInt(1)),
                    Box::new(Expression::CInt(2)),
                )),
                Box::new(Expression::CString("expecting an error".to_string())),
            );
            let parsed = parse_assert_statement(input).unwrap().1;
            assert_eq!(parsed, expected);
        }

        #[test]
        fn test_parse_asserteq_statement() {
            let input = "asserteq(1, 2, \"msg\")";
            let expected = Statement::AssertEQ(
                Box::new(Expression::CInt(1)),
                Box::new(Expression::CInt(2)),
                Box::new(Expression::CString("msg".to_string())),
            );
            let parsed = parse_asserteq_statement(input).unwrap().1;
            assert_eq!(parsed, expected);
        }

        #[test]
        fn test_parse_assertneq_statement() {
            let input = "assertneq(3, 4, \"fail\")";
            let expected = Statement::AssertNEQ(
                Box::new(Expression::CInt(3)),
                Box::new(Expression::CInt(4)),
                Box::new(Expression::CString("fail".to_string())),
            );
            let parsed = parse_assertneq_statement(input).unwrap().1;
            assert_eq!(parsed, expected);
        }

        #[test]
        fn test_parse_asserttrue_statement() {
            let input = "asserttrue(True, \"should be true\")";
            let expected = Statement::AssertTrue(
                Box::new(Expression::CTrue),
                Box::new(Expression::CString("should be true".to_string())),
            );
            let parsed = parse_asserttrue_statement(input).unwrap().1;
            assert_eq!(parsed, expected);
        }

        #[test]
        fn test_parse_assertfalse_statement() {
            let input = "assertfalse(False, \"should be false\")";
            let expected = Statement::AssertFalse(
                Box::new(Expression::CFalse),
                Box::new(Expression::CString("should be false".to_string())),
            );
            let parsed = parse_assertfalse_statement(input).unwrap().1;
            assert_eq!(parsed, expected);
        }

        #[test]
        fn test_parse_assert_statement_invalid_argnumber() {
            let input = "assert(False, False, \"should be false\")";

            let result = std::panic::catch_unwind(|| parse_assert_statement(input));

            assert!(
                result.is_err(),
                "Expected panic for invalid number of arguments"
            );

            if let Err(err) = result {
                if let Some(s) = err.downcast_ref::<&str>() {
                    assert_eq!(*s, "Assert statement requires exactly 2 arguments");
                } else if let Some(s) = err.downcast_ref::<String>() {
                    assert_eq!(s, "Assert statement requires exactly 2 arguments");
                } else {
                    panic!("Panic occurred, but message is not a string");
                }
            }
        }

        #[test]
        fn test_parse_asserteq_statement_invalid_argnumber() {
            let input = "asserteq(1, 2, 3, \"msg\")";

            let result = std::panic::catch_unwind(|| parse_asserteq_statement(input));

            assert!(
                result.is_err(),
                "Expected panic for invalid number of arguments"
            );

            if let Err(err) = result {
                if let Some(s) = err.downcast_ref::<&str>() {
                    assert_eq!(*s, "AssertEQ statement requires exactly 3 arguments");
                } else if let Some(s) = err.downcast_ref::<String>() {
                    assert_eq!(s, "AssertEQ statement requires exactly 3 arguments");
                } else {
                    panic!("Panic occurred, but message is not a string");
                }
            }
        }

        #[test]
        fn test_parse_assertneq_statement_invalid_argnumber() {
            let input = "assertneq(3, 4, 5, \"fail\")";

            let result = std::panic::catch_unwind(|| parse_assertneq_statement(input));

            assert!(
                result.is_err(),
                "Expected panic for invalid number of arguments"
            );

            if let Err(err) = result {
                if let Some(s) = err.downcast_ref::<&str>() {
                    assert_eq!(*s, "AssertNEQ statement requires exactly 3 arguments");
                } else if let Some(s) = err.downcast_ref::<String>() {
                    assert_eq!(s, "AssertNEQ statement requires exactly 3 arguments");
                } else {
                    panic!("Panic occurred, but message is not a string");
                }
            }
        }

        #[test]
        fn test_parse_asserttrue_statement_invalid_argnumber() {
            let input = "asserttrue(True, True, \"should be true\")";

            let result = std::panic::catch_unwind(|| parse_asserttrue_statement(input));

            assert!(
                result.is_err(),
                "Expected panic for invalid number of arguments"
            );

            if let Err(err) = result {
                if let Some(s) = err.downcast_ref::<&str>() {
                    assert_eq!(*s, "AssertTrue statement requires exactly 2 arguments");
                } else if let Some(s) = err.downcast_ref::<String>() {
                    assert_eq!(s, "AssertTrue statement requires exactly 2 arguments");
                } else {
                    panic!("Panic occurred, but message is not a string");
                }
            }
        }

        #[test]
        fn test_parse_assertfalse_statement_invalid_argnumber() {
            let input = "assertfalse(False, False, \"should be false\")";

            let result = std::panic::catch_unwind(|| parse_assertfalse_statement(input));

            assert!(
                result.is_err(),
                "Expected panic for invalid number of arguments"
            );
            if let Err(err) = result {
                if let Some(s) = err.downcast_ref::<&str>() {
                    assert_eq!(*s, "AssertFalse statement requires exactly 2 arguments");
                } else if let Some(s) = err.downcast_ref::<String>() {
                    assert_eq!(s, "AssertFalse statement requires exactly 2 arguments");
                } else {
                    panic!("Panic occurred, but message is not a string");
                }
            }
        }
    }
}
