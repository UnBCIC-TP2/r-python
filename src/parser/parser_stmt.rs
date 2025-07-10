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

use crate::ir::ast::{FormalArgument, Function, Name, Statement};
use crate::parser::parser_common::{
    identifier, keyword, flexible_keyword, ASSERT_KEYWORD, COLON_CHAR, COMMA_CHAR, DEF_KEYWORD, ELSE_KEYWORD,
    END_KEYWORD, EQUALS_CHAR, FOR_KEYWORD, FUNCTION_ARROW, IF_KEYWORD, IN_KEYWORD, LEFT_PAREN,
    RIGHT_PAREN, SEMICOLON_CHAR, VAL_KEYWORD, VAR_KEYWORD, WHILE_KEYWORD, MATCH_KEYWORD,
    MATCH_ARM_ARROW, LEFT_BRACE, RIGHT_BRACE,
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
        parse_function_definition_statement,
        parse_match_statement,
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

fn parse_match_statement(input: &str) -> IResult<&str, Statement> {
    map(
        tuple((
            flexible_keyword(MATCH_KEYWORD),
            delimited(
                multispace0,
                parse_expression,
                multispace0,
            ),
            char(COLON_CHAR),
            multispace1,
            separated_list0(
                delimited(
                    multispace0,
                    char(COMMA_CHAR),
                    multispace1,
                ),
                parse_match_arm,
            ),
            delimited(
                multispace1,
                tag(END_KEYWORD),
                multispace1,
            ),
            flexible_keyword(MATCH_KEYWORD),
        )),
        |(_, expr, _, _, arms, _, _)| Statement::Match(Box::new(expr), arms),
    )(input)
}

fn parse_match_arm(input: &str) -> IResult<&str, ((Name, Vec<Name>), Statement)> {
    map(
        tuple((
            preceded(multispace0, identifier),
            opt(delimited(
                    preceded(multispace0, char::<&str, Error<&str>>(LEFT_PAREN)),
                    separated_list0(
                        delimited(multispace0, char::<&str, Error<&str>>(COMMA_CHAR), multispace0),
                        identifier,
                    ),
                    preceded(multispace0, char::<&str, Error<&str>>(RIGHT_PAREN)),
                )
            ),
            preceded(
                delimited(
                    multispace0,
                    tag(MATCH_ARM_ARROW),
                    multispace0,
                ),
                delimited(
                    char::<&str, Error<&str>>(LEFT_BRACE),
                    delimited(multispace0, parse_block, multispace0),
                    char::<&str, Error<&str>>(RIGHT_BRACE),
                ),
            ),
        )),
        |(constructor, parameters, statement)| ((constructor.to_string(), parameters.unwrap_or_default().into_iter().map(|p| p.to_string()).collect()), statement),
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

    #[test]
    fn test_parse_match_statement_simple() {
        let input = "match x: A => { : x = 1; end }, B => { : x = 2; end } end match";
        let result: IResult<&str, Statement> = parse_match_statement(input);
        assert!(result.is_ok());
        
        let (rest, parsed) = result.unwrap();
        assert_eq!(rest, "");
        
        match parsed {
            Statement::Match(expr, arms) => {
                assert_eq!(*expr, Expression::Var("x".to_string()));
                assert_eq!(arms.len(), 2);
                
                // Verificar primeiro braço: A => { : x = 1; end }
                assert_eq!(arms[0].0.0, "A");
                assert_eq!(arms[0].0.1, Vec::<Name>::new()); // Sem parâmetros
                
                // Verificar segundo braço: B => { : x = 2; end }
                assert_eq!(arms[1].0.0, "B");
                assert_eq!(arms[1].0.1, Vec::<Name>::new()); // Sem parâmetros
            }
            _ => panic!("Expected Match statement"),
        }
    }

    #[test]
    fn test_parse_match_statement_with_parameters() {
        let input = "match x: A(value) => { : x = value; end }, B(x, y) => { : z = x + y; end } end match";
        let result: IResult<&str, Statement> = parse_match_statement(input);
        assert!(result.is_ok());
        
        let (rest, parsed) = result.unwrap();
        assert_eq!(rest, "");
        
        match parsed {
            Statement::Match(expr, arms) => {
                assert_eq!(*expr, Expression::Var("x".to_string()));
                assert_eq!(arms.len(), 2);
                
                // Verificar primeiro braço: A(value) => { : x = value; end }
                assert_eq!(arms[0].0.0, "A");
                assert_eq!(arms[0].0.1, vec!["value".to_string()]);
                
                // Verificar segundo braço: B(x, y) => { : z = x + y; end }
                assert_eq!(arms[1].0.0, "B");
                assert_eq!(arms[1].0.1, vec!["x".to_string(), "y".to_string()]);
            }
            _ => panic!("Expected Match statement"),
        }
    }

    #[test]
    fn test_parse_match_statement_complex_expression() {
        let input = "match f(x) + 1: A => { : x = 1; end }, B(value) => { : y = value * 2; end } end match";
        let result: IResult<&str, Statement> = parse_match_statement(input);
        assert!(result.is_ok());
        
        let (rest, parsed) = result.unwrap();
        assert_eq!(rest, "");
        
        match parsed {
            Statement::Match(expr, arms) => {
                // Verificar que a expressão é f(x) + 1
                match &*expr {
                    Expression::Add(left, right) => {
                        match &**left {
                            Expression::FuncCall(name, args) => {
                                assert_eq!(name, "f");
                                assert_eq!(args.len(), 1);
                            }
                            _ => panic!("Expected function call"),
                        }
                        match &**right {
                            Expression::CInt(1) => {}
                            _ => panic!("Expected integer 1"),
                        }
                    }
                    _ => panic!("Expected Add expression"),
                }
                
                assert_eq!(arms.len(), 2);
                assert_eq!(arms[0].0.0, "A");
                assert_eq!(arms[1].0.0, "B");
            }
            _ => panic!("Expected Match statement"),
        }
    }

    #[test]
    fn test_parse_match_statement_multiple_arms() {
        let input = "match x: A => { : x = 1; end }, B => { : x = 2; end }, C(value) => { : x = value; end } end match";
        let result: IResult<&str, Statement> = parse_match_statement(input);
        assert!(result.is_ok());
        
        let (rest, parsed) = result.unwrap();
        assert_eq!(rest, "");
        
        match parsed {
            Statement::Match(expr, arms) => {
                assert_eq!(*expr, Expression::Var("x".to_string()));
                assert_eq!(arms.len(), 3);
                
                assert_eq!(arms[0].0.0, "A");
                assert_eq!(arms[0].0.1, Vec::<Name>::new());
                
                assert_eq!(arms[1].0.0, "B");
                assert_eq!(arms[1].0.1, Vec::<Name>::new());
                
                assert_eq!(arms[2].0.0, "C");
                assert_eq!(arms[2].0.1, vec!["value".to_string()]);
            }
            _ => panic!("Expected Match statement"),
        }
    }

    #[test]
    fn test_parse_match_statement_empty_arms() {
        let input = "match x: A => { : end }, B => { : end } end match";
        let result: IResult<&str, Statement> = parse_match_statement(input);
        assert!(result.is_ok());
        
        let (rest, parsed) = result.unwrap();
        assert_eq!(rest, "");
        
        match parsed {
            Statement::Match(expr, arms) => {
                assert_eq!(*expr, Expression::Var("x".to_string()));
                assert_eq!(arms.len(), 2);
                
                // Verificar que os blocos estão vazios
                match &arms[0].1 {
                    Statement::Block(statements) => {
                        assert_eq!(statements.len(), 0);
                    }
                    _ => panic!("Expected Block statement"),
                }
                
                match &arms[1].1 {
                    Statement::Block(statements) => {
                        assert_eq!(statements.len(), 0);
                    }
                    _ => panic!("Expected Block statement"),
                }
            }
            _ => panic!("Expected Match statement"),
        }
    }

    #[test]
    fn test_parse_match_statement_with_whitespace() {
        let input = "match   x   :   A   =>   {   :   x   =   1   ;   end   }   ,   B   =>   {   :   x   =   2   ;   end   }   end   match";
        let result: IResult<&str, Statement> = parse_match_statement(input);
        assert!(result.is_ok());
        
        let (rest, parsed) = result.unwrap();
        assert_eq!(rest, "");
        
        match parsed {
            Statement::Match(expr, arms) => {
                assert_eq!(*expr, Expression::Var("x".to_string()));
                assert_eq!(arms.len(), 2);
                assert_eq!(arms[0].0.0, "A");
                assert_eq!(arms[1].0.0, "B");
            }
            _ => panic!("Expected Match statement"),
        }
    }

    #[test]
    fn test_parse_match_arm_simple() {
        let input = "A => { : x = 1; end }";
        let result: IResult<&str, ((Name, Vec<Name>), Statement)> = parse_match_arm(input);
        assert!(result.is_ok());
        
        let (rest, parsed) = result.unwrap();
        assert_eq!(rest, "");
        
        let ((constructor, params), statement) = parsed;
        assert_eq!(constructor, "A");
        assert_eq!(params, Vec::<Name>::new());
        
        match statement {
            Statement::Block(statements) => {
                assert_eq!(statements.len(), 1);
                match &statements[0] {
                    Statement::Assignment(name, expr) => {
                        assert_eq!(name, "x");
                        assert_eq!(**expr, Expression::CInt(1));
                    }
                    _ => panic!("Expected Assignment statement"),
                }
            }
            _ => panic!("Expected Block statement"),
        }
    }

    #[test]
    fn test_parse_match_arm_with_parameters() {
        let input = "A(value1, value2) => { : x = value1 + value2; end }";
        let result: IResult<&str, ((Name, Vec<Name>), Statement)> = parse_match_arm(input);
        assert!(result.is_ok());
        
        let (rest, parsed) = result.unwrap();
        assert_eq!(rest, "");
        
        let ((constructor, params), statement) = parsed;
        assert_eq!(constructor, "A");
        assert_eq!(params, vec!["value1".to_string(), "value2".to_string()]);
        
        match statement {
            Statement::Block(statements) => {
                assert_eq!(statements.len(), 1);
                match &statements[0] {
                    Statement::Assignment(name, expr) => {
                        assert_eq!(name, "x");
                        match &**expr {
                            Expression::Add(left, right) => {
                                assert_eq!(*left, Box::new(Expression::Var("value1".to_string())));
                                assert_eq!(*right, Box::new(Expression::Var("value2".to_string())));
                            }
                            _ => panic!("Expected Add expression"),
                        }
                    }
                    _ => panic!("Expected Assignment statement"),
                }
            }
            _ => panic!("Expected Block statement"),
        }
    }

    #[test]
    fn test_parse_match_statement_invalid_missing_end() {
        let input = "match x: A => { : x = 1; end }";
        let result = parse_match_statement(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_match_statement_invalid_missing_expression() {
        let input = "match : A => { : x = 1; end } end match";
        let result = parse_match_statement(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_match_statement_invalid_missing_arms() {
        let input = "match x: end match";
        let result = parse_match_statement(input);
        assert!(result.is_err());
    }
}
