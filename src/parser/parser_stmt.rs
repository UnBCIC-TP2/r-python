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

use crate::ir::ast::{FormalArgument, Function, Statement};
use crate::ir::ast::Expression;
use crate::parser::parser_common::{
    identifier, keyword, flexible_keyword, ASSERT_KEYWORD, COLON_CHAR, COMMA_CHAR, DEF_KEYWORD, ELSE_KEYWORD,
    END_KEYWORD, EQUALS_CHAR, FOR_KEYWORD, FUNCTION_ARROW, IF_KEYWORD, IN_KEYWORD, LEFT_PAREN,
    RIGHT_PAREN, SEMICOLON_CHAR, VAL_KEYWORD, VAR_KEYWORD, WHILE_KEYWORD, MATCH_KEYWORD,
    MATCH_ARM_ARROW, LEFT_BRACE, RIGHT_BRACE,
};
use crate::parser::parser_expr::parse_expression;
use crate::parser::parse_pattern::parse_pattern_argument;
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
            preceded(
                multispace1,
                tag(END_KEYWORD)
            ),
            opt(delimited(
                multispace1, 
                tag(MATCH_KEYWORD), 
                multispace0
            )),
        )),
        |(_, expr, _, _, arms, _, _)| Statement::Match(Box::new(expr), arms),
    )(input)
}

fn parse_match_arm(input: &str) -> IResult<&str, (Expression, Statement)> {
    map(
        tuple((
            preceded(multispace0, identifier),
            opt(delimited(
                    preceded(multispace0, char::<&str, Error<&str>>(LEFT_PAREN)),
                    separated_list0(
                        delimited(multispace0, char::<&str, Error<&str>>(COMMA_CHAR), multispace0),
                        parse_pattern_argument,
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
        |(constructor, parameters, statement)| {
            let args = parameters.unwrap_or_default();
            (Expression::Constructor(constructor.to_string(), args.into_iter().map(Box::new).collect()), statement)
        },
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
                // Verifica padrão do primeiro braço: A
                match &arms[0].0 {
                    Expression::Constructor(name, args) => {
                        assert_eq!(name, "A");
                        assert!(args.is_empty());
                    }
                    _ => panic!("Esperado padrão Constructor no primeiro braço"),
                }
                // Verifica padrão do segundo braço: B
                match &arms[1].0 {
                    Expression::Constructor(name, args) => {
                        assert_eq!(name, "B");
                        assert!(args.is_empty());
                    }
                    _ => panic!("Esperado padrão Constructor no segundo braço"),
                }
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

                // Verifica padrão do primeiro braço: A(value)
                match &arms[0].0 {
                    Expression::Constructor(name, args) => {
                        assert_eq!(name, "A");
                        assert_eq!(args.len(), 1);
                        assert_eq!(*args[0], Expression::Var("value".to_string()));
                    }
                    _ => panic!("Esperado padrão Constructor no primeiro braço"),
                }

                // Verifica padrão do segundo braço: B(x, y)
                match &arms[1].0 {
                    Expression::Constructor(name, args) => {
                        assert_eq!(name, "B");
                        assert_eq!(args.len(), 2);
                        assert_eq!(*args[0], Expression::Var("x".to_string()));
                        assert_eq!(*args[1], Expression::Var("y".to_string()));
                    }
                    _ => panic!("Esperado padrão Constructor no segundo braço"),
                }
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
                // Verifica padrão do primeiro braço: A
                match &arms[0].0 {
                    Expression::Constructor(name, args) => {
                        assert_eq!(name, "A");
                        assert!(args.is_empty());
                    }
                    _ => panic!("Esperado padrão Constructor no primeiro braço"),
                }
                // Verifica padrão do segundo braço: B
                match &arms[1].0 {
                    Expression::Constructor(name, args) => {
                        assert_eq!(name, "B");
                        assert!(args.is_empty());
                    }
                    _ => panic!("Esperado padrão Constructor no segundo braço"),
                }
                // Verifica padrão do terceiro braço: C(value)
                match &arms[2].0 {
                    Expression::Constructor(name, args) => {
                        assert_eq!(name, "C");
                        assert_eq!(args.len(), 1);
                        assert_eq!(*args[0], Expression::Var("value".to_string()));
                    }
                    _ => panic!("Esperado padrão Constructor no terceiro braço"),
                }
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
        let input = "match   x   :   A   =>   {   :   x   =   1   ;   end   }   ,   B   =>   {   :   x   =   2   ;   end   }   end   match"; // You can end with "end match"
        let result: IResult<&str, Statement> = parse_match_statement(input);
        assert!(result.is_ok());
        
        let (rest, parsed) = result.unwrap();
        assert_eq!(rest, "");
        
        match parsed {
            Statement::Match(expr, arms) => {
                assert_eq!(*expr, Expression::Var("x".to_string()));
                assert_eq!(arms.len(), 2);
                // Verifica padrão do primeiro braço: A
                match &arms[0].0 {
                    Expression::Constructor(name, args) => {
                        assert_eq!(name, "A");
                        assert!(args.is_empty());
                    }
                    _ => panic!("Esperado padrão Constructor no primeiro braço"),
                }
                // Verifica padrão do segundo braço: B
                match &arms[1].0 {
                    Expression::Constructor(name, args) => {
                        assert_eq!(name, "B");
                        assert!(args.is_empty());
                    }
                    _ => panic!("Esperado padrão Constructor no segundo braço"),
                }
            }
            _ => panic!("Expected Match statement"),
        }
    }

    
    #[test]
    fn test_parse_match_statement_complex() {
        let input = r#"match x: Cons(1, y) => { : z = y; end }, Nil => { : z = 0; end } end"#; // or you can end with just "end"
        let result: IResult<&str, Statement> = parse_match_statement(input);
        assert!(result.is_ok());
        let (rest, stmt) = result.unwrap();
        assert_eq!(rest, "");
        match stmt {
            Statement::Match(expr, arms) => {
                assert_eq!(*expr, Expression::Var("x".to_string()));
                assert_eq!(arms.len(), 2);
                // Primeiro braço: Cons(1, y)
                match &arms[0].0 {
                    Expression::Constructor(name, args) => {
                        assert_eq!(name, "Cons");
                        assert_eq!(args.len(), 2);
                        assert_eq!(*args[0], Expression::CInt(1));
                        assert_eq!(*args[1], Expression::Var("y".to_string()));
                    }
                    _ => panic!("Esperado padrão Cons(1, y)"),
                }
                match &arms[0].1 {
                    Statement::Block(stmts) => {
                        assert_eq!(stmts.len(), 1);
                        match &stmts[0] {
                            Statement::Assignment(var, expr) => {
                                assert_eq!(var, "z");
                                assert_eq!(**expr, Expression::Var("y".to_string()));
                            }
                            _ => panic!("Esperado Assignment"),
                        }
                    }
                    _ => panic!("Esperado Block"),
                }
                // Segundo braço: Nil
                match &arms[1].0 {
                    Expression::Constructor(name, args) => {
                        assert_eq!(name, "Nil");
                        assert!(args.is_empty());
                    }
                    _ => panic!("Esperado padrão Nil"),
                }
                match &arms[1].1 {
                    Statement::Block(stmts) => {
                        assert_eq!(stmts.len(), 1);
                        match &stmts[0] {
                            Statement::Assignment(var, expr) => {
                                assert_eq!(var, "z");
                                assert_eq!(**expr, Expression::CInt(0));
                            }
                            _ => panic!("Esperado Assignment"),
                        }
                    }
                    _ => panic!("Esperado Block"),
                }
            }
            _ => panic!("Esperado Match statement"),
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
    #[test]
    fn test_parse_match_arm_simple() {
        let input = "A => { : x = 1; end }";
        let result: IResult<&str, (Expression, Statement)> = parse_match_arm(input);
        assert!(result.is_ok());
        let (rest, (pattern, stmt)) = result.unwrap();
        assert_eq!(rest, "");
        match pattern {
            Expression::Constructor(ref name, ref args) => {
                assert_eq!(name, "A");
                assert!(args.is_empty());
            }
            _ => panic!("Esperado padrão Constructor"),
        }
        match stmt {
            Statement::Block(ref stmts) => {
                assert_eq!(stmts.len(), 1);
                match &stmts[0] {
                    Statement::Assignment(var, expr) => {
                        assert_eq!(var, "x");
                        assert_eq!(**expr, Expression::CInt(1));
                    }
                    _ => panic!("Esperado Assignment"),
                }
            }
            _ => panic!("Esperado Block"),
        }
    }
    
    #[test]
    fn test_parse_match_arm_with_parameters() {
        let input = "B(42, y) => { : z = y; end }";
        let result: IResult<&str, (Expression, Statement)> = parse_match_arm(input);
        assert!(result.is_ok());
        let (rest, (pattern, stmt)) = result.unwrap();
        assert_eq!(rest, "");
        match pattern {
            Expression::Constructor(ref name, ref args) => {
                assert_eq!(name, "B");
                assert_eq!(args.len(), 2);
                assert_eq!(*args[0], Expression::CInt(42));
                assert_eq!(*args[1], Expression::Var("y".to_string()));
            }
            _ => panic!("Esperado padrão Constructor"),
        }
        match stmt {
            Statement::Block(ref stmts) => {
                assert_eq!(stmts.len(), 1);
                match &stmts[0] {
                    Statement::Assignment(var, expr) => {
                        assert_eq!(var, "z");
                        assert_eq!(**expr, Expression::Var("y".to_string()));
                    }
                    _ => panic!("Esperado Assignment"),
                }
            }
            _ => panic!("Esperado Block"),
        }
    }
}
