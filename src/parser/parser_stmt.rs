use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, multispace0, multispace1},
    combinator::{map, opt},
    error::Error,
    multi::separated_list0,
    sequence::{delimited, preceded, tuple},
    IResult,
    multi::many0,
    sequence::terminated,
};

use crate::ir::ast::{FormalArgument, Function, Statement};
use crate::parser::parser_common::{
    identifier, keyword, ASSERT_KEYWORD, COLON_CHAR, COMMA_CHAR, DEF_KEYWORD, ELSE_KEYWORD,
    END_KEYWORD, EQUALS_CHAR, FOR_KEYWORD, FUNCTION_ARROW, IF_KEYWORD, IN_KEYWORD, LEFT_PAREN,
    RIGHT_PAREN, SEMICOLON_CHAR, VAL_KEYWORD, VAR_KEYWORD, WHILE_KEYWORD,
};
use crate::parser::parser_expr::parse_expression;
use crate::parser::parser_type::parse_type;

use crate::ir::ast::Trait;

pub fn parse_statement(input: &str) -> IResult<&str, Statement> {
    alt((
        parse_trait_statement,
        parse_var_declaration_statement,
        parse_val_declaration_statement,
        parse_assignment_statement,
        parse_if_else_statement,
        parse_while_statement,
        parse_for_statement,
        parse_assert_statement,
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

fn parse_trait_statement(input: &str) -> IResult<&str, Statement> {
    map(
        tuple((
            terminated(keyword("trait"), multispace1),
            identifier,
            preceded(multispace0, char(COLON_CHAR)),
            many0(preceded(
                multispace0, // aceita espaço ou newline antes do método
                parse_trait_method_signature,
            )),
            preceded(multispace0, keyword("end")),
        )),
        |(_, name, _, methods, _)| {
            Statement::Trait(Trait {
                name: name.to_string(),
                methods,
            })
        },
    )(input)
}

fn parse_trait_method_signature(input: &str) -> IResult<&str, Function> {
    map(
        tuple((
            keyword("def"),
            preceded(multispace1, identifier),
            delimited(
                char(LEFT_PAREN),
                separated_list0(
                    tuple((multispace0, char(COMMA_CHAR), multispace0)),
                    parse_formal_argument,
                ),
                char(RIGHT_PAREN),
            ),
            preceded(multispace0, tag(FUNCTION_ARROW)),
            preceded(multispace0, parse_type),
            opt(char(SEMICOLON_CHAR)), // ← tolera o ;
        )),
        |(_, name, args, _, return_type, _)| Function {
            name: name.to_string(),
            kind: return_type,
            params: args,
            body: None,
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
fn test_parse_trait_statement() {
    let input = r#"
trait Printable:
    def print(self: String) -> Void;
    def to_string(self: String) -> String;
end
"#;

    let result = parse_trait_statement(input);
    dbg!(&result); // útil se ainda falhar

    match result {
        Ok((_, stmt)) => {
            match stmt {
                Statement::Trait(tr) => {
                    assert_eq!(tr.name, "Printable");
                    assert_eq!(tr.methods.len(), 2);
                    assert_eq!(tr.methods[0].name, "print");
                    assert_eq!(tr.methods[1].name, "to_string");
                }
                _ => panic!("Expected Trait statement"),
            }
        }
        Err(e) => {
            panic!("Erro ao fazer parsing de trait: {:?}", e);
        }
    }
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
}
