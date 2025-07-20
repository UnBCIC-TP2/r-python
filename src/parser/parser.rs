use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::{char, digit1, line_ending, space0, space1},
    combinator::{map, map_res, opt},
    multi::{many0, many1, separated_list0, separated_list1},
    sequence::{delimited, preceded, tuple},
    IResult,
};{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{char, digit1, line_ending, space0, space1},
    combinator::{map, map_res, opt},
    multi::{many0, many1, separated_list0, separated_list1},
    sequence::{delimited, preceded, tuple},
    IResult,
};

use crate::ir::ast::{Expression, Name, Statement};

// Parse identifier
fn identifier(input: &str) -> IResult<&str, Name> {
    let (input, id) = take_while1(|c: char| c.is_alphanumeric() || c == '_')(input)?;
    Ok((input, id.to_string()))
}

// Parse integer literals
fn integer(input: &str) -> IResult<&str, Expression> {
    map_res(digit1, |s: &str| s.parse::<i32>().map(Expression::CInt))(input)
}


// Parse string literals
fn string_literal(input: &str) -> IResult<&str, Expression> {
    let (input, _) = char('"')(input)?;
    let (input, content) = take_while1(|c| c != '"')(input)?;
    let (input, _) = char('"')(input)?;
    Ok((input, Expression::CString(content.to_string())))
}

// Parse function calls
fn function_call(input: &str) -> IResult<&str, Expression> {
    let (input, name) = identifier(input)?;
    let (input, _) = char('(')(input)?;
    let (input, _) = space0(input)?;
    let (input, args) = separated_list0(
        delimited(space0, char(','), space0),
        expression
    )(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = char(')')(input)?;
    Ok((input, Expression::FunctionCall(name, args)))

// NOVO: Parser para string literals (entre aspas duplas)
fn string_literal(input: &str) -> IResult<&str, Expression> {
    map(
        delimited(
            char('"'),
            many0(alt((
                take_while1(|c: char| c != '"' && c != '\\'), // Caracteres normais
                tag("\\\""), // Aspas duplas escapadas
                tag("\\\\"), // Barra invertida escapada
            ))),
            char('"'),
        ),
        |chars: Vec<&str>| Expression::CString(chars.join("")),
    )(input)

}

//term parser for arithmetic
fn term(input: &str) -> IResult<&str, Expression> {
    alt((
        delimited(
            tuple((char('('), space0)),
            arithmetic_expression,
            tuple((space0, char(')'))),
        ),
        function_call,
        string_literal,
        integer,
        string_literal, // NOVO: Adicionado para reconhecer strings literais
        map(identifier, |name| Expression::Var(name)),
    ))(input)
}

//expression parser to include if statements
pub fn statement(input: &str) -> IResult<&str, Statement> {
    let (input, _) = space0(input)?;
    alt((if_statement, assignment, declaration, expression_statement))(input)
}

// Parse basic expressions
fn expression(input: &str) -> IResult<&str, Expression> {
    alt((
        function_call, // Mantenha antes de `comparison_expression` e `map(identifier, ...)`
        comparison_expression,
        arithmetic_expression,
        function_call,
        string_literal,
        integer,
        string_literal, // NOVO: Adicionado para reconhecer strings literais
        map(identifier, |name| Expression::Var(name)),
    ))(input)
}

// Parse arithmetic operators
fn operator(input: &str) -> IResult<&str, &str> {
    alt((tag("+"), tag("-"), tag("*"), tag("/")))(input)
}

// Add comparison operator parsing
fn comparison_operator(input: &str) -> IResult<&str, &str> {
    alt((
        tag("=="),
        tag("!="),
        tag(">="),
        tag("<="),
        tag(">"),
        tag("<"),
    ))(input)
}

// Update expression to handle comparisons
fn comparison_expression(input: &str) -> IResult<&str, Expression> {
    let (input, left) = term(input)?;
    let (input, _) = space0(input)?;
    let (input, op) = comparison_operator(input)?;
    let (input, _) = space0(input)?;
    let (input, right) = term(input)?;

    Ok((
        input,
        match op {
            ">" => Expression::GT(Box::new(left), Box::new(right)),
            "<" => Expression::LT(Box::new(left), Box::new(right)),
            ">=" => Expression::GTE(Box::new(left), Box::new(right)),
            "<=" => Expression::LTE(Box::new(left), Box::new(right)),
            "==" => Expression::EQ(Box::new(left), Box::new(right)),
            _ => unreachable!(),
        },
    ))
}

// Parse expressions with operator precedence
fn arithmetic_expression(input: &str) -> IResult<&str, Expression> {
    let (input, first) = term(input)?;
    let (input, operations) = many0(tuple((delimited(space0, operator, space0), term)))(input)?;

    Ok((
        input,
        operations
            .into_iter()
            .fold(first, |acc, (op, val)| match op {
                "+" => Expression::Add(Box::new(acc), Box::new(val)),
                "-" => Expression::Sub(Box::new(acc), Box::new(val)),
                "*" => Expression::Mul(Box::new(acc), Box::new(val)),
                "/" => Expression::Div(Box::new(acc), Box::new(val)),
                _ => unreachable!(),
            }),
    ))
}
//indented block parser
fn indented_block(input: &str) -> IResult<&str, Vec<Statement>> {
    let (input, _) = line_ending(input)?;
    let (input, statements) = separated_list1(
        line_ending,
        preceded(
            space1, // Require at least one space for indentation
            statement,
        ),
    )(input)?;
    Ok((input, statements))
}

fn if_statement(input: &str) -> IResult<&str, Statement> {
    let (input, _) = tag("if")(input)?;
    let (input, _) = space1(input)?;
    let (input, condition) = comparison_expression(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = char(':')(input)?;
    let (input, then_block) = indented_block(input)?;

    let (input, else_block) = opt(preceded(
        tuple((line_ending, space0, tag("else"), char(':'))),
        indented_block,
    ))(input)?;

    Ok((
        input,
        Statement::IfThenElse(
            Box::new(condition),
            Box::new(Statement::Block(then_block)),
            else_block.map(|stmts| Box::new(Statement::Block(stmts))),
        ),
    ))
}
fn declaration(input: &str) -> IResult<&str, Statement> {
    let (input, keyword) = alt((tag("var"), tag("val")))(input)?;
    let (input, _) = space1(input)?;
    let (input, name) = identifier(input)?;

    Ok((
        input,
        match keyword {
            "var" => Statement::VarDeclaration(name),
            "val" => Statement::ValDeclaration(name),
            _ => unreachable!(),
        },
    ))
}

// Parse assignment statements
fn assignment(input: &str) -> IResult<&str, Statement> {
    let (input, _) = space0(input)?;
    let (input, name) = identifier(input)?;
    let (input, _) = delimited(space0, char('='), space0)(input)?;
    let (input, expr) = expression(input)?;
    let (input, _) = space0(input)?;

    Ok((
        input,
        Statement::Assignment(
            name,
            Box::new(expr),
        ),
    ))
}

// Parser para argumentos de função
fn function_arguments(input: &str) -> IResult<&str, Vec<Expression>> {
    delimited(
        char('('),
        separated_list1(tuple((space0, char(','), space0)), expression), // Usa o parser 'expression'
        char(')'),
    )(input)
}

// Parser para chamadas de função
fn function_call(input: &str) -> IResult<&str, Expression> {
    map(
        tuple((identifier, space0, function_arguments)),
        |(name, _, args)| Expression::Call(name, args),
    )(input)
}

// Parser para statements que são apenas expressões (como chamadas de função)
fn expression_statement(input: &str) -> IResult<&str, Statement> {
    map(expression, Statement::ExpressionStmt)(input)
}

// Parse multiple statements
pub fn parse_statements(input: &str) -> IResult<&str, Vec<Statement>> {
    let (input, _) = space0(input)?; // Handle initial whitespace
    let (input, statements) = separated_list1(
        many1(line_ending), // Require at least one newline between statements
        statement,          // Use statement directly instead of limited alternatives
    )(input)?;
    let (input, _) = space0(input)?; // Handle trailing whitespace
    Ok((input, statements))
}

// Main parse function
pub fn parse(input: &str) -> IResult<&str, Vec<Statement>> {
    let (input, statements) = parse_statements(input)?;
    let (input, _) = many0(line_ending)(input)?; // Consume trailing newlines
    let (input, _) = space0(input)?; // Consume trailing whitespace
    Ok((input, statements))
}

pub fn parse_sequence_statement(input: &str) -> IResult<&str, Statement> {
    let (rest1, s1) = statement(input)?;
    let (rest2, _) = many1(line_ending)(rest1)?;
    let (rest3, s2) = statement(rest2)?;
    Ok((rest3, Statement::Sequence(Box::new(s1), Box::new(s2))))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::ast::{Expression, Statement};

    #[test]
    fn test_simple_assignment() {
        let input = "x = 42";
        let (rest, stmt) = assignment(input).unwrap();
        assert_eq!(rest, "");
        match stmt {
            Statement::Assignment(name, expr) => {
                assert_eq!(name, "x");
                match *expr {
                    Expression::CInt(val) => assert_eq!(val, 42),
                    _ => panic!("Expected CInt"),
                }
            }
            _ => panic!("Expected Assignment"),
        }
    }

    #[test]
    fn test_complete_program() {
        let input = "x = 10\nif x > 5:\n    y = 1\nelse:\n    y = 2";
        let (rest, stmts) = parse(input).unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 2);
    }

    #[test]
    fn test_complex_expression() {
        let input = "x = (2 * 3) + (10 - 4)";
        let (rest, stmts) = parse(input).unwrap();
        assert_eq!(rest, "");

        match &stmts[0] {
            Statement::Assignment(name, expr) => {
                assert_eq!(name, "x");
                match **expr {
                    Expression::Add(_, _) => (),
                    _ => panic!("Expected Add expression"),
                }
            }
            _ => panic!("Expected Assignment"),
        }
    }

    #[test]
    fn test_multiline_with_if() {
        let input = "x = 10\nif x > 5:\n    y = 1\nelse:\n    y = 2";
        let (rest, stmts) = parse(input).unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 2);

        match &stmts[0] {
            Statement::Assignment(name, expr) => {
                assert_eq!(name, "x");
                assert!(matches!(**expr, Expression::CInt(10)));
            }
            _ => panic!("Expected Assignment"),
        }

        match &stmts[1] {
            Statement::IfThenElse(condition, then_block, else_block) => {
                assert!(matches!(**condition, Expression::GT(_, _)));

                match **then_block {
                    Statement::Block(ref stmts) => {
                        assert_eq!(stmts.len(), 1);
                        match &stmts[0] {
                            Statement::Assignment(name, expr) => {
                                assert_eq!(name, "y");
                                assert!(matches!(**expr, Expression::CInt(1)));
                            }
                            _ => panic!("Expected Assignment in then block"),
                        }
                    }
                    _ => panic!("Expected Block"),
                }

                match else_block {
                    Some(else_stmt) => match **else_stmt {
                        Statement::Block(ref stmts) => {
                            assert_eq!(stmts.len(), 1);
                            match &stmts[0] {
                                Statement::Assignment(name, expr) => {
                                    assert_eq!(name, "y");
                                    assert!(matches!(**expr, Expression::CInt(2)));
                                }
                                _ => panic!("Expected Assignment in else block"),
                            }
                        }
                        _ => panic!("Expected Block"),
                    },
                    None => panic!("Expected Some else block"),
                }
            }
            _ => panic!("Expected IfThenElse"),
        }
    }

    #[test]
    fn test_if_else_block() {
        let input = "if x > 0:\n    y = 1\nelse:\n    y = 2";
        let (rest, stmt) = if_statement(input).unwrap();
        assert_eq!(rest, "");

        match stmt {
            Statement::IfThenElse(condition, then_block, else_block) => {
                assert!(matches!(*condition, Expression::GT(_, _)));

                match *then_block {
                    Statement::Block(ref stmts) => {
                        assert_eq!(stmts.len(), 1);
                        match &stmts[0] {
                            Statement::Assignment(name, expr) => {
                                assert_eq!(name, "y");
                                assert!(matches!(**expr, Expression::CInt(1)));
                            }
                            _ => panic!("Expected Assignment in then block"),
                        }
                    }
                    _ => panic!("Expected Block"),
                }

                match else_block {
                    Some(else_stmt) => match *else_stmt {
                        Statement::Block(ref stmts) => {
                            assert_eq!(stmts.len(), 1);
                            match &stmts[0] {
                                Statement::Assignment(name, expr) => {
                                    assert_eq!(name, "y");
                                    assert!(matches!(**expr, Expression::CInt(2)));
                                }
                                _ => panic!("Expected Assignment in else block"),
                            }
                        }
                        _ => panic!("Expected Block"),
                    },
                    None => panic!("Expected Some else block"),
                }
            }
            _ => panic!("Expected IfThenElse"),
        }
    }

    #[test]
    fn test_if_else_statement() {
        let input = "if x > 0:\n    y = 1\nelse:\n    y = 2";
        let (rest, stmt) = if_statement(input).unwrap();
        assert_eq!(rest, "");

        match stmt {
            Statement::IfThenElse(condition, then_block, else_block) => {
                assert!(matches!(
                    *condition,
                    Expression::GT(_box_ref @ _, _box_ref2 @ _)
                ));

                match *then_block {
                    Statement::Block(ref stmts) => {
                        assert_eq!(stmts.len(), 1);
                        match &stmts[0] {
                            Statement::Assignment(name, expr) => {
                                assert_eq!(name, "y");
                                assert!(matches!(**expr, Expression::CInt(1)));
                            }
                            _ => panic!("Expected Assignment"),
                        }
                    }
                    _ => panic!("Expected Block"),
                }

                match else_block {
                    Some(else_stmt) => match *else_stmt {
                        Statement::Block(ref stmts) => {
                            assert_eq!(stmts.len(), 1);
                            match &stmts[0] {
                                Statement::Assignment(name, expr) => {
                                    assert_eq!(name, "y");
                                    assert!(matches!(**expr, Expression::CInt(2)));
                                }
                                _ => panic!("Expected Assignment"),
                            }
                        }
                        _ => panic!("Expected Block"),
                    },
                    None => panic!("Expected Some else block"),
                }
            }
            _ => panic!("Expected IfThenElse"),
        }
    }

    #[test]
    fn test_multiline_parse() {
        let input = "x = 42\ny = 10";
        let (rest, stmts) = parse(input).unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 2);

        match &stmts[0] {
            Statement::Assignment(name, expr) => {
                assert_eq!(&**name, "x");
                match **expr {
                    Expression::CInt(42) => (),
                    _ => panic!("Expected CInt(42)"),
                }
            }
            _ => panic!("Expected Assignment"),
        }

        match &stmts[1] {
            Statement::Assignment(name, expr) => {
                assert_eq!(&**name, "y");
                match **expr {
                    Expression::CInt(10) => (),
                    _ => panic!("Expected CInt(10)"),
                }
            }
            _ => panic!("Expected Assignment"),
        }
    }

    #[test]
    fn test_whitespace_handling() {
        let input = "   x    =    42   \n   y   =   10   ";
        let (rest, stmts) = parse(input).unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 2);
    }

    // --- NOVOS TESTES PARA CHAMADAS DE FUNÇÃO E STRINGS LITERAIS ---

    #[test]
    fn test_print_call_no_args() {
        let input = "print()";
        let (rest, stmt) = expression_statement(input).unwrap();
        assert_eq!(rest, "");
        match stmt {
            Statement::ExpressionStmt(expr) => match expr {
                Expression::Call(name, args) => {
                    assert_eq!(name, "print");
                    assert!(args.is_empty());
                }
                _ => panic!("Expected Expression::Call"),
            },
            _ => panic!("Expected Statement::ExpressionStmt"),
        }
    }

    #[test]
    fn test_print_call_with_string_literal() {
        let input = "print(\"hello\")"; // Agora este teste deve funcionar
        let (rest, stmt) = expression_statement(input).unwrap();
        assert_eq!(rest, "");
        match stmt {
            Statement::ExpressionStmt(expr) => match expr {
                Expression::Call(name, args) => {
                    assert_eq!(name, "print");
                    assert_eq!(args.len(), 1);
                    assert!(matches!(args[0], Expression::CString(s) if s == "hello"));
                }
                _ => panic!("Expected Expression::Call"),
            },
            _ => panic!("Expected Statement::ExpressionStmt"),
        }
    }

    #[test]
    fn test_print_call_with_variable() {
        let input = "print(my_var)";
        let (rest, stmt) = expression_statement(input).unwrap();
        assert_eq!(rest, "");
        match stmt {
            Statement::ExpressionStmt(expr) => match expr {
                Expression::Call(name, args) => {
                    assert_eq!(name, "print");
                    assert_eq!(args.len(), 1);
                    assert!(matches!(args[0], Expression::Var(v) if v == "my_var"));
                }
                _ => panic!("Expected Expression::Call"),
            },
            _ => panic!("Expected Statement::ExpressionStmt"),
        }
    }

    #[test]
    fn test_input_call_no_args() {
        let input = "input()";
        let (rest, stmt) = expression_statement(input).unwrap();
        assert_eq!(rest, "");
        match stmt {
            Statement::ExpressionStmt(expr) => match expr {
                Expression::Call(name, args) => {
                    assert_eq!(name, "input");
                    assert!(args.is_empty());
                }
                _ => panic!("Expected Expression::Call"),
            },
            _ => panic!("Expected Statement::ExpressionStmt"),
        }
    }

    #[test]
    fn test_input_call_with_prompt() {
        let input = "input(\"Enter your name: \")"; // Agora este teste deve funcionar
        let (rest, stmt) = expression_statement(input).unwrap();
        assert_eq!(rest, "");
        match stmt {
            Statement::ExpressionStmt(expr) => match expr {
                Expression::Call(name, args) => {
                    assert_eq!(name, "input");
                    assert_eq!(args.len(), 1);
                    assert!(matches!(args[0], Expression::CString(s) if s == "Enter your name: "));
                }
                _ => panic!("Expected Expression::Call"),
            },
            _ => panic!("Expected Statement::ExpressionStmt"),
        }
    }

    #[test]
    fn test_multiple_args_print_call() {
        let input = "print(1, \"hello\", x)"; // Teste com string literal
        let (rest, stmt) = expression_statement(input).unwrap();
        assert_eq!(rest, "");
        match stmt {
            Statement::ExpressionStmt(expr) => match expr {
                Expression::Call(name, args) => {
                    assert_eq!(name, "print");
                    assert_eq!(args.len(), 3);
                    assert!(matches!(args[0], Expression::CInt(1)));
                    assert!(matches!(args[1], Expression::CString(s) if s == "hello"));
                    assert!(matches!(args[2], Expression::Var(v) if v == "x"));
                }
                _ => panic!("Expected Expression::Call"),
            },
            _ => panic!("Expected Statement::ExpressionStmt"),
        }
    }

    #[test]
    fn test_call_as_part_of_assignment() {
        let input = "result = input()";
        let (rest, stmt) = assignment(input).unwrap();
        assert_eq!(rest, "");
        match stmt {
            Statement::Assignment(name, expr_box) => {
                assert_eq!(name, "result");
                match *expr_box {
                    Expression::Call(call_name, call_args) => {
                        assert_eq!(call_name, "input");
                        assert!(call_args.is_empty());
                    }
                    _ => panic!("Expected Expression::Call for assignment value"),
                }
            }
            _ => panic!("Expected Assignment"),
        }
    }

    #[test]
    fn test_string_literal_parsing() {
        let input = "\"hello world\"";
        let (rest, expr) = string_literal(input).unwrap();
        assert_eq!(rest, "");
        assert_eq!(expr, Expression::CString(String::from("hello world")));
    }

    #[test]
    fn test_string_literal_with_escaped_quote() {
        let input = "\"hello \\\"world\\\"\"";
        let (rest, expr) = string_literal(input).unwrap();
        assert_eq!(rest, "");
        assert_eq!(expr, Expression::CString(String::from("hello \"world\"")));
    }

    #[test]
    fn test_string_literal_with_escaped_backslash() {
        let input = "\"path\\\\to\\\\file\"";
        let (rest, expr) = string_literal(input).unwrap();
        assert_eq!(rest, "");
        assert_eq!(expr, Expression::CString(String::from("path\\to\\file")));
    }
}
