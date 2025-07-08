use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    character::complete::{char, digit1, multispace0},
    combinator::{map, map_res, opt, value, verify},
    error::Error,
    multi::{fold_many0, separated_list0},
    sequence::{delimited, pair, preceded, tuple},
    IResult,
};

use std::str::FromStr;

use crate::{ir::ast::{Expression}};
use crate::parser::parser_common::{
    identifier,
    is_string_char,
    keyword,
    // Other character constants
    COMMA_CHAR,
    // Bracket and parentheses constants
    LEFT_BRACKET,
    LEFT_PAREN,
    RIGHT_BRACKET,
    RIGHT_PAREN,
    // MATCH_KEYWORD,
    // END_KEYWORD,
    // COLON_CHAR,
    // MATCH_ARM_ARROW,
    // LEFT_BRACE,
    // RIGHT_BRACE,
};

pub fn parse_expression(input: &str) -> IResult<&str, Expression> {
    parse_or(input)
}

fn parse_or(input: &str) -> IResult<&str, Expression> {
    let (input, init) = parse_and(input)?;
    fold_many0(
        preceded(keyword("or"), parse_and),
        move || init.clone(),
        |acc, val| Expression::Or(Box::new(acc), Box::new(val)),
    )(input)
}

fn parse_and(input: &str) -> IResult<&str, Expression> {
    let (input, init) = parse_not(input)?;
    fold_many0(
        preceded(keyword("and"), parse_not),
        move || init.clone(),
        |acc, val| Expression::And(Box::new(acc), Box::new(val)),
    )(input)
}

fn parse_not(input: &str) -> IResult<&str, Expression> {
    alt((
        map(preceded(keyword("not"), parse_not), |e| {
            Expression::Not(Box::new(e))
        }),
        parse_relational,
    ))(input)
}

fn parse_relational(input: &str) -> IResult<&str, Expression> {
    let (input, init) = parse_add_sub(input)?;
    fold_many0(
        pair(
            alt((
                operator("<="),
                operator("<"),
                operator(">="),
                operator(">"),
                operator("=="),
                operator("!="),
            )),
            parse_add_sub,
        ),
        move || init.clone(),
        |acc, (op, val)| match op {
            "<" => Expression::LT(Box::new(acc), Box::new(val)),
            "<=" => Expression::LTE(Box::new(acc), Box::new(val)),
            ">" => Expression::GT(Box::new(acc), Box::new(val)),
            ">=" => Expression::GTE(Box::new(acc), Box::new(val)),
            "==" => Expression::EQ(Box::new(acc), Box::new(val)),
            "!=" => Expression::NEQ(Box::new(acc), Box::new(val)),
            _ => unreachable!(),
        },
    )(input)
}

fn parse_add_sub(input: &str) -> IResult<&str, Expression> {
    let (input, init) = parse_term(input)?;
    fold_many0(
        pair(alt((operator("+"), operator("-"))), parse_term),
        move || init.clone(),
        |acc, (op, val)| match op {
            "+" => Expression::Add(Box::new(acc), Box::new(val)),
            "-" => Expression::Sub(Box::new(acc), Box::new(val)),
            _ => unreachable!(),
        },
    )(input)
}

fn parse_term(input: &str) -> IResult<&str, Expression> {
    let (input, init) = parse_factor(input)?;
    fold_many0(
        pair(alt((operator("*"), operator("/"))), parse_factor),
        move || init.clone(),
        |acc, (op, val)| match op {
            "*" => Expression::Mul(Box::new(acc), Box::new(val)),
            "/" => Expression::Div(Box::new(acc), Box::new(val)),
            _ => unreachable!(),
        },
    )(input)
}

fn parse_factor(input: &str) -> IResult<&str, Expression> {
    alt((
        parse_bool,
        parse_number,
        parse_string,
        parse_list,
        parse_function_call,
        parse_var,
        //parse_match_expression,
        delimited(
            char::<&str, Error<&str>>(LEFT_PAREN),
            parse_expression,
            char::<&str, Error<&str>>(RIGHT_PAREN),
        ),
    ))(input)
}

fn parse_bool(input: &str) -> IResult<&str, Expression> {
    alt((
        value(Expression::CTrue, keyword("True")),
        value(Expression::CFalse, keyword("False")),
    ))(input)
}

fn parse_number(input: &str) -> IResult<&str, Expression> {
    let float_parser = map_res(
        verify(
            tuple((
                opt(char::<&str, Error<&str>>('-')),
                digit1,
                char::<&str, Error<&str>>('.'),
                digit1,
            )),
            |(_, _, _, _)| true,
        ),
        |(sign, d1, _, d2)| {
            let s = match sign {
                Some(_) => format!("-{}.{}", d1, d2),
                None => format!("{}.{}", d1, d2),
            };
            f64::from_str(&s)
        },
    );

    let int_parser = map_res(
        tuple((opt(char::<&str, Error<&str>>('-')), digit1)),
        |(sign, digits)| {
            let s = match sign {
                Some(_) => format!("-{}", digits),
                None => digits.to_string(),
            };
            i32::from_str(&s)
        },
    );

    alt((
        map(float_parser, Expression::CReal),
        map(int_parser, Expression::CInt),
    ))(input)
}

fn parse_string(input: &str) -> IResult<&str, Expression> {
    map(
        delimited(
            multispace0,
            delimited(
                char::<&str, Error<&str>>('"'),
                map(take_while(is_string_char), |s: &str| s.to_string()),
                char::<&str, Error<&str>>('"'),
            ),
            multispace0,
        ),
        |s| Expression::CString(s),
    )(input)
}

fn parse_var(input: &str) -> IResult<&str, Expression> {
    map(identifier, |v| Expression::Var(v.into()))(input)
}

fn parse_function_call(input: &str) -> IResult<&str, Expression> {
    let (input, name) = identifier(input)?;
    let (input, args) = parse_actual_arguments(input)?;
    Ok((input, Expression::FuncCall(name.to_string(), args)))
}

pub fn parse_actual_arguments(input: &str) -> IResult<&str, Vec<Expression>> {
    map(
        tuple((
            multispace0,
            char::<&str, Error<&str>>(LEFT_PAREN),
            separated_list0(
                tuple((
                    multispace0,
                    char::<&str, Error<&str>>(COMMA_CHAR),
                    multispace0,
                )),
                parse_expression,
            ),
            multispace0,
            char::<&str, Error<&str>>(RIGHT_PAREN),
        )),
        |(_, _, args, _, _)| args,
    )(input)
}

fn parse_list(input: &str) -> IResult<&str, Expression> {
    let (input, _) = multispace0(input)?;
    let (input, _) = char(LEFT_BRACKET)(input)?;
    let (input, _) = multispace0(input)?;

    let (input, elements) = separated_list0(
        delimited(multispace0, char(COMMA_CHAR), multispace0),
        parse_expression,
    )(input)?;

    let (input, _) = multispace0(input)?;
    let (input, _) = char(RIGHT_BRACKET)(input)?;
    let (input, _) = multispace0(input)?;

    Ok((input, Expression::ListValue(elements)))
}

/// Parses an operator.
fn operator<'a>(op: &'static str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str> {
    delimited(multispace0, tag(op), multispace0)
}

/*fn parse_match_expression(input: &str) -> IResult<&str, Expression> {
    map(
        tuple((
            keyword(MATCH_KEYWORD),
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
            keyword(MATCH_KEYWORD),
        )),
        |(_, expr, _, _, arms, _, _)| Expression::Match(Box::new(expr), arms),
    )(input)
}

fn parse_match_arm(input: &str) -> IResult<&str, ((Name, Vec<Name>), Expression)> {
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
        |(constructor, parameters, expression)| ((constructor.to_string(), parameters.unwrap_or_default().into_iter().map(|p| p.to_string()).collect()), expression),
    )(input)
}
*/

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_expression_integer() {
        assert_eq!(
            parse_expression("123abc"),
            Ok(("abc", Expression::CInt(123)))
        );

        assert_eq!(
            parse_expression("-456 rest"),
            Ok((" rest", Expression::CInt(-456)))
        );
    }

    #[test]
    #[ignore]
    fn test_parse_expression_real() {
        assert_eq!(
            parse_expression("3.14xyz"),
            Ok(("xyz", Expression::CReal(3.14)))
        );

        assert_eq!(
            parse_expression("-0.001rest"),
            Ok(("rest", Expression::CReal(-0.001)))
        );
    }

    #[test]
    #[ignore]
    fn test_parse_expression_errors() {
        // Doesn't start with a number
        assert!(parse_expression("hello").is_err());

        // Not a valid number
        assert!(parse_expression("12.34.56").is_err());
    }

    #[test]
    fn test_keywords() {
        let cases = [
            ("if", "if"),
            ("else", "else"),
            ("while", "while"),
            ("and", "and"),
            ("or", "or"),
            ("not", "not"),
            ("for", "for"),
            ("def", "def"),
        ];

        for (input, expected) in cases {
            let result = keyword(expected)(input);
            assert!(result.is_ok(), "Failed to parse keyword '{}'", expected);
            let (rest, parsed) = result.unwrap();
            assert_eq!(parsed, expected);
            assert!(rest.is_empty() || rest.starts_with(' '));
        }
    }

    #[test]
    fn test_keyword_should_not_match_prefix_of_identifiers() {
        let mut parser = keyword("if");
        assert!(parser("iffy").is_err());

        let mut parser = keyword("def");
        assert!(parser("default").is_err());

        let mut parser = keyword("or");
        assert!(parser("origin").is_err());
    }

    #[test]
    fn test_parse_empty_list() {
        let input = "[]";
        let (rest, result) = parse_list(input).unwrap();
        assert_eq!(rest, "");
        assert!(matches!(result, Expression::ListValue(elements) if elements.is_empty()));
    }

    #[test]
    fn test_parse_integer_list() {
        let input = "[1, 2, 3]";
        let (rest, result) = parse_list(input).unwrap();
        assert_eq!(rest, "");
        if let Expression::ListValue(elements) = result {
            assert_eq!(elements.len(), 3);
            assert!(matches!(elements[0], Expression::CInt(1)));
            assert!(matches!(elements[1], Expression::CInt(2)));
            assert!(matches!(elements[2], Expression::CInt(3)));
        } else {
            panic!("Expected ListValue expression");
        }
    }

    #[test]
    fn test_parse_string_list() {
        let input = "[\"abc\", \"def\"]";
        let (rest, result) = parse_list(input).unwrap();
        assert_eq!(rest, "");
        if let Expression::ListValue(elements) = result {
            assert_eq!(elements.len(), 2);
            assert!(matches!(elements[0], Expression::CString(ref s) if s == "abc"));
            assert!(matches!(elements[1], Expression::CString(ref s) if s == "def"));
        } else {
            panic!("Expected ListValue expression");
        }
    }

    #[test]
    fn test_parse_list_with_spaces() {
        let input = "[ 1 , 2 , 3 ]";
        let (rest, result) = parse_list(input).unwrap();
        assert_eq!(rest, "");
        if let Expression::ListValue(elements) = result {
            assert_eq!(elements.len(), 3);
            assert!(matches!(elements[0], Expression::CInt(1)));
            assert!(matches!(elements[1], Expression::CInt(2)));
            assert!(matches!(elements[2], Expression::CInt(3)));
        } else {
            panic!("Expected ListValue expression");
        }
    }

    #[test]
    fn test_parse_list_with_outer_spaces() {
        let input = "    [1, 2, 3]    rest";
        let (rest, result) = parse_list(input).unwrap();
        assert_eq!(rest, "rest");
        if let Expression::ListValue(elements) = result {
            assert_eq!(elements.len(), 3);
            assert!(matches!(elements[0], Expression::CInt(1)));
            assert!(matches!(elements[1], Expression::CInt(2)));
            assert!(matches!(elements[2], Expression::CInt(3)));
        } else {
            panic!("Expected ListValue expression");
        }
    }

    #[test]
    fn test_parse_nested_list() {
        let input = "[[1, 2], [3, 4]]";
        let (rest, result) = parse_list(input).unwrap();
        assert_eq!(rest, "");
        if let Expression::ListValue(elements) = result {
            assert_eq!(elements.len(), 2);
            for element in elements {
                if let Expression::ListValue(inner) = element {
                    assert_eq!(inner.len(), 2);
                } else {
                    panic!("Expected inner ListValue expression");
                }
            }
        } else {
            panic!("Expected ListValue expression");
        }
    }
}
