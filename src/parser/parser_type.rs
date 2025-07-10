use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, multispace0},
    combinator::map,
    multi::{many1, separated_list0, separated_list1},
    sequence::{preceded, tuple},
    IResult,
};

use crate::ir::ast::{Type, Name};
use std::collections::HashMap;

use crate::parser::parser_common::{
    identifier, keyword, separator, ANY_TYPE, BOOLEAN_TYPE, COLON_CHAR, COMMA_CHAR, COMMA_SYMBOL,
    DATA_KEYWORD, END_KEYWORD, FUNCTION_ARROW, INT_TYPE, LEFT_BRACKET, LEFT_PAREN, MAYBE_TYPE,
    PIPE_CHAR, REAL_TYPE, RESULT_TYPE, RIGHT_BRACKET, RIGHT_PAREN, STRING_TYPE, UNIT_TYPE,
};

pub fn parse_type(input: &str) -> IResult<&str, Type> {
    alt((
        parse_basic_types,
        parse_list_type,
        parse_tuple_type,
        parse_maybe_type,
        parse_result_type,
        parse_function_type,
        parse_adt_type,
    ))(input)
}

fn parse_basic_types(input: &str) -> IResult<&str, Type> {
    map(
        alt((
            keyword(INT_TYPE),
            keyword(REAL_TYPE),
            keyword(BOOLEAN_TYPE),
            keyword(STRING_TYPE),
            keyword(UNIT_TYPE),
            keyword(ANY_TYPE),
        )),
        |t| match t {
            INT_TYPE => Type::TInteger,
            REAL_TYPE => Type::TReal,
            BOOLEAN_TYPE => Type::TBool,
            STRING_TYPE => Type::TString,
            UNIT_TYPE => Type::TVoid,
            ANY_TYPE => Type::TAny,
            _ => unreachable!(),
        },
    )(input)
}

fn parse_list_type(input: &str) -> IResult<&str, Type> {
    map(
        tuple((
            preceded(multispace0, char(LEFT_BRACKET)),
            preceded(multispace0, parse_type),
            preceded(multispace0, char(RIGHT_BRACKET)),
        )),
        |(_, t, _)| Type::TList(Box::new(t)),
    )(input)
}

fn parse_tuple_type(input: &str) -> IResult<&str, Type> {
    map(
        tuple((
            preceded(multispace0, char(LEFT_PAREN)),
            preceded(
                multispace0,
                separated_list1(separator(COMMA_SYMBOL), parse_type),
            ),
            preceded(multispace0, char(RIGHT_PAREN)),
        )),
        |(_, ts, _)| Type::TTuple(ts),
    )(input)
}

fn parse_maybe_type(input: &str) -> IResult<&str, Type> {
    map(
        tuple((
            preceded(multispace0, keyword(MAYBE_TYPE)),
            preceded(multispace0, char(LEFT_BRACKET)),
            preceded(multispace0, parse_type),
            preceded(multispace0, char(RIGHT_BRACKET)),
        )),
        |(_, _, t, _)| Type::TMaybe(Box::new(t)),
    )(input)
}

fn parse_result_type(input: &str) -> IResult<&str, Type> {
    map(
        tuple((
            preceded(multispace0, keyword(RESULT_TYPE)),
            preceded(multispace0, char(LEFT_BRACKET)),
            preceded(multispace0, parse_type),
            preceded(multispace0, char(COMMA_CHAR)),
            preceded(multispace0, parse_type),
            preceded(multispace0, char(RIGHT_BRACKET)),
        )),
        |(_, _, t_ok, _, t_err, _)| Type::TResult(Box::new(t_ok), Box::new(t_err)),
    )(input)
}

fn parse_function_type(input: &str) -> IResult<&str, Type> {
    map(
        tuple((
            preceded(multispace0, char(LEFT_PAREN)),
            preceded(
                multispace0,
                separated_list0(separator(COMMA_SYMBOL), parse_type),
            ),
            preceded(multispace0, char(RIGHT_PAREN)),
            preceded(multispace0, tag(FUNCTION_ARROW)),
            preceded(multispace0, parse_type),
        )),
        |(_, t_args, _, _, t_ret)| Type::TFunction(Box::new(Some(t_ret)), t_args),
    )(input)
}

fn parse_adt_type(input: &str) -> IResult<&str, Type> {
    map(
        tuple((
            keyword(DATA_KEYWORD),
            preceded(multispace0, identifier),
            preceded(multispace0, char(COLON_CHAR)),
            many1(parse_adt_cons),
            preceded(multispace0, keyword(END_KEYWORD)),
        )),
        |(_, name, _, cons, _)| {
            let cons_map: HashMap<Name, Vec<Type>> = cons.into_iter().collect();
            Type::TAlgebraicData(name.to_string(), cons_map)
        },
    )(input)
}

fn parse_adt_cons(input: &str) -> IResult<&str, (Name, Vec<Type>)> {
    map(
        tuple((
            preceded(multispace0, char(PIPE_CHAR)),
            preceded(multispace0, identifier),
            separated_list0(multispace0, parse_type),
        )),
        |(_, name, types)| (name.to_string(), types),
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_basic_types() {
        assert_eq!(parse_basic_types("Int"), Ok(("", Type::TInteger)));
        assert_eq!(parse_basic_types("Boolean"), Ok(("", Type::TBool)));
    }

    #[test]
    fn test_parse_list_type() {
        assert_eq!(
            parse_list_type("[Int]"),
            Ok(("", Type::TList(Box::new(Type::TInteger))))
        );
    }

    #[test]
    fn test_parse_tuple_type() {
        assert_eq!(
            parse_tuple_type("(Int, Real)"),
            Ok(("", Type::TTuple(vec![Type::TInteger, Type::TReal])))
        );
    }

    #[test]
    fn test_parse_maybe_type() {
        assert_eq!(
            parse_maybe_type("Maybe [Boolean]"),
            Ok(("", Type::TMaybe(Box::new(Type::TBool))))
        );
    }

    #[test]
    fn test_parse_result_type() {
        assert_eq!(
            parse_result_type("Result [Int, String]"),
            Ok((
                "",
                Type::TResult(Box::new(Type::TInteger), Box::new(Type::TString))
            ))
        );
    }

    #[test]
    fn test_parse_function_type() {
        assert_eq!(
            parse_function_type("(Int, Boolean) -> String"),
            Ok((
                "",
                Type::TFunction(
                    Box::new(Some(Type::TString)),
                    vec![Type::TInteger, Type::TBool]
                )
            ))
        );
    }

    #[test]
    #[ignore]
    fn test_parse_adt_type() {
        let input = "data Maybe:\n  | Just Int\n  | Nothing\nend";
        let mut expected_constructors = HashMap::new();
        expected_constructors.insert("Just".to_string(), vec![Type::TInteger]);
        expected_constructors.insert("Nothing".to_string(), vec![]);
        let expected = Type::TAlgebraicData("Maybe".to_string(), expected_constructors);
        assert_eq!(parse_adt_type(input), Ok(("", expected)));
    }
}
