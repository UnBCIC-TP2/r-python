use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, multispace0},
    combinator::{not, peek, recognize},
    multi::many0,
    sequence::{delimited, terminated},
    IResult,
};

use crate::parser::keywords::KEYWORDS;

// Type name constants
pub const INT_TYPE: &str = "Int";
pub const REAL_TYPE: &str = "Real";
pub const BOOLEAN_TYPE: &str = "Boolean";
pub const STRING_TYPE: &str = "String";
pub const UNIT_TYPE: &str = "Unit";
pub const ANY_TYPE: &str = "Any";

// Special type constructor constants
pub const MAYBE_TYPE: &str = "Maybe";
pub const RESULT_TYPE: &str = "Result";

// Keyword constants
pub const DATA_KEYWORD: &str = "data";
pub const END_KEYWORD: &str = "end";

// Statement keyword constants
pub const IF_KEYWORD: &str = "if";
pub const ELSE_KEYWORD: &str = "else";
pub const WHILE_KEYWORD: &str = "while";
pub const FOR_KEYWORD: &str = "for";
pub const IN_KEYWORD: &str = "in";
pub const ASSERT_KEYWORD: &str = "assert";
pub const VAR_KEYWORD: &str = "var";
pub const VAL_KEYWORD: &str = "val";
pub const DEF_KEYWORD: &str = "def";
pub const MATCH_KEYWORD: &str = "match";

// Operator and symbol constants
pub const FUNCTION_ARROW: &str = "->";
pub const MATCH_ARM_ARROW: &str = "=>";
pub const PIPE_SYMBOL: &str = "|";
pub const COLON_SYMBOL: &str = ":";
pub const COMMA_SYMBOL: &str = ",";
pub const SEMICOLON_SYMBOL: &str = ";";

// Bracket and parentheses constants
pub const LEFT_BRACKET: char = '[';
pub const RIGHT_BRACKET: char = ']';
pub const LEFT_PAREN: char = '(';
pub const RIGHT_PAREN: char = ')';
pub const LEFT_BRACE: char = '{';
pub const RIGHT_BRACE: char = '}';

// Other character constants
pub const COMMA_CHAR: char = ',';
pub const COLON_CHAR: char = ':';
pub const PIPE_CHAR: char = '|';
pub const SEMICOLON_CHAR: char = ';';
pub const EQUALS_CHAR: char = '=';

/// Accepts any character except '"' and control characters (like \n, \t)
pub fn is_string_char(c: char) -> bool {
    c != '"' && !c.is_control()
}

pub fn separator<'a>(sep: &'static str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str> {
    delimited(multispace0, tag(sep), multispace0)
}

/// Parses a reserved keyword (e.g., "if") surrounded by optional spaces
/// Fails if followed by an identifier character
pub fn keyword<'a>(kw: &'static str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str> {
    terminated(
        delimited(multispace0, tag(kw), multispace0),
        not(peek(identifier_start_or_continue)),
    )
}

/// Parses a keyword that can be followed by expressions or identifiers
/// This is more flexible than the standard keyword parser
pub fn flexible_keyword<'a>(kw: &'static str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str> {
    delimited(multispace0, tag(kw), multispace0)
}

/// Parsers for identifiers.
pub fn identifier(input: &str) -> IResult<&str, &str> {
    let (input, _) = multispace0(input)?;

    let (input, first_char) = identifier_start(input)?;
    let (input, rest) = identifier_continue(input)?;

    let ident = format!("{}{}", first_char, rest);

    if KEYWORDS.contains(&ident.as_str()) {
        Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        )))
    } else {
        Ok((input, Box::leak(ident.into_boxed_str())))
    }
}

/// First character of an identifier: [a-zA-Z_]
fn identifier_start(input: &str) -> IResult<&str, &str> {
    alt((alpha1, tag("_")))(input)
}

/// Remaining characters: [a-zA-Z0-9_]*
fn identifier_continue(input: &str) -> IResult<&str, &str> {
    recognize(many0(identifier_start_or_continue))(input)
}

/// A single identifier character: alphanumeric or underscore
pub fn identifier_start_or_continue(input: &str) -> IResult<&str, &str> {
    recognize(alt((alpha1, tag("_"), nom::character::complete::digit1)))(input)
}
