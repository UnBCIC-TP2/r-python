use crate::ir::ast::{Expression, Name, Statement};
use nom::character::complete::multispace0;
use nom::multi::separated_list0;
use nom::sequence::separated_pair;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{char, digit1, line_ending, space0, space1},
    combinator::{map, map_res, opt},
    multi::{many0, many1, separated_list1},
    sequence::{delimited, preceded, tuple},
    IResult,
};

// Parse identifier
fn identifier(input: &str) -> IResult<&str, Name> {
    let (input, id) = take_while1(|c: char| c.is_alphanumeric() || c == '_')(input)?;
    Ok((input, id.to_string()))
}

// Parse integer literals
fn integer(input: &str) -> IResult<&str, Expression> {
    map_res(digit1, |s: &str| s.parse::<i32>().map(Expression::CInt))(input)
}

//term parser for arithmetic
fn term(input: &str) -> IResult<&str, Expression> {
    alt((
        delimited(
            tuple((char('('), space0)),
            arithmetic_expression,
            tuple((space0, char(')'))),
        ),
        integer,
        map(identifier, |name| Expression::Var(name)),
    ))(input)
}

//expression parser to include if statements
fn statement(input: &str) -> IResult<&str, Statement> {
    let (input, _) = space0(input)?;
    alt((if_statement, var_assignment, dict_assignment, declaration))(input)
}

// Parse basic expressions
fn expression(input: &str) -> IResult<&str, Expression> {
    alt((
        comparison_expression,
        arithmetic_expression,
        dictionary_expression,
        dictionary_access_expression,
        dict_merge_expression,
        in_expression,
        not_in_expression,
        integer,
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

/// Parses a dictionary expression.
/// Example: `{key1: value1, key2: value2}`
fn dictionary_expression(input: &str) -> IResult<&str, Expression> {
    let (input, _) = delimited(space0, char('{'), multispace0)(input)?;

    let (input, key_value_pairs) = separated_list0(
        delimited(space0, char(','), multispace0),
        separated_pair(identifier, delimited(space0, char(':'), space0), expression),
    )(input)?;

    let (input, _) = preceded(multispace0, char('}'))(input)?;

    Ok((
        input,
        Expression::Dict(
            key_value_pairs
                .iter()
                .map(|(key, value)| (key.clone(), Box::new(value.clone())))
                .collect(),
        ),
    ))
}

/// Parses a dictionary access expression.
/// Example: `dict.key`
fn dictionary_access_expression(input: &str) -> IResult<&str, Expression> {
    let (input, (dict_name, key_name)) = separated_pair(identifier, char('.'), identifier)(input)?;

    Ok((input, Expression::DictAccess(dict_name, key_name)))
}

/// Parses a merge expression that creates a new updated dictionary.
/// Example: `dict_name with { key1 = value1, key2 = value2 }`
fn dict_merge_expression(input: &str) -> IResult<&str, Expression> {
    let (input, (dict_name, new_dict)) = separated_pair(
        identifier,
        tuple((space0, tag("with"), space0)),
        dictionary_expression,
    )(input)?;

    let new_entries = match new_dict {
        Expression::Dict(entries) => entries,
        _ => unreachable!(),
    };

    Ok((input, Expression::DictMerge(dict_name, new_entries)))
}

/// Parses a `in` expression.
/// Example: `key in dict`
fn in_expression(input: &str) -> IResult<&str, Expression> {
    let (input, (item_name, collection_name)) =
        separated_pair(identifier, tuple((space0, tag("in"), space0)), identifier)(input)?;

    Ok((input, Expression::In(item_name, collection_name)))
}

/// Parses a `not in` expression.
/// Example: `key not in dict`
fn not_in_expression(input: &str) -> IResult<&str, Expression> {
    let (input, (item_name, collection_name)) = separated_pair(
        identifier,
        tuple((space0, tag("not"), space0, tag("in"), space0)),
        identifier,
    )(input)?;

    Ok((input, Expression::NotIn(item_name, collection_name)))
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
            else_block.map(|stmts| Box::new(Statement::Block(stmts))), // Changed this line
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
            "var" => Statement::VarDeclaration(name), // No Box needed
            "val" => Statement::ValDeclaration(name), // No Box needed
            _ => unreachable!(),
        },
    ))
}

// Parse assignment statements
fn var_assignment(input: &str) -> IResult<&str, Statement> {
    let (input, _) = space0(input)?;
    let (input, name) = identifier(input)?;
    let (input, _) = delimited(space0, char('='), space0)(input)?;
    let (input, expr) = expression(input)?;
    let (input, _) = space0(input)?;

    Ok((
        input,
        Statement::VarAssignment(
            name, // No longer need to Box the name
            Box::new(expr),
        ),
    ))
}

/// Parses a dictionary assignment statement.
/// Example: `dict.key = exp`
fn dict_assignment(input: &str) -> IResult<&str, Statement> {
    let (input, (dict_name, key_name)) = separated_pair(identifier, char('.'), identifier)(input)?;
    let (input, _) = delimited(space0, char('='), space0)(input)?;
    let (input, expr) = expression(input)?;

    Ok((
        input,
        Statement::DictAssigment(dict_name, key_name, Box::new(expr)),
    ))
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

#[cfg(test)]
mod tests {
    use super::*; // Import everything from parent module
    use crate::ir::ast::{Expression, Statement}; // Import AST types
    #[test]
    fn test_simple_assignment() {
        let input = "x = 42";
        let (rest, stmt) = var_assignment(input).unwrap();
        assert_eq!(rest, "");
        match stmt {
            Statement::VarAssignment(name, expr) => {
                assert_eq!(name, "x"); // Direct string comparison
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
        assert_eq!(stmts.len(), 2); // Assignment and IfThenElse
    }

    #[test]
    fn test_complex_expression() {
        let input = "x = (2 * 3) + (10 - 4)";
        let (rest, stmts) = parse(input).unwrap();
        assert_eq!(rest, "");

        match &stmts[0] {
            Statement::VarAssignment(name, expr) => {
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
        assert_eq!(stmts.len(), 2); // Should have assignment and if-statement

        // Verify first statement is assignment
        match &stmts[0] {
            Statement::VarAssignment(name, expr) => {
                assert_eq!(name, "x");
                assert!(matches!(**expr, Expression::CInt(10)));
            }
            _ => panic!("Expected Assignment"),
        }

        // Verify second statement is if-else
        match &stmts[1] {
            Statement::IfThenElse(condition, then_block, else_block) => {
                // Check condition - using GT instead of Comparison
                assert!(matches!(**condition, Expression::GT(_, _)));

                // Check then block
                match **then_block {
                    Statement::Block(ref stmts) => {
                        assert_eq!(stmts.len(), 1);
                        match &stmts[0] {
                            Statement::VarAssignment(name, expr) => {
                                assert_eq!(name, "y");
                                assert!(matches!(**expr, Expression::CInt(1)));
                            }
                            _ => panic!("Expected Assignment in then block"),
                        }
                    }
                    _ => panic!("Expected Block"),
                }

                // Check else block
                match else_block {
                    Some(else_stmt) => match **else_stmt {
                        Statement::Block(ref stmts) => {
                            assert_eq!(stmts.len(), 1);
                            match &stmts[0] {
                                Statement::VarAssignment(name, expr) => {
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
                // Check condition
                assert!(matches!(*condition, Expression::GT(_, _)));

                // Check then block
                match *then_block {
                    Statement::Block(ref stmts) => {
                        assert_eq!(stmts.len(), 1);
                        match &stmts[0] {
                            Statement::VarAssignment(name, expr) => {
                                assert_eq!(name, "y");
                                assert!(matches!(**expr, Expression::CInt(1)));
                            }
                            _ => panic!("Expected Assignment in then block"),
                        }
                    }
                    _ => panic!("Expected Block"),
                }

                // Check else block
                match else_block {
                    Some(else_stmt) => match *else_stmt {
                        Statement::Block(ref stmts) => {
                            assert_eq!(stmts.len(), 1);
                            match &stmts[0] {
                                Statement::VarAssignment(name, expr) => {
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
                // Check condition
                assert!(matches!(
                    *condition,
                    Expression::GT(_box_ref @ _, _box_ref2 @ _)
                ));

                // Check then block
                match *then_block {
                    Statement::Block(ref stmts) => {
                        assert_eq!(stmts.len(), 1);
                        match &stmts[0] {
                            Statement::VarAssignment(name, expr) => {
                                assert_eq!(name, "y");
                                assert!(matches!(**expr, Expression::CInt(1)));
                            }
                            _ => panic!("Expected Assignment"),
                        }
                    }
                    _ => panic!("Expected Block"),
                }

                // Check else block
                match else_block {
                    Some(else_stmt) => match *else_stmt {
                        Statement::Block(ref stmts) => {
                            assert_eq!(stmts.len(), 1);
                            match &stmts[0] {
                                Statement::VarAssignment(name, expr) => {
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
            Statement::VarAssignment(name, expr) => {
                assert_eq!(&**name, "x");
                match **expr {
                    Expression::CInt(42) => (),
                    _ => panic!("Expected CInt(42)"),
                }
            }
            _ => panic!("Expected Assignment"),
        }

        match &stmts[1] {
            Statement::VarAssignment(name, expr) => {
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

    #[test]
    fn test_dictionary_expression() {
        let input = "{x: 42, y: 10}";

        let (rest, dict) = dictionary_expression(input).unwrap();
        assert_eq!(rest, "");

        let expected_pairs = vec![("x", 42), ("y", 10)];

        match dict {
            Expression::Dict(pairs) => {
                assert_eq!(pairs.len(), 2, "Expected 2 pairs in dictionary");

                for (i, pair) in pairs.iter().enumerate() {
                    assert_eq!(
                        pair.0, expected_pairs[i].0,
                        "Expected key '{}'",
                        expected_pairs[i].0
                    );

                    match *pair.1 {
                        Expression::CInt(value) => assert_eq!(value, expected_pairs[i].1),
                        _ => panic!(
                            "Expected CInt for the value of key '{}'",
                            expected_pairs[i].0
                        ),
                    }
                }
            }
            _ => panic!("Expected Dictionary"),
        }
    }

    #[test]
    fn test_empty_dictionary_expression() {
        let input = "{}";

        let (rest, dict) = dictionary_expression(input).unwrap();
        assert_eq!(rest, "");

        match dict {
            Expression::Dict(pairs) => {
                assert_eq!(pairs.len(), 0, "Expected 0 pairs in dictionary");
            }
            _ => panic!("Expected Dictionary"),
        }
    }

    #[test]
    fn test_dictionary_exp_with_whitespace() {
        let input = r#"{
        w:    12,      x   : 42   ,
            y :    11,
        z:14
               }"#;

        let (rest, dict) = dictionary_expression(input).unwrap();
        assert_eq!(rest, "");

        let expected_pairs = vec![("w", 12), ("x", 42), ("y", 11), ("z", 14)];

        match dict {
            Expression::Dict(pairs) => {
                assert_eq!(pairs.len(), 4, "Expected 4 pairs in dictionary");

                for (i, pair) in pairs.iter().enumerate() {
                    assert_eq!(
                        pair.0, expected_pairs[i].0,
                        "Expected key '{}'",
                        expected_pairs[i].0
                    );

                    match *pair.1 {
                        Expression::CInt(value) => assert_eq!(value, expected_pairs[i].1),
                        _ => panic!(
                            "Expected CInt for the value of key '{}'",
                            expected_pairs[i].0
                        ),
                    }
                }
            }
            _ => panic!("Expected Dictionary"),
        }
    }

    #[test]
    fn test_dictionary_with_complex_expressions() {
        let input = "{x: 42 + 10 + var, y: 10 - 2 * 3}";

        let (rest, dict) = dictionary_expression(input).unwrap();
        assert_eq!(rest, "");

        let expected_pairs = vec![
            (
                "x",
                Box::new(Expression::Add(
                    Box::new(Expression::Add(
                        Box::new(Expression::CInt(42)),
                        Box::new(Expression::CInt(10)),
                    )),
                    Box::new(Expression::Var(String::from("var"))),
                )),
            ),
            (
                "y",
                Box::new(Expression::Mul(
                    Box::new(Expression::Sub(
                        Box::new(Expression::CInt(10)),
                        Box::new(Expression::CInt(2)),
                    )),
                    Box::new(Expression::CInt(3)),
                )),
            ),
        ];

        match dict {
            Expression::Dict(pairs) => {
                assert_eq!(pairs.len(), 2, "Expected 2 pairs in dictionary");

                for (i, pair) in pairs.iter().enumerate() {
                    assert_eq!(
                        pair.0, expected_pairs[i].0,
                        "Expected key '{}'",
                        expected_pairs[i].0
                    );

                    assert_eq!(
                        pair.1, expected_pairs[i].1,
                        "Key '{}' has a different type than expected",
                        expected_pairs[i].0,
                    );
                }
            }
            _ => panic!("Expected Dictionary"),
        }
    }

    #[test]
    fn test_dictionary_access_expression() {
        let input = "dict.key";

        let (rest, access_exp) = dictionary_access_expression(input).unwrap();
        assert_eq!(rest, "");

        match access_exp {
            Expression::DictAccess(dict_name, key_name) => {
                assert_eq!(dict_name, "dict");
                assert_eq!(key_name, "key");
            }
            _ => panic!("Expected dictionary access expression"),
        }
    }

    #[test]
    fn test_dict_assignment() {
        let input = "dict.key = 42";
        let (rest, stmt) = dict_assignment(input).unwrap();
        assert_eq!(rest, "");

        match stmt {
            Statement::DictAssigment(dict_name, key_name, exp) => {
                assert_eq!(dict_name, "dict");
                assert_eq!(key_name, "key");

                match *exp {
                    Expression::CInt(val) => assert_eq!(val, 42),
                    _ => panic!("Expected CInt"),
                }
            }
            _ => panic!("Expected dictionary key assignment"),
        }
    }

    #[test]
    fn test_dict_assignment_with_complex_expressions() {
        let input = "dict.key = 42 * (10 - (2 / 1))";
        let (rest, stmt) = dict_assignment(input).unwrap();
        assert_eq!(rest, "");

        match stmt {
            Statement::DictAssigment(dict_name, key_name, exp) => {
                assert_eq!(dict_name, "dict");
                assert_eq!(key_name, "key");

                let expected_exp = Box::new(Expression::Mul(
                    Box::new(Expression::CInt(42)),
                    Box::new(Expression::Sub(
                        Box::new(Expression::CInt(10)),
                        Box::new(Expression::Div(
                            Box::new(Expression::CInt(2)),
                            Box::new(Expression::CInt(1)),
                        )),
                    )),
                ));

                assert_eq!(
                    exp, expected_exp,
                    "Expression has a different type than expected"
                );
            }
            _ => panic!("Expected dictionary key assignment"),
        }
    }

    #[test]
    fn test_in_expression() {
        let input = "key in dict";

        let (rest, access_exp) = in_expression(input).unwrap();
        assert_eq!(rest, "");

        match access_exp {
            Expression::In(item_name, collection_name) => {
                assert_eq!(item_name, "key");
                assert_eq!(collection_name, "dict");
            }
            _ => panic!("Expected 'in' expression"),
        }
    }

    #[test]
    fn test_not_in_expression() {
        let input = "key not in dict";

        let (rest, access_exp) = not_in_expression(input).unwrap();
        assert_eq!(rest, "");

        match access_exp {
            Expression::NotIn(item_name, collection_name) => {
                assert_eq!(item_name, "key");
                assert_eq!(collection_name, "dict");
            }
            _ => panic!("Expected 'in' expression"),
        }
    }

    #[test]
    fn test_dictionary_merge_expression() {
        let input = "dict with {x: 42, y: 10}";

        let (rest, dict) = dict_merge_expression(input).unwrap();
        assert_eq!(rest, "");

        let expected_dict_name = "dict";
        let expected_new_entries = vec![
            ("x", Box::new(Expression::CInt(42))),
            ("y", Box::new(Expression::CInt(10))),
        ];

        match dict {
            Expression::DictMerge(dict_name, new_entries) => {
                assert_eq!(dict_name, expected_dict_name);
                assert_eq!(new_entries.len(), 2, "Expected 2 new entries");

                for (i, pair) in expected_new_entries.iter().enumerate() {
                    assert_eq!(
                        pair.0, expected_new_entries[i].0,
                        "Expected new key '{}'",
                        expected_new_entries[i].0
                    );

                    assert_eq!(
                        pair.1, expected_new_entries[i].1,
                        "Key '{}' has a different type than expected",
                        expected_new_entries[i].0,
                    );
                }
            }
            _ => panic!("Expected dictionary 'with' expression"),
        }
    }
}
