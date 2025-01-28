pub type Name = String;

use nom::IResult;

#[derive(Debug, PartialEq)]
pub enum Type {
    TInteger,
    TBool,
    TReal,
    TString,
    TList(Box<Type>),
    TTuple(Vec<Type>),
    TDict(Vec<(Name, Type)>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    /* constants */
    CTrue,
    CFalse,
    CInt(i32),
    CReal(f64),
    CString(String),

    /* variable reference */
    Var(Name),

    /* arithmetic expressions over numbers */
    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    Rmd(Box<Expression>, Box<Expression>),

    /* boolean expressions over booleans */
    And(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),
    Not(Box<Expression>),

    /* relational expressions over numbers */
    EQ(Box<Expression>, Box<Expression>),
    GT(Box<Expression>, Box<Expression>),
    LT(Box<Expression>, Box<Expression>),
    GTE(Box<Expression>, Box<Expression>),
    LTE(Box<Expression>, Box<Expression>),

    /* data structures */
    Dict(Vec<(Name, Box<Expression>)>),
    DictAccess(Name, Name),

    /* membership checks over collections */
    In(Name, Name),
    NotIn(Name, Name),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    VarDeclaration(Name),
    ValDeclaration(Name),
    VarAssignment(Name, Box<Expression>),
    IfThenElse(Box<Expression>, Box<Statement>, Option<Box<Statement>>),
    While(Box<Expression>, Box<Statement>),
    Block(Vec<Statement>), // For indented blocks
    Sequence(Box<Statement>, Box<Statement>),
    DictAssigment(Name, Name, Box<Expression>),
}

#[derive(Debug)]
pub enum ParseError {
    IndentationError(usize),
    UnexpectedToken(String),
    InvalidExpression(String),
}

pub fn with_error_context<'a, T>(
    parser: impl Fn(&'a str) -> IResult<&'a str, T>,
    _context: &'a str,
) -> impl Fn(&'a str) -> IResult<&'a str, T> {
    move |input| {
        parser(input)
            .map_err(|_| nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag)))
    }
}
