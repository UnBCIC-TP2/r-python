use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::collections::HashSet;

pub type Name = String;

use nom::IResult;
use std::collections::HashMap;

#[derive(Clone)]
pub struct Frame<A> {
    pub parent_function: Option<Function>,
    pub parent_key: Option<(Name, i32)>,
    pub variables: HashMap<Name, A>,
}

impl<A> Frame<A> {
    pub fn new(func: Option<Function>, key: Option<(Name, i32)>) -> Frame<A> {
        let variables: HashMap<Name, A> = HashMap::new();

        Frame {
            parent_function: func,
            parent_key: key,
            variables,
        }
    }
}

#[derive(Clone)]
pub struct Environment<A> {
    pub scope: Function,
    pub recursion: i32,
    pub stack: HashMap<(Name, i32), Frame<A>>,
}

impl<A> Environment<A> {
    pub fn new() -> Environment<A> {
        let frame: Frame<A> = Frame::new(None, None);
        let scope = Function::new();

        Environment {
            scope,
            recursion: 0,
            stack: HashMap::from([(("__main__".to_string(), 0), frame)]),
        }
    }

    pub fn scope_key(&self) -> (Name, i32) {
        (self.scope_name(), self.recursion)
    }

    pub fn scope_name(&self) -> Name {
        self.scope.name.clone()
    }

    pub fn scope_return(&self) -> Option<&A> {
        self.search_frame(self.scope_name())
    }

    pub fn get_frame(&self, key: (Name, i32)) -> &Frame<A> {
        self.stack.get(&key).unwrap()
    }

    pub fn search_frame(&self, name: Name) -> Option<&A> {
        self.stack
            .get(&self.scope_key())
            .unwrap()
            .variables
            .get(&name)
    }

    pub fn insert_frame(&mut self, func: Function) {
        let new_frame: Frame<A> = Frame::new(Some(self.scope.clone()), Some(self.scope_key()));

        self.stack
            .insert((func.name.clone(), self.scope_key().1 + 1), new_frame);
        self.scope = func;
        self.recursion += 1;
    }

    pub fn remove_frame(&mut self) {
        let recursion = self.scope_key().1 - 1;
        self.scope = self
            .stack
            .remove(&self.scope_key())
            .unwrap()
            .parent_function
            .unwrap();
        self.recursion = recursion;
    }

    pub fn insert_variable(&mut self, name: Name, kind: A) {
        if let Some(frame) = self.stack.get_mut(&self.scope_key()) {
            frame.variables.insert(name, kind);
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub name: Name,
    pub kind: Option<Type>,
    pub params: Option<Vec<(Name, Type)>>,
    pub body: Option<Box<Statement>>,
}

impl Function {
    pub fn new() -> Function {
        Function {
            name: "__main__".to_string(),
            kind: None,
            params: None,
            body: None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    TInteger,
    TBool,
    TReal,
    TString,
    TFunction(Box<Option<Type>>, Vec<Type>),
    TList(Box<Type>),
    TTuple(Vec<Type>),
    TSet(Box<Type>),
    // TDict(Box<Type>,Box<Type>),
    THash(Box<Type>, Box<Type>),
    TUnit,
}

//#[derive(Debug, PartialEq, Clone)]
#[derive(Debug, Clone)]
pub enum Expression {
    /* constants */
    CTrue,
    CFalse,
    CInt(i32),
    CReal(f64),
    CString(String),
    

    /* variable reference */
    Var(Name),

    /* function call */
    FuncCall(Name, Vec<Expression>),

    /* arithmetic expressions over numbers */
    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),

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

    /* Data Structure */
    List(Vec<Expression>,Box<Expression>),
    Tuple(Vec<Expression>),
    Set(Vec<Expression>),
    
    Append(Box<Expression>,Box<Expression>),
    Pop(Box<Expression>),
    Get(Box<Expression>,Box<Expression>),
    Len(Box<Expression>),
    
    // Dict(Option<Vec<(Expression, Expression)>>),
    // GetDict(Box<Expression>, Box<Expression>),
    // SetDict(Box<Expression>, Box<Expression>, Box<Expression>),
    // RemoveDict(Box<Expression>, Box<Expression>),
    
    Hash(Option<HashMap<Expression, Expression>>),
    GetHash(Box<Expression>, Box<Expression>),
    SetHash(Box<Expression>, Box<Expression>, Box<Expression>),
    RemoveHash(Box<Expression>, Box<Expression>),
}

impl PartialEq for Expression {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // Comparison of constants
            (Expression::CTrue, Expression::CTrue) => true,
            (Expression::CFalse, Expression::CFalse) => true,
            (Expression::CInt(a), Expression::CInt(b)) => a == b,
            (Expression::CReal(a), Expression::CReal(b)) => a == b,
            (Expression::CString(a), Expression::CString(b)) => a == b,

            // Comparison of variables
            (Expression::Var(a), Expression::Var(b)) => a == b,

            // Comparison of arithmetic expressions
            (Expression::Add(a1, b1), Expression::Add(a2, b2)) => a1 == a2 && b1 == b2,
            (Expression::Sub(a1, b1), Expression::Sub(a2, b2)) => a1 == a2 && b1 == b2,
            (Expression::Mul(a1, b1), Expression::Mul(a2, b2)) => a1 == a2 && b1 == b2,
            (Expression::Div(a1, b1), Expression::Div(a2, b2)) => a1 == a2 && b1 == b2,
            (Expression::Rmd(a1, b1), Expression::Rmd(a2, b2)) => a1 == a2 && b1 == b2,

            // Comparison of boolean expressions
            (Expression::And(a1, b1), Expression::And(a2, b2)) => a1 == a2 && b1 == b2,
            (Expression::Or(a1, b1), Expression::Or(a2, b2)) => a1 == a2 && b1 == b2,
            (Expression::Not(a1), Expression::Not(a2)) => a1 == a2,

            // Comparison of relational expressions
            (Expression::EQ(a1, b1), Expression::EQ(a2, b2)) => a1 == a2 && b1 == b2,
            (Expression::GT(a1, b1), Expression::GT(a2, b2)) => a1 == a2 && b1 == b2,
            (Expression::LT(a1, b1), Expression::LT(a2, b2)) => a1 == a2 && b1 == b2,
            (Expression::GTE(a1, b1), Expression::GTE(a2, b2)) => a1 == a2 && b1 == b2,
            (Expression::LTE(a1, b1), Expression::LTE(a2, b2)) => a1 == a2 && b1 == b2,

            // Comparison of data structures (List, Tuple)
            (Expression::List(a1, b1), Expression::List(a2, b2)) => a1 == a2 && b1 == b2,
            (Expression::Tuple(a1), Expression::Tuple(a2)) => a1 == a2,

            // Comparison of data structures (Dict, Hash)
            // (Expression::Dict(a1), Expression::Dict(a2)) => a1 == a2,
            (Expression::Hash(a1), Expression::Hash(a2)) => {
                match (a1, a2) {
                    (Some(map1), Some(map2)) => map1 == map2,
                    (None, None) => true,
                    _ => false,
                }
            },
            // Comparison of data structures (Set)
            (Expression::Set(a), Expression::Set(b)) => {
                let set_a: HashSet<_> = a.iter().collect();
                let set_b: HashSet<_> = b.iter().collect();
                set_a == set_b
            }
            // Other cases
            _ => false,
        }
    }
}


impl Eq for Expression {}

impl Hash for Expression {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Expression::CInt(i) => i.hash(state),
            Expression::CReal(f) => f.to_bits().hash(state), // f64 -> bits
            Expression::CString(s) => s.hash(state),
            Expression::CTrue => 0.hash(state),
            Expression::CFalse => 1.hash(state),
            Expression::Var(v) => v.hash(state),
            Expression::Add(a, b) => {
                a.hash(state);
                b.hash(state);
            }
            Expression::Set(elements) => {
                for element in elements {
                    element.hash(state);
                }
            }
            _ => 0.hash(state),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    VarDeclaration(Name),
    ValDeclaration(Name),
    Assignment(Name, Box<Expression>, Option<Type>),
    IfThenElse(Box<Expression>, Box<Statement>, Option<Box<Statement>>),
    While(Box<Expression>, Box<Statement>),
    Block(Vec<Statement>),
    Sequence(Box<Statement>, Box<Statement>),
    FuncDef(Function),
    Return(Box<Expression>),
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