use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::collections::HashSet;

pub type Name = String;

use nom::IResult;

#[derive(Debug, PartialEq, Clone)]
pub struct Frame<A> {
    pub parent_function: Option<Function>,
    pub parent_key: Option<(Name, i32)>,
    pub variables: HashMap<Name, A>,
    pub tests: HashMap<Name, Function>,
}

impl<A> Frame<A> {
    pub fn new(func: Option<Function>, key: Option<(Name, i32)>) -> Frame<A> {
        let variables: HashMap<Name, A> = HashMap::new();
        let tests: HashMap<Name, Function> = HashMap::new();
        return Frame {
            parent_function: func,
            parent_key: key,
            variables,
            tests,
        };
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Environment<A> {
    pub scope: Function,
    pub recursion: i32,
    pub stack: HashMap<(Name, i32), Frame<A>>,
}

impl<A> Environment<A> {
    pub fn new() -> Environment<A> {
        let frame: Frame<A> = Frame::new(None, None);
        let scope = Function::new();

        return Environment {
            scope,
            recursion: 0,
            stack: HashMap::from([(("__main__".to_string(), 0), frame)]),
        };
    }

    pub fn scope_key(&self) -> (Name, i32) {
        return (self.scope_name(), self.recursion);
    }

    pub fn scope_name(&self) -> Name {
        return self.scope.name.clone();
    }

    pub fn scope_return(&self) -> Option<&A> {
        return self.search_frame(self.scope_name());
    }

    pub fn get_frame(&self, key: (Name, i32)) -> &Frame<A> {
        return self.stack.get(&key).unwrap();
    }

    pub fn search_frame(&self, name: Name) -> Option<&A> {
        return self
            .stack
            .get(&self.scope_key())
            .unwrap()
            .variables
            .get(&name);
    }

    pub fn insert_frame(&mut self, func: Function) -> () {
        let new_frame: Frame<A> = Frame::new(Some(self.scope.clone()), Some(self.scope_key()));

        self.stack
            .insert((func.name.clone(), self.scope_key().1 + 1), new_frame);
        self.scope = func;
        self.recursion += 1;
    }

    pub fn remove_frame(&mut self) -> () {
        let recursion = self.scope_key().1 - 1;
        self.scope = self
            .stack
            .remove(&self.scope_key())
            .unwrap()
            .parent_function
            .unwrap();
        self.recursion = recursion;
    }

    pub fn insert_variable(&mut self, name: Name, kind: A) -> () {
        if let Some(frame) = self.stack.get_mut(&self.scope_key()) {
            frame.variables.insert(name, kind);
        }
    }

    pub fn insert_test(&mut self, name: Name, test: Function) -> () {
        if let Some(frame) = self.stack.get_mut(&self.scope_key()) {
            frame.tests.insert(name, test);
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub name: Name,
    pub kind: Option<Type>,
    pub params: Option<Vec<(Name, Type)>>,
    pub body: Option<Box<Statement>>,
}

impl Function {
    pub fn new() -> Function {
        return Function {
            name: "__main__".to_string(),
            kind: None,
            params: None,
            body: None,
        };
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TestEnvironment<A> {
    pub name: Name,
    pub env: Environment<A>,
}

impl<A> TestEnvironment<A> {
    pub fn new() -> TestEnvironment<A> {
        return TestEnvironment {
            name: "__test__".to_string(),
            env: Environment::<A>::new(),
        };
    }
}
#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    TInteger,
    TBool,
    TReal,
    TString,
    TVoid,
    TFunction(Box<Option<Type>>, Vec<Type>),
    NotDefine,
    TList(Box<Type>),
    TTuple(Vec<Type>),
    TSet(Box<Type>),
    THash(Box<Type>, Box<Type>),
    TMaybe(Box<Type>),
    TResult(Box<Type>, Box<Type>),
    TAny,
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
    CVoid,

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
    List(Vec<Expression>),
    Tuple(Vec<Expression>),
    Hash(Option<HashMap<Expression, Expression>>),
    Set(Vec<Expression>),


    Union(Box<Expression>,Box<Expression>),
    Intersection(Box<Expression>,Box<Expression>),
    Difference(Box<Expression>,Box<Expression>),
    Disjunction(Box<Expression>,Box<Expression>),
    Insert(Box<Expression>,Box<Expression>),
    Append(Box<Expression>,Box<Expression>),
    Concat(Box<Expression>,Box<Expression>),
    PopBack(Box<Expression>),
    PopFront(Box<Expression>),
    Get(Box<Expression>,Box<Expression>),
    Len(Box<Expression>),
    GetHash(Box<Expression>, Box<Expression>),
    SetHash(Box<Expression>, Box<Expression>, Box<Expression>),
    RemoveHash(Box<Expression>, Box<Expression>),

    /* error expressions */
    COk(Box<Expression>),
    CErr(Box<Expression>),

    CJust(Box<Expression>),
    CNothing,

    Unwrap(Box<Expression>),
    IsError(Box<Expression>),
    IsNothing(Box<Expression>),
    Propagate(Box<Expression>),
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
            (Expression::List(ref a1), Expression::List(ref a2)) => a1 == a2,
            (Expression::Tuple(ref a1), Expression::Tuple(ref a2)) => a1 == a2,
            
            // Comparison of data structures (Hash)
            (Expression::Hash(ref a1), Expression::Hash(ref a2)) => {
                match (a1, a2) {
                    (Some(map1), Some(map2)) => map1 == map2,
                    (None, None) => true,
                    _ => false,
                }
            },
            
            // Comparison of data structures (Set)
            (Expression::Set(ref a), Expression::Set(ref b)) => {
                let set_a: HashSet<_> = a.iter().collect();
                let set_b: HashSet<_> = b.iter().collect();
                set_a == set_b
            }

            // Comparison of union, intersection, difference, and other expressions
            (Expression::Union(a1, b1), Expression::Union(a2, b2)) => a1 == a2 && b1 == b2,
            (Expression::Intersection(a1, b1), Expression::Intersection(a2, b2)) => a1 == a2 && b1 == b2,
            (Expression::Difference(a1, b1), Expression::Difference(a2, b2)) => a1 == a2 && b1 == b2,
            (Expression::Disjunction(a1, b1), Expression::Disjunction(a2, b2)) => a1 == a2 && b1 == b2,
            (Expression::Insert(a1, b1), Expression::Insert(a2, b2)) => a1 == a2 && b1 == b2,
            (Expression::Append(a1, b1), Expression::Append(a2, b2)) => a1 == a2 && b1 == b2,
            (Expression::Concat(a1, b1), Expression::Concat(a2, b2)) => a1 == a2 && b1 == b2,
            (Expression::PopBack(a1), Expression::PopBack(a2)) => a1 == a2,
            (Expression::PopFront(a1), Expression::PopFront(a2)) => a1 == a2,
            (Expression::Get(a1, b1), Expression::Get(a2, b2)) => a1 == a2 && b1 == b2,
            (Expression::Len(a1), Expression::Len(a2)) => a1 == a2,
            (Expression::GetHash(a1, b1), Expression::GetHash(a2, b2)) => a1 == a2 && b1 == b2,
            (Expression::SetHash(a1, b1, c1), Expression::SetHash(a2, b2, c2)) => a1 == a2 && b1 == b2 && c1 == c2,
            (Expression::RemoveHash(a1, b1), Expression::RemoveHash(a2, b2)) => a1 == a2 && b1 == b2,

            // Error and Maybe expressions
            (Expression::COk(a1), Expression::COk(a2)) => a1 == a2,
            (Expression::CErr(a1), Expression::CErr(a2)) => a1 == a2,
            (Expression::CJust(a1), Expression::CJust(a2)) => a1 == a2,
            (Expression::CNothing, Expression::CNothing) => true,
            (Expression::IsError(a1), Expression::IsError(a2)) => a1 == a2,
            (Expression::IsNothing(a1), Expression::IsNothing(a2)) => a1 == a2,
            (Expression::Unwrap(a1), Expression::Unwrap(a2)) => a1 == a2,
            (Expression::Propagate(a1), Expression::Propagate(a2)) => a1 == a2,

            // Comparison of function calls
            (Expression::FuncCall(ref name1, ref args1), Expression::FuncCall(ref name2, ref args2)) => {
                name1 == name2 && args1 == args2
            }

            // Default case for unmatched expressions
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
    AssertTrue(Box<Expression>, String),
    AssertFalse(Box<Expression>, String),
    AssertEQ(Box<Expression>, Box<Expression>, String),
    AssertNEQ(Box<Expression>, Box<Expression>, String),
    TestDef(Function),
    ModTestDef(Name, Box<Statement>),
    AssertFails(String),
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
