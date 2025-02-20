use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::collections::HashSet;

pub type Name = String;

#[derive(Debug, PartialEq,Clone)]
pub enum Type {
    TInteger,
    TBool,
    TReal,
    TString,
    NotDefine,
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

    /* Data Structure */
    List(Vec<Expression>),
    Tuple(Vec<Expression>),
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
            (Expression::List(a1 ), Expression::List(a2)) => a1 == a2,
            (Expression::Tuple(a1), Expression::Tuple(a2)) => a1 == a2,
            
            // Comparison of data structures (Hash)
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
    Assignment(Name, Box<Expression>),
    IfThenElse(Box<Expression>, Box<Statement>, Option<Box<Statement>>),
    While(Box<Expression>, Box<Statement>),
    Sequence(Box<Statement>, Box<Statement>),
}