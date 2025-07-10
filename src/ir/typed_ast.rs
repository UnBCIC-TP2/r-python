use crate::ir::ast::{Name, Type, ValueConstructor,FormalArgument};


#[derive(Debug, PartialEq, Clone)]
pub enum TypedExpression {
    CTrue { type_info: Type },
    CFalse { type_info: Type },
    CInt { value: i32, type_info: Type },
    CReal { value: f64, type_info: Type },
    CString { value: String, type_info: Type },
    CVoid { type_info: Type },

    Var { name: Name, type_info: Type },

    // Expressões aritméticas
    Add {
        left: Box<TypedExpression>,
        right: Box<TypedExpression>,
        type_info: Type,
    },
    Sub {
        left: Box<TypedExpression>,
        right: Box<TypedExpression>,
        type_info: Type,
    },
    Mul{
        left: Box<TypedExpression>,
        right: Box<TypedExpression>,
        type_info: Type,
    },
    Div{
        left: Box<TypedExpression>,
        right: Box<TypedExpression>,
        type_info: Type,
    },

    And {
        left: Box<TypedExpression>,
        right: Box<TypedExpression>,
        type_info: Type, 
    },
    Or {
        left: Box<TypedExpression>,
        right: Box<TypedExpression>,
        type_info: Type, 
    },
    Not {
        expr: Box<TypedExpression>,
        type_info: Type, 
    },

    // Relational expressions over numbers
    EQ {
        left: Box<TypedExpression>,
        right: Box<TypedExpression>,
        type_info: Type,
    },
    NEQ {
        left: Box<TypedExpression>,
        right: Box<TypedExpression>,
        type_info: Type, 
    },
    GT {
        left: Box<TypedExpression>,
        right: Box<TypedExpression>,
        type_info: Type, 
    },
    LT {
        left: Box<TypedExpression>,
        right: Box<TypedExpression>,
        type_info: Type, 
    },
    GTE {
        left: Box<TypedExpression>,
        right: Box<TypedExpression>,
        type_info: Type, 
    },
    LTE {
        left: Box<TypedExpression>,
        right: Box<TypedExpression>,
        type_info: Type, 
    },

    COk { expr: Box<TypedExpression>, type_info: Type },    
    CErr { expr: Box<TypedExpression>, type_info: Type },   
    CJust { expr: Box<TypedExpression>, type_info: Type },   
    CNothing { type_info: Type },                      

    Unwrap { expr: Box<TypedExpression>, type_info: Type }, 
    IsError { expr: Box<TypedExpression>, type_info: Type }, 
    IsNothing { expr: Box<TypedExpression>, type_info: Type }, 
    Propagate { expr: Box<TypedExpression>, type_info: Type }, 

    // List value
    ListValue {
        elements: Vec<TypedExpression>,
        type_info: Type, 
    },

    // Constructor
    Constructor {
        name: Name,
        args: Vec<Box<TypedExpression>>,
        type_info: Type, 
    },

    FuncCall {
        name: Name,
        args: Vec<Box<TypedExpression>>,
        type_info: Type,
    },

}

impl TypedExpression {
    pub fn type_info(&self) -> &Type {
        match self {
            TypedExpression::CTrue { type_info } => type_info,
            TypedExpression::CFalse { type_info } => type_info,
            TypedExpression::CInt { type_info, .. } => type_info,
            TypedExpression::CReal { type_info, .. } => type_info,
            TypedExpression::CString { type_info, .. } => type_info,
            TypedExpression::CVoid { type_info } => type_info,
            TypedExpression::Var { type_info, .. } => type_info,
            TypedExpression::FuncCall { type_info, .. } => type_info,

            // Arithmetic expressions
            TypedExpression::Add { type_info, .. } => type_info,
            TypedExpression::Sub { type_info, .. } => type_info,
            TypedExpression::Mul { type_info, .. } => type_info,
            TypedExpression::Div { type_info, .. } => type_info,

            // Boolean expressions
            TypedExpression::And { type_info, .. } => type_info,
            TypedExpression::Or { type_info, .. } => type_info,
            TypedExpression::Not { type_info, .. } => type_info,

            // Relational expressions
            TypedExpression::EQ { type_info, .. } => type_info,
            TypedExpression::NEQ { type_info, .. } => type_info,
            TypedExpression::GT { type_info, .. } => type_info,
            TypedExpression::LT { type_info, .. } => type_info,
            TypedExpression::GTE { type_info, .. } => type_info,
            TypedExpression::LTE { type_info, .. } => type_info,

            // Error-related expressions
            TypedExpression::COk { type_info, .. } => type_info,
            TypedExpression::CErr { type_info, .. } => type_info,
            TypedExpression::CJust { type_info, .. } => type_info,
            TypedExpression::CNothing { type_info } => type_info,
            TypedExpression::Unwrap { type_info, .. } => type_info,
            TypedExpression::IsError { type_info, .. } => type_info,
            TypedExpression::IsNothing { type_info, .. } => type_info,
            TypedExpression::Propagate { type_info, .. } => type_info,

            // List and Constructor
            TypedExpression::ListValue { type_info, .. } => type_info,
            TypedExpression::Constructor { type_info, .. } => type_info,
        }
    }
}


#[derive(Debug, PartialEq, Clone)]
pub struct TypedFunction {
    pub name: Name,
    pub kind: Type,
    pub params: Vec<FormalArgument>, 
    pub body: Option<Box<TypedStatement>>,
}


#[derive(Debug, PartialEq, Clone)]
pub enum TypedStatement {

    // Declaração de variável agora contém uma TypedExpression
    VarDeclaration {
        name: Name,
        value: Box<TypedExpression>,
    },
    ValDeclaration {
        name: Name,
        value: Box<TypedExpression>,
    },
    Assignment {
        name: Name,
        value: Box<TypedExpression>,
    },

    IfThenElse {
        cond: Box<TypedExpression>,
        stmt_then: Box<TypedStatement>,
        stmt_else_opt: Option<Box<TypedStatement>>,
    },
    While {
        cond: Box<TypedExpression>,
        body: Box<TypedStatement>,
    },
    For {
        name: Name,
        iterable: Box<TypedExpression>,
        body: Box<TypedStatement>,
    },
 
    Block(Vec<TypedStatement>),
    
    Sequence {
    first: Box<TypedStatement>,
    second: Box<TypedStatement>,
    },

    Assert {
        left: Box<TypedExpression>,
        right: Box<TypedExpression>,
    },
    AssertTrue {
        expr: Box<TypedExpression>,
        message: String,
    },
    AssertFalse {
        expr: Box<TypedExpression>,
        message: String,
    },
    AssertEQ {
        left: Box<TypedExpression>,
        right: Box<TypedExpression>,
        message: String,
    },
    AssertNEQ {
        left: Box<TypedExpression>,
        right: Box<TypedExpression>,
        message: String,
    },

    TestDef(TypedFunction),
    ModTestDef {
        name: Name,
        body: Box<TypedStatement>,
    },

    AssertFails(String),

    FuncDef(TypedFunction),

    Return(Box<TypedExpression>),

    TypeDeclaration(Name, Vec<ValueConstructor>),


}
