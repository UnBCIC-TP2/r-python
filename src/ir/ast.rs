// src/ir/ast.rs

pub type Name = String;

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    CInt(i32),
    CReal(f64),
    CTrue,
    CFalse,
    CString(String), // Adicionado para literais de string
    Var(Name),
    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    Rmd(Box<Expression>, Box<Expression>),
    And(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),
    Not(Box<Expression>),
    EQ(Box<Expression>, Box<Expression>),
    GT(Box<Expression>, Box<Expression>),
    LT(Box<Expression>, Box<Expression>),
    GTE(Box<Expression>, Box<Expression>),
    LTE(Box<Expression>, Box<Expression>),
    /* file operations */
    ReadFile(Box<Expression>),   // read_file(path)
    WriteFile(Box<Expression>, Box<Expression>), // write_file(path, content)
    FileExists(Box<Expression>), // file_exists(path)
    
    /* function calls - for future extensibility */
    FunctionCall(Name, Vec<Expression>), // function_name(args...)

    Call(Name, Vec<Expression>),

}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Assignment(Name, Box<Expression>),
    VarDeclaration(Name),
    ValDeclaration(Name),
    IfThenElse(Box<Expression>, Box<Statement>, Option<Box<Statement>>),
    Block(Vec<Statement>),
    While(Box<Expression>, Box<Statement>),
    Sequence(Box<Statement>, Box<Statement>),
    ExpressionStmt(Expression),
}

// DEFINIÇÃO DA ENUM 'Type' - ESTA É A PARTE QUE O COMPILADOR ESTÁ PROCURANDO
#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Int,
    Real,
    Bool,
    String,
    None,    // Para o tipo de `None` do Python
    Unknown, // Para casos onde o tipo ainda não foi determinado ou é ambíguo
}
