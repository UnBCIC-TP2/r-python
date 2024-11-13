pub type Name = String;

#[derive(Debug, Clone)]
pub enum Expression {
    CInt(i32),
    Var(String),
    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    FuncCall(String, Vec<Expression>),
}

#[derive(Debug, Clone)]
pub enum Statement {
    VarDeclaration(Box<Name>),
    ValDeclaration(Box<Name>),
    Assignment(Box<Name>, Box<Expression>),
    IfThenElse(Box<Expression>, Box<Statement>, Box<Statement>),
    While(Box<Expression>, Box<Statement>),
    For(
        Box<Name>,
        Box<Expression>,
        Box<Expression>,
        Box<Expression>,
        Box<Statement>,
    ),
    Sequence(Box<Statement>, Box<Statement>),
    Func(
        Box<Name>,
        Vec<Name>,
        Option<Box<Statement>>,
        Box<Expression>,
    ),
}
