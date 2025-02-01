pub type Name = String;

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    TInteger,
    TBool,
    TReal,
    TString,
    TList(Box<Type>),
    TTuple(Vec<Type>),
    Tadt(Name, Vec<ValueConstructor>),
}

#[derive(Debug,PartialEq, Clone)]
pub struct  ValueConstructor{
    pub name: Name,
    pub types: Vec<Type> 
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

    /* ADT Constructor */
    ADTConstructor(Name, Vec<Box<Expression>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    VarDeclaration(Name),
    ValDeclaration(Name),
    Assignment(Name, Box<Expression>),
    IfThenElse(Box<Expression>, Box<Statement>, Option<Box<Statement>>),
    While(Box<Expression>, Box<Statement>),
    Sequence(Box<Statement>, Box<Statement>), 
    ADTDeclaration(Name, Vec<ValueConstructor>),
}
