use crate::types::Type;

use super::*;

pub type Tokens<'source, 'tokens> = &'tokens [Token<'source>];
pub type Ast = Node;

#[derive(Debug, Clone)]
pub struct Node {
    pub range: crate::error::Range,
    pub inner: NodeInner,
}

#[derive(Debug, Clone)]
pub enum NodeInner {
    StatementList(Vec<Node>),
    String(String),
    Indentifier(String),
    Literal(crate::ir::Value),
    Assignment {
        name: Box<Node>,
        args: Vec<Node>,
        body: Box<Node>,
    },
    Function {
        name: String,
        body: Box<Node>,
    },
    TypedDeclare {
        name: String,
        type_: Type,
        value: Box<Node>,
    },
    Lambda {
        args: Vec<Node>,
        body: Box<Node>,
    },
    Call(String, Vec<Node>),
    UnaryOperation(UnaryOperation, Box<Node>),
    BinaryOperation(BinaryOperation, Box<Node>, Box<Node>),
    IfThenElse {
        condition: Box<Node>,
        then_branch: Box<Node>,
        else_branch: Option<Box<Node>>,
    },
    While {
        condition: Box<Node>,
        body: Box<Node>,
    },
    For {
        var: Option<Box<Node>>,
        range: Box<Node>,
        body: Box<Node>,
    },
    ArrayLiteral(Vec<Node>),
    Import {
        qualified: bool,
        path: Box<Node>,
    },
    Scope(Box<Node>),
    Return(Box<Node>),
    Defer(Box<Node>),
    MarkupBlock {
        tag: String,
        attributes: Vec<(String, Node)>,
        body: Option<Box<Node>>,
        siblings: Vec<Node>,
    },
    ObjectLiteral(Vec<(String, Node)>),
    Type(Type),
    Deref(Box<Node>),
    AddressOf(Box<Node>),
    Break,
    Continue,
}

impl NodeInner {
    pub fn to_node(self, range: (usize, usize)) -> Node {
        Node {
            range: crate::error::Range { range },
            inner: self,
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnaryOperation {
    Negate,
    Not,
    Deref,
    AddressOf,
}

impl UnaryOperation {
    pub fn stringify(&self) -> String {
        match self {
            UnaryOperation::Negate => "-".to_string(),
            UnaryOperation::Not => "!".to_string(),
            UnaryOperation::Deref => "*".to_string(),
            UnaryOperation::AddressOf => "&".to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NameAndType {
    pub name: String,
    pub type_: Type,
    pub range: (usize, usize),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Decorator {
    pub name: String,
    pub range: (usize, usize),
}

#[derive(Debug, Clone)]
pub enum BinaryOperation {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Eq,
    Neq,
    Lt,
    Gt,
    Lte,
    Gte,
    And,
    Or,
    Assign,
    AssignAnd(Box<BinaryOperation>),
    Declare,
    DeclareConst,
    Index,
    Concat,
    Range,
    RangeInclusive,
    WriteFile,
    EnvAssign,
    Pipe,
    SemiColon,
    Xor,
    Custom(String),
}

impl BinaryOperation {
    pub fn stringify(&self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Mul => "*",
            _ => "???",
        }
    }

    pub(crate) fn logical_output(&self) -> bool {
        matches!(
            self,
            Self::Eq
                | Self::Neq
                | Self::Lt
                | Self::Gt
                | Self::Lte
                | Self::Gte
                | Self::And
                | Self::Or
        )
    }
}
