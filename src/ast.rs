use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};

#[cfg(test)]
mod ast_test;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Statement {
    Let(String, Expression),
    Return(Expression),
    Expression(Expression),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Statement::Let(stmt, expr) => format!("let {} = {};", stmt, expr),
            Statement::Return(ret) => format!("return {};", ret),
            Statement::Expression(exp) => format!("{}", exp),
        };
        write!(f, "{}", s)
    }
}

pub type BlockStatement = Program;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Expression {
    Identifier(String),
    Integer(i64),
    Boolean(bool),
    String(String),
    Array(Vec<Expression>),
    Index(Box<Expression>, Box<Expression>),
    Hash(HashLiteral),
    Prefix(Prefix, Box<Expression>),
    Infix(Box<Expression>, Infix, Box<Expression>),
    If(Box<Expression>, BlockStatement, Option<BlockStatement>),
    Function(Vec<String>, BlockStatement),
    Call(Box<Expression>, Vec<Expression>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Expression::Identifier(identifier) => format!("{}", identifier),
            Expression::Integer(integer) => format!("{}", integer),
            Expression::Boolean(boolean) => format!("{}", boolean),
            Expression::String(string) => format!("{}", string),
            Expression::Array(array) => {
                let elements: Vec<String> =
                    array.iter().map(|element| element.to_string()).collect();
                format!("[{}]", elements.join(", "))
            }
            Expression::Index(left, right) => format!("({}[{}])", *left, *right),
            Expression::Hash(hash) => format!("{}", *hash),
            Expression::Prefix(prefix, right) => format!("({}{})", prefix, *right),
            Expression::Infix(left, infix, right) => format!("({} {} {})", *left, infix, *right),
            Expression::If(condition, consequence, alternative) => {
                if let Some(alternative) = alternative {
                    format!("if{} {}else {}", *condition, *consequence, *alternative)
                } else {
                    format!("if{} {}", *condition, *consequence)
                }
            }
            Expression::Function(parameters, body) => {
                let params: Vec<String> =
                    parameters.iter().map(|param| param.to_string()).collect();
                format!("fn({}) {}", params.join(", "), *body)
            }
            Expression::Call(function, arguments) => {
                let args: Vec<String> = arguments.iter().map(|arg| arg.to_string()).collect();
                format!("{}({})", *function, args.join(", "))
            }
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let stmts: Vec<String> = (&self.statements)
            .iter()
            .map(|stmt| stmt.to_string())
            .collect();
        write!(f, "{}", stmts.join("\n"))
    }
}

impl Program {
    pub fn new() -> Self {
        Program {
            statements: Vec::new(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Prefix {
    MINUS, // -
    BANG,  // !
}

impl fmt::Display for Prefix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Prefix::MINUS => "-",
                Prefix::BANG => "!",
            }
        )
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Infix {
    PLUS,     // +
    MINUS,    // -
    ASTERISK, // *
    SLASH,    // /
    LT,       // <
    GT,       // >
    EQ,       // ==
    NEQ,      // !=
}

impl fmt::Display for Infix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Infix::PLUS => "+",
                Infix::MINUS => "-",
                Infix::ASTERISK => "*",
                Infix::SLASH => "/",
                Infix::LT => "<",
                Infix::GT => ">",
                Infix::EQ => "==",
                Infix::NEQ => "!=",
            }
        )
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct HashLiteral {
    pub pairs: HashMap<Expression, Expression>,
}

impl Hash for HashLiteral {
    fn hash<H: Hasher>(&self, _state: &mut H) {
        panic!("hash not implemented for HashLiteral");
    }
}

impl fmt::Display for HashLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let pairs: Vec<String> = (&self.pairs)
            .iter()
            .map(|(k, v)| format!("{}:{}", k.to_string(), v.to_string()))
            .collect();
        write!(f, "{{{}}}", pairs.join(", "))
    }
}
