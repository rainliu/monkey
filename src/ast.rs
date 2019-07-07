use std::fmt;

#[cfg(test)]
mod ast_test;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let(IdentifierLiteral, Expression),
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

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Identifier(IdentifierLiteral),
    Integer(IntegerLiteral),
    Boolean(BooleanLiteral),
    String(StringLiteral),
    Prefix(Prefix, Box<Expression>),
    Infix(Box<Expression>, Infix, Box<Expression>),
    If(Box<Expression>, BlockStatement, Option<BlockStatement>),
    Function(Vec<IdentifierLiteral>, BlockStatement),
    Call(Box<Expression>, Vec<Expression>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Expression::Identifier(identifier) => format!("{}", identifier),
            Expression::Integer(integer) => format!("{}", integer),
            Expression::Boolean(boolean) => format!("{}", boolean),
            Expression::String(string) => format!("{}", string),
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

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct IdentifierLiteral(pub String);

impl fmt::Display for IdentifierLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IntegerLiteral(pub i64);

impl fmt::Display for IntegerLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StringLiteral(pub String);

impl fmt::Display for StringLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}


#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct BooleanLiteral(pub bool);

impl fmt::Display for BooleanLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
