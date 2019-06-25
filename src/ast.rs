use crate::token::*;

pub trait Node {
    fn token_literal(&self) -> &str;
}

pub trait Statement: Node {
    fn statement_node();
}

pub trait Expression: Node {
    fn expression_node();
}

pub struct Program<T: Statement> {
    pub statements: Vec<T>,
}

impl<T: Statement> Program<T> {
    pub fn new() -> Self {
        Program {
            statements: Vec::new(),
        }
    }
}

impl<T: Statement> Node for Program<T> {
    fn token_literal(&self) -> &str {
        if let Some(stmt) = self.statements.first() {
            stmt.token_literal()
        } else {
            ""
        }
    }
}

pub struct Identifier {
    pub token: Token,
}

impl Node for Identifier {
    fn token_literal(&self) -> &str {
        match &self.token {
            Token::IDENT(_) => "IDENT",
            _ => "",
        }
    }
}

impl Expression for Identifier {
    fn expression_node() {}
}

pub struct LetStatement {
    //<T: Expression>
    pub token: Token,
    pub name: Identifier,
    //pub value: T,
}

//impl<T: Expression> LetStatement<T> {
impl Node for LetStatement {
    fn token_literal(&self) -> &str {
        match self.token {
            Token::LET => "LET",
            _ => "",
        }
    }
}

impl Statement for LetStatement {
    fn statement_node() {}
}
