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
    statements: Vec<T>,
}

impl<T: Statement> Program<T> {
    fn token_literal(&self) -> &str {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            ""
        }
    }
}

pub struct Identifier {
    token: Token,
    value: String,
}

impl Identifier {
    fn expression_node() {}

    fn token_literal(&self) -> &str {
        match &self.token {
            Token::IDENT(s) => s.as_str(),
            _ => "",
        }
    }
}

pub struct LetStatement<T: Expression> {
    token: Token,
    name: Identifier,
    value: T,
}

impl<T: Expression> LetStatement<T> {
    fn statement_node() {}

    fn token_literal(&self) -> &str {
        match self.token {
            Token::LET => "let",
            _ => "",
        }
    }
}
