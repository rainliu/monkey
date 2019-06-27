use std::iter::Peekable;

use crate::ast::*;
use crate::lexer::*;
use crate::token::*;
use std::fs::OpenOptions;

#[cfg(test)]
mod parser_test;

type PrefixParseFn = fn(&Token) -> Option<Expression>;
type InfixParseFn = fn(&Token, &Expression) -> Option<Expression>;

#[derive(Debug, Clone, PartialEq)]
enum Precedence {
    LOWEST = 0,
    EQUALS,      // ==
    LESSGREATER, // > or <
    SUM,         // +
    PRODUCT,     // *
    PREFIX,      // -X or !X
    CALL,        // myFunc(X)
}

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    cur_token: Token,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut p = Parser {
            lexer: lexer.peekable(),
            cur_token: Token::EOF,
            errors: Vec::new(),
        };

        p.next_token();

        p
    }

    fn next_token(&mut self) {
        if let Some(token) = self.lexer.next() {
            self.cur_token = token;
        } else {
            self.cur_token = Token::EOF;
        }
    }

    fn prefix_parse_fn(token: &Token) -> Option<PrefixParseFn> {
        match token {
            Token::IDENT(_) => Some(Parser::parse_indentifier),
            Token::INT(_) => Some(Parser::parse_integer),
            _ => None,
        }
    }

    fn parse_indentifier(token: &Token) -> Option<Expression> {
        match token {
            Token::IDENT(ident) => Some(Expression::Ident(Identifier(ident.to_string()))),
            _ => None,
        }
    }

    fn parse_integer(token: &Token) -> Option<Expression> {
        match token {
            Token::INT(int) => match int.parse::<i64>() {
                Ok(i) => Some(Expression::Int(Integer(i))),
                _ => None,
            },
            _ => None,
        }
    }

    fn infix_parse_fn(token: &Token) -> Option<InfixParseFn> {
        None
    }

    pub fn errors(&self) -> &[String] {
        &self.errors
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program::new();

        while self.cur_token != Token::EOF {
            let stmt = self.parse_statement();
            if let Some(stmt) = stmt {
                program.statements.push(stmt);
            }
            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token {
            Token::LET => self.parse_statement_let(),
            Token::RETURN => self.parse_statement_return(),
            _ => self.parse_statement_expression(),
        }
    }

    fn parse_statement_let(&mut self) -> Option<Statement> {
        match self.lexer.peek() {
            Some(&Token::IDENT(_)) => self.next_token(),
            Some(t) => {
                self.errors.push(format!(
                    "expected next token to be IDENT, got {:?} instead",
                    t
                ));
                return None;
            }
            None => {
                self.errors
                    .push(format!("expected next token to be IDENT, got None instead"));
                return None;
            }
        };

        let ident = match &self.cur_token {
            Token::IDENT(ident) => ident.to_string(),
            _ => return None,
        };

        match self.lexer.peek() {
            Some(&Token::ASSIGN) => self.next_token(),
            Some(t) => {
                self.errors.push(format!(
                    "expected next token to be ASSIGN, got {:?} instead",
                    t
                ));
                return None;
            }
            None => {
                self.errors.push(format!(
                    "expected next token to be ASSIGN, got None instead"
                ));
                return None;
            }
        };

        //TODO:
        while self.cur_token != Token::SEMICOLON && self.cur_token != Token::EOF {
            self.next_token();
        }

        Some(Statement::Let(
            Identifier(ident.clone()),
            Expression::Ident(Identifier(ident)),
        ))
    }

    fn parse_statement_return(&mut self) -> Option<Statement> {
        self.next_token();

        //TODO:
        while self.cur_token != Token::SEMICOLON && self.cur_token != Token::EOF {
            self.next_token();
        }

        Some(Statement::Return(Expression::Ident(Identifier(
            "TODO".to_string(),
        ))))
    }

    fn parse_statement_expression(&mut self) -> Option<Statement> {
        let expr = match self.parse_expression(Precedence::LOWEST) {
            Some(expr) => expr,
            _ => return None,
        };

        let stmt = Statement::Expression(expr);

        match self.lexer.peek() {
            Some(&Token::SEMICOLON) => self.next_token(),
            _ => {}
        };

        Some(stmt)
    }

    fn parse_expression(&self, precedence: Precedence) -> Option<Expression> {
        match Parser::prefix_parse_fn(&self.cur_token) {
            Some(prefix) => prefix(&self.cur_token),
            _ => None,
        }
    }
}
