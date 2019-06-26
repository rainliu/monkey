use std::iter::Peekable;

use crate::ast::*;
use crate::lexer::*;
use crate::token::*;

#[cfg(test)]
mod parser_test;

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

    pub fn errors(&self) ->&[String] {
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
            _ => None,
        }
    }

    fn parse_statement_let(&mut self) -> Option<Statement> {
        match self.lexer.peek() {
            Some(&Token::IDENT(_)) => self.next_token(),
            Some(t) => {
                self.errors.push(format!("expected next token to be IDENT, got {:?} instead", t));
                return None
            },
            None => {
                self.errors.push(format!("expected next token to be IDENT, got None instead"));
                return None
            },
        };

        let ident = self.cur_token.clone();

        match self.lexer.peek() {
            Some(&Token::ASSIGN) => self.next_token(),
            Some(t) => {
                self.errors.push(format!("expected next token to be ASSIGN, got {:?} instead", t));
                return None
            },
            None => {
                self.errors.push(format!("expected next token to be ASSIGN, got None instead"));
                return None
            },
        };

        //TODO:
        while self.cur_token != Token::SEMICOLON && self.cur_token != Token::EOF {
            self.next_token();
        }

        Some(Statement::Let(
            Identifier { token: ident.clone() },
            Expression::Ident(Identifier { token: ident }),
        ))
    }

    fn parse_statement_return(&mut self) -> Option<Statement> {
        self.next_token();

        //TODO:
        while self.cur_token != Token::SEMICOLON && self.cur_token != Token::EOF {
            self.next_token();
        }

        Some(Statement::Return(Expression::Ident(Identifier { token: Token::RETURN })))
    }

    fn next_token(&mut self) {
        if let Some(token)  = self.lexer.next() {
            self.cur_token = token;
        }else {
            self.cur_token = Token::EOF;
        }
    }
}
