use std::iter::Peekable;

use crate::ast::*;
use crate::lexer::*;
use crate::token::*;

#[cfg(test)]
mod parser_test;

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Parser {
            lexer: lexer.peekable(),
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program::new();

        while self.lexer.peek() != None && self.lexer.peek() != Some(&Token::EOF) {
            let stmt = self.parse_statement();
            if let Some(stmt) = stmt {
                program.statements.push(stmt);
            }
        }

        program
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.lexer.peek() {
            Some(&Token::LET) => self.parse_statement_let(),
            _ => None,
        }
    }

    fn parse_statement_let(&mut self) -> Option<Statement> {
        self.lexer.next().unwrap();
        let ident = match self.lexer.peek() {
            Some(&Token::IDENT(_)) => self.lexer.next().unwrap(),
            _ => return None,
        };
        match self.lexer.peek() {
            Some(&Token::ASSIGN) => self.lexer.next(),
            _ => return None,
        };

        //TODO:
        while let Some(t) = self.lexer.next() {
            if t == Token::SEMICOLON {
                break;
            }
        }

        Some(Statement::Let(
            Identifier { token: ident.clone() },
            Expression::Ident(Identifier { token: ident }),
        ))
    }
}
