use std::iter::Peekable;

use crate::ast::*;
use crate::lexer::*;
use crate::token::*;

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Parser {
            lexer: lexer.peekable(),
        }
    }

    pub fn parse_program(&mut self) -> Program<impl Statement> {
        let mut program = Program::new();

        while self.lexer.peek() != None && self.lexer.peek() != Some(&Token::EOF) {
            let stmt = self.parse_statement();
            if let Some(stmt) = stmt {
                program.statements.push(stmt);
            }
        }

        program
    }

    fn parse_statement(&mut self) -> Option<impl Statement> {
        match self.lexer.peek() {
            Some(&Token::LET) => self.parse_statement_let(),
            _ => None,
        }
    }

    fn parse_statement_let(&mut self) -> Option<impl Statement> {
        let token = self.lexer.next().unwrap();
        let name = match self.lexer.peek() {
            Some(&Token::IDENT(_)) => self.lexer.next().unwrap(),
            _ => return None,
        };
        match self.lexer.peek() {
            Some(&Token::ASSIGN) => self.lexer.next(),
            _ => return None,
        };

        Some(LetStatement {
            token,
            name: Identifier { token: name },
            //value: ,
        })
    }
}
