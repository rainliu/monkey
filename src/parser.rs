use std::iter::Peekable;

use crate::ast::*;
use crate::lexer::*;
use crate::token::*;

#[cfg(test)]
mod parser_test;

type PrefixParseFn = fn(parser: &mut Parser) -> Option<Expression>;
type InfixParseFn = fn(parser: &mut Parser, left: Expression) -> Option<Expression>;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
enum Precedence {
    LOWEST = 0,
    EQUALS,      // ==
    LESSGREATER, // > or <
    SUM,         // +
    PRODUCT,     // *
    PREFIX,      // -X or !X
    CALL,        // myFunc(X)
}

#[inline]
fn token2precedence(token: &Token) -> Precedence {
    match token {
        Token::EQ | Token::NEQ => Precedence::EQUALS,
        Token::LT | Token::GT => Precedence::LESSGREATER,
        Token::PLUS | Token::MINUS => Precedence::SUM,
        Token::SLASH | Token::ASTERISK => Precedence::PRODUCT,
        _ => Precedence::LOWEST,
    }
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

    pub fn errors(&self) -> &[String] {
        &self.errors
    }

    #[inline]
    fn prefix_parse_fn(token: &Token) -> Option<PrefixParseFn> {
        match token {
            Token::IDENT(_) => Some(Parser::parse_indentifier),
            Token::INT(_) => Some(Parser::parse_integer),
            Token::BANG | Token::MINUS => Some(Parser::parse_prefix),
            _ => None,
        }
    }

    #[inline]
    fn infix_parse_fn(token: &Token) -> Option<InfixParseFn> {
        match token {
            Token::EQ
            | Token::NEQ
            | Token::LT
            | Token::GT
            | Token::PLUS
            | Token::MINUS
            | Token::SLASH
            | Token::ASTERISK => Some(Parser::parse_infix),
            _ => None,
        }
    }

    fn parse_indentifier(parser: &mut Parser) -> Option<Expression> {
        match &parser.cur_token {
            Token::IDENT(ident) => Some(Expression::Ident(Identifier(ident.to_string()))),
            _ => None,
        }
    }

    fn parse_integer(parser: &mut Parser) -> Option<Expression> {
        match &parser.cur_token {
            Token::INT(int) => match int.parse::<i64>() {
                Ok(i) => Some(Expression::Int(Integer(i))),
                _ => None,
            },
            _ => None,
        }
    }

    fn parse_prefix(parser: &mut Parser) -> Option<Expression> {
        let prefix = match &parser.cur_token {
            Token::BANG => Prefix::BANG,
            Token::MINUS => Prefix::MINUS,
            _ => return None,
        };

        parser.next_token();

        if let Some(right) = parser.parse_expression(Precedence::PREFIX) {
            Some(Expression::Prefix(prefix, Box::new(right)))
        } else {
            None
        }
    }

    fn parse_infix(parser: &mut Parser, left: Expression) -> Option<Expression> {
        let infix = match &parser.cur_token {
            Token::PLUS => Infix::PLUS,
            Token::MINUS => Infix::MINUS,
            Token::ASTERISK => Infix::ASTERISK,
            Token::SLASH => Infix::SLASH,
            Token::LT => Infix::LT,
            Token::GT => Infix::GT,
            Token::EQ => Infix::EQ,
            Token::NEQ => Infix::NEQ,
            _ => return None,
        };

        let cur_precedence = token2precedence(&parser.cur_token);
        parser.next_token();

        if let Some(right) = parser.parse_expression(cur_precedence) {
            Some(Expression::Infix(Box::new(left), infix, Box::new(right)))
        } else {
            None
        }
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

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let opt_left = match Parser::prefix_parse_fn(&self.cur_token) {
            Some(prefix) => prefix(self),
            _ => {
                self.errors.push(format!(
                    "no prefix parse function for {:?} found",
                    self.cur_token
                ));
                None
            }
        };

        if let Some(mut left) = opt_left {
            while let Some(peek_token) = self.lexer.peek() {
                if *peek_token != Token::SEMICOLON && precedence < token2precedence(peek_token) {
                    let infix = match Parser::infix_parse_fn(peek_token) {
                        Some(infix) => infix,
                        _ => break,
                    };

                    self.next_token();

                    let expr = infix(self, left);
                    if expr.is_none() {
                        return None;
                    }

                    left = expr.unwrap();
                } else {
                    break;
                }
            }
            Some(left)
        } else {
            None
        }
    }
}
