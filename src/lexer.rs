use crate::token::*;

use std::iter::Iterator;
use std::iter::Peekable;
use std::str::Chars;

#[cfg(test)]
mod lexer_test;

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &str) -> Lexer {
        Lexer {
            input: input.chars().peekable(),
        }
    }

    pub fn read_char(&mut self) -> Option<char> {
        self.input.next()
    }

    pub fn peek_char(&mut self) -> Option<&char> {
        self.input.peek()
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        match self.read_char() {
            Some('=') => {
                let eq = match self.peek_char() {
                    Some(&ch) => ch == '=',
                    None => false,
                };
                if eq {
                    self.read_char();
                    Token::EQ
                } else {
                    Token::ASSIGN
                }
            }
            Some('+') => Token::PLUS,
            Some('-') => Token::MINUS,
            Some('!') => {
                let neq = match self.peek_char() {
                    Some(&ch) => ch == '=',
                    None => false,
                };
                if neq {
                    self.read_char();
                    Token::NEQ
                } else {
                    Token::BANG
                }
            }
            Some('/') => Token::SLASH,
            Some('*') => Token::ASTERISK,
            Some('<') => Token::LT,
            Some('>') => Token::GT,
            Some(';') => Token::SEMICOLON,
            Some(',') => Token::COMMA,
            Some(':') => Token::COLON,
            Some('(') => Token::LPAREN,
            Some(')') => Token::RPAREN,
            Some('{') => Token::LBRACE,
            Some('}') => Token::RBRACE,
            Some('[') => Token::LBRACKET,
            Some(']') => Token::RBRACKET,
            Some('"') => self.read_string(),
            Some(ch) => {
                if Self::is_letter(ch) {
                    self.read_identifier(ch)
                } else if Self::is_digital(ch) {
                    self.read_number(ch)
                } else {
                    Token::ILLEGAL
                }
            }
            None => Token::EOF,
        }
    }

    fn peek_is_whitespace(&mut self) -> bool {
        match self.peek_char() {
            Some(&ch) => ch.is_whitespace(),
            None => false,
        }
    }

    fn skip_whitespace(&mut self) {
        while self.peek_is_whitespace() {
            self.read_char();
        }
    }

    fn peek_is_letter(&mut self) -> bool {
        match self.peek_char() {
            Some(&ch) => Self::is_letter(ch),
            None => false,
        }
    }

    fn read_identifier(&mut self, ch: char) -> Token {
        let mut ident = String::new();
        ident.push(ch);
        while self.peek_is_letter() {
            match self.read_char() {
                Some(ch) => ident.push(ch),
                None => break,
            };
        }

        Token::lookup_ident(ident)
    }

    fn read_string(&mut self) -> Token {
        let mut string = String::new();
        loop {
            match self.read_char() {
                Some('"') | None => break,
                Some(ch) => string.push(ch),
            };
        }

        Token::STRING(string)
    }

    fn peek_is_number(&mut self) -> bool {
        match self.peek_char() {
            Some(&ch) => Self::is_digital(ch),
            None => false,
        }
    }

    fn read_number(&mut self, ch: char) -> Token {
        let mut number = String::new();
        number.push(ch);
        while self.peek_is_number() {
            match self.read_char() {
                Some(ch) => number.push(ch),
                None => break,
            };
        }

        Token::INT(number)
    }

    fn is_letter(ch: char) -> bool {
        ch.is_alphabetic() || ch == '_'
    }

    fn is_digital(ch: char) -> bool {
        ch.is_digit(10)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        let tok = self.next_token();
        if tok == Token::EOF {
            None
        } else {
            Some(tok)
        }
    }
}
