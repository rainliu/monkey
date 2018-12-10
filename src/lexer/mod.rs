use crate::token::*;
use std::str::Chars;
use std::iter::Peekable;

#[cfg(test)]
mod lexer_test;

struct Lexer<'a>{
  input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a>{
  pub fn new(input:&str)->Lexer{
    Lexer{input: input.chars().peekable()}
  }

  pub fn read_char(&mut self) -> Option<char>{
    self.input.next()
  }

  pub fn peek_char(&mut self) ->Option<&char> {
    self.input.peek()
  }

  pub fn next_token(&mut self) -> Token {
    match self.read_char() {
      Some('=') => Token::ASSIGN,
      Some(';') => Token::SEMICOLON,
      Some('(') => Token::LPAREN,
      Some(')') => Token::RPAREN,
      Some(',') => Token::COMMA,
      Some('+') => Token::PLUS,
      Some('{') => Token::LBRACE,
      Some('}') => Token::RBRACE,
      Some(_) => Token::ILLEGAL,
      None => Token::EOF,
    }
  }
}