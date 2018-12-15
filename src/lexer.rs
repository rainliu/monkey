use crate::token::*;
use std::iter::Peekable;
use std::str::Chars;

#[cfg(test)]
mod lexer_test;

struct Lexer<'a> {
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
    match self.read_char() {
      Some('=') => Token::ASSIGN,
      Some(';') => Token::SEMICOLON,
      Some('(') => Token::LPAREN,
      Some(')') => Token::RPAREN,
      Some(',') => Token::COMMA,
      Some('+') => Token::PLUS,
      Some('{') => Token::LBRACE,
      Some('}') => Token::RBRACE,
      Some(ch) => {
        if is_letter(ch) {
          self.read_identifier(ch)
        } else {
          Token::ILLEGAL
        }
      }
      None => Token::EOF,
    }
  }

  fn peek_is_letter(&mut self) -> bool {
    match self.peek_char() {
      Some(&ch) => is_letter(ch),
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
    Token::IDENT(ident)
  }
}

fn is_letter(ch: char) -> bool {
  ch.is_alphabetic() || ch == '_'
}
