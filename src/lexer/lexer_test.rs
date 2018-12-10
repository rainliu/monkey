use crate::token::*;
use super::*;

#[test]
fn test_next_token() {
  let input = "=+(){},;";
  let tests = vec![Token::ASSIGN, 
    Token::PLUS, 
    Token::LPAREN, 
    Token::RPAREN, 
    Token::LBRACE, 
    Token::RBRACE, 
    Token::COMMA, 
    Token::SEMICOLON,
    Token::EOF,
    ];

  let mut l = Lexer::new(input);
  for test in &tests{
    let tok = l.next_token();
    assert_eq!(tok, *test);
  }
}