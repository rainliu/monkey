use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    ILLEGAL,
    EOF,

    // Identifiers + literals
    IDENT(String), // add, foobar, x, y, ...
    INT(String),   // 123456

    // Operators
    ASSIGN,
    PLUS,

    // Delimiters
    COMMA,
    SEMICOLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    // Keywords
    FUNCTION,
    LET,
}

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, Token> = {
        let mut map = HashMap::new();
        map.insert("fn", Token::FUNCTION);
        map.insert("let", Token::LET);
        map
    };
}

impl Token {
    pub fn lookup_ident(ident: String) -> Token {
        if let Some(tok) = KEYWORDS.get(ident.as_str()) {
            tok.clone()
        } else {
            Token::IDENT(ident)
        }
    }
}