use std::iter::Peekable;

use crate::ast::*;
use crate::lexer::*;
use crate::token::*;

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    //fn new(lexer: &'a mut Lexer) -> Parser<'a> {
    //    Parser {
    //        lexer: lexer.iter().peakable(),
    //    }
    //}
}

