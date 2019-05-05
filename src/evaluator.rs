use std::io;
use std::io::prelude::*;

use crate::lexer::*;
use crate::token::*;

const PROMPT: &'static str = ">> ";

pub fn run() {
    for line in io::stdin().lock().lines() {
        print!("{}", PROMPT);
        let input = line.unwrap();
        let mut l = Lexer::new(&input);
        loop {
            let tok = l.next_token();
            match tok {
                Token::EOF => break,
                _ => println!("{:?}", tok),
            }
        }
    }
}
