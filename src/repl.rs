use crate::lexer::*;
use crate::token::*;
use std::io;

const PROMPT: &'static str = ">> ";

pub fn start<R: io::BufRead, W: io::Write>(mut reader: R, mut writer: W) -> io::Result<()> {
    loop {
        writer.write(PROMPT.as_bytes())?;
        writer.flush()?;
        let mut line = String::new();
        reader.read_line(&mut line)?;
        let mut lexer = Lexer::new(&line);
        loop {
            let tok = lexer.next_token();
            match tok {
                Token::EOF => break,
                _ => println!("{:?}", tok),
            }
        }
    }
}
