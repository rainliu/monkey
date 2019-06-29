use crate::lexer::*;
use crate::parser::*;

use std::io;

const PROMPT: &'static str = ">> ";

pub fn start<R: io::BufRead, W: io::Write>(mut reader: R, mut writer: W) -> io::Result<()> {
    loop {
        writer.write(PROMPT.as_bytes())?;
        writer.flush()?;
        let mut line = String::new();
        reader.read_line(&mut line)?;
        let l = Lexer::new(&line);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        let errors = p.errors();
        if errors.len() != 0 {
            for error in errors {
                writer.write(format!("\t{}\t\n", error).as_bytes())?;
            }
            continue;
        }
        writer.write(format!("{}\n", program).as_bytes())?;
    }
}
