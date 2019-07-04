use crate::evaluator::*;
use crate::lexer::*;
use crate::parser::*;

use std::io;
use std::rc::Rc;

const PROMPT: &'static str = ">> ";

pub fn start<R: io::BufRead, W: io::Write>(mut reader: R, mut writer: W) -> io::Result<()> {
    let env = Environment::new();

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

        match eval(&program, Rc::clone(&env)) {
            Ok(evaluated) => writer.write(format!("{}\n", evaluated).as_bytes())?,
            Err(err) => writer.write(format!("error: {}\n", err).as_bytes())?,
        };
    }
}
