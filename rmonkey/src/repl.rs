use std::io::{self, BufRead, BufReader, BufWriter, Read, Write};

use crate::{lexer::Lexer, token::TokenKind};

const PROMPT: &str = ">> ";

pub fn start(input: impl Read, output: impl Write) -> io::Result<()> {
    let mut bufread = BufReader::new(input);
    let mut bufwrite = BufWriter::new(output);

    loop {
        write!(bufwrite, "{}", PROMPT)?;
        bufwrite.flush().unwrap();

        let mut buf = String::new();
        bufread.read_line(&mut buf)?;
        // Strip the newline at the end
        let buf = buf[..buf.len() - 1].to_string();

        if buf == ".exit" {
            writeln!(bufwrite, "Good bye!")?;
            break;
        }

        let mut lexer = Lexer::new(buf);

        loop {
            let token = lexer.next_token();
            match token.kind {
                TokenKind::Eof => break,
                _ => writeln!(bufwrite, "{:?}", token)?,
            }
        }
    }
    Ok(())
}
