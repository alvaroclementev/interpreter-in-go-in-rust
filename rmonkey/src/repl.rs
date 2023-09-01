use std::io::{self, BufRead, BufReader, BufWriter, Read, Write};

use crate::{lexer::Lexer, parser::Parser};

const PROMPT: &str = ">> ";
const MONKEY_FACE: &str = r#"            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"""""""-./, -' /
   ''-' /_   ^ ^   _\ '-''
       |  \._   _./  |
       \   \ '~' /   /
        '._ '-=-' _.'
           '-----'
"#;

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

        let lexer = Lexer::new(buf);
        let mut parser = Parser::new(lexer);

        let program = parser.parse();
        if !parser.errors.is_empty() {
            print_parser_errors(&mut bufwrite, &parser);
            continue;
        }
        writeln!(bufwrite, "{}", program).unwrap();
    }
    Ok(())
}

fn print_parser_errors(output: &mut impl Write, parser: &Parser) {
    writeln!(output, "{}", MONKEY_FACE).unwrap();
    writeln!(output, "Woops! We ran into some monkey business here!").unwrap();
    writeln!(output, " parser errors:").unwrap();
    parser
        .errors
        .iter()
        .try_for_each(|e| writeln!(output, "\t{}", e))
        .expect("printing parser errors to work")
}
