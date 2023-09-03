use std::{
    cell::RefCell,
    io::{self, BufRead, BufReader, BufWriter, Read, Write},
    rc::Rc,
};

use crate::{
    evaluator::{self, Environment},
    lexer::Lexer,
    parser::Parser,
};

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
    let environment = Rc::new(RefCell::new(Environment::new()));

    loop {
        write!(bufwrite, "{}", PROMPT)?;
        bufwrite.flush().unwrap();

        // Read
        let mut buf = String::new();
        bufread.read_line(&mut buf)?;
        if buf.is_empty() {
            // Read an EOF
            writeln!(bufwrite, "Good bye!")?;
            break;
        }

        // Strip the newline at the end
        let buf = if buf.ends_with('\n') {
            buf.strip_suffix('\n').unwrap()
        } else {
            &buf
        };

        if buf == ".exit" {
            writeln!(bufwrite, "Good bye!")?;
            break;
        }

        // Parse
        let lexer = Lexer::new(buf.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse();
        if !parser.errors.is_empty() {
            print_parser_errors(&mut bufwrite, &parser);
            continue;
        }

        // Eval
        let result = evaluator::eval(&program, environment.clone());

        // Print
        writeln!(bufwrite, "{}", result).unwrap();
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
