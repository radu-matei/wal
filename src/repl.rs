use crate::{lexer, parser, token};
use std::io::{stdin, stdout, Write};

const PROMPT: &str = ">>";

pub fn start() {
    loop {
        print!("{} ", PROMPT);
        let buffer = &mut String::new();
        let _ = stdout().flush();
        stdin().read_line(buffer).unwrap();

        let l = lexer::Lexer::new(buffer).unwrap();
        let mut p = parser::Parser::new(l).unwrap();

        match p.parse() {
            Ok(pr) => println!("{}", pr),
            Err(err) => println!("{:?}", err),
        }

        // loop {
        //     match l.next_token().unwrap() {
        //         token::Token::EOF => break,
        //         x => println!("{:?}", x),
        //     }
        // }
    }
}
