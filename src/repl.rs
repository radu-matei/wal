use crate::{lexer, token};
use std::io::{stdin, stdout, Write};

const PROMPT: &str = ">>";

pub fn start() {
    loop {
        print!("{} ", PROMPT);
        let buffer = &mut String::new();
        let _ = stdout().flush();
        stdin().read_line(buffer).unwrap();

        let mut l = lexer::Lexer::new(buffer).unwrap();

        loop {
            match l.next_token().unwrap() {
                token::Token::EOF => break,
                x => println!("{:?}", x),
            }
        }
    }
}
