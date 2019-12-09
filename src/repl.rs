use crate::{eval, lexer, object, parser, token};
use std::io::{stdin, stdout, Write};

const PROMPT: &str = ">>";

pub fn start() {
    let mut env = object::Environment::new();

    loop {
        print!("{} ", PROMPT);
        let buffer = &mut String::new();
        let _ = stdout().flush();
        stdin().read_line(buffer).unwrap();

        let l = lexer::Lexer::new(buffer).unwrap();
        let mut p = parser::Parser::new(l).unwrap();

        match p.parse() {
            Ok(pr) => {
                println!("{:?}", pr);
                match eval::eval_program(pr, &mut env) {
                    Ok(obj) => println!("{}", obj),
                    Err(err) => println!("evaluator error: {:?}", err),
                };
            }
            Err(err) => println!("parser error: {:?}", err),
        }

        // loop {
        //     match l.next_token().unwrap() {
        //         token::Token::EOF => break,
        //         x => println!("{:?}", x),
        //     }
        // }
    }
}
