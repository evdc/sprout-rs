use std::str::Chars;
use std::iter::Peekable;

use crate::tokens::Token;

fn is_letter(ch: char) -> bool {
    ch.is_alphabetic() || ch == '_'
}

#[derive(Debug)]
pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>
}

impl<'a> Lexer<'a> {
    pub fn new(input: &str) -> Lexer {
        Lexer { input: input.chars().peekable() }
    }

    fn advance(&mut self) -> Option<char> {
        return self.input.next()
    }

    fn peek_char(&mut self) -> Option<&char> {
        return self.input.peek()
    }

    fn skip_whitespace(&mut self) {
        while match self.peek_char() {
            Some(ch) => ch.is_whitespace(),
            _ => false,
        } {
            self.advance();
        }
    }

    fn read_name(&mut self, ch: char) -> String {
        let mut name = String::new();
        name.push(ch);
        while let Some(c) = self.peek_char() {
            if is_letter(*c) {
                name.push(self.advance().unwrap())      // safe, because we just peeked it
            } else {
                break;
            }
        }
        return name;
    }

    fn read_number(&mut self, ch: char) -> String {
        let mut n = String::new();
        n.push(ch);
        while let Some(c) = self.peek_char() {
            if c.is_numeric() {
                n.push(self.advance().unwrap())      // safe, because we just peeked it
            } else {
                break;
            }
        }
        // return n.parse::<i32>().expect("Error parsing number")
        return n;
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        // n.b. in other implementations, tokens carry information like position and lineno
        match self.advance() {
            Some('+') => Token::Plus,
            Some('-') => Token::Minus,
            Some('*') => Token::Star,
            Some('/') => Token::Slash,

            Some(ch @ _) => {
                if is_letter(ch) {
                    let name = self.read_name(ch);
                    return Token::Name(name);
                } else if ch.is_numeric() {
                    let num = self.read_number(ch);
                    return Token::Literal(num);
                } else {
                    panic!("Illegal char")
                }
            }

            None => Token::EOF
        }
    }
}