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

    fn consume(&mut self, expected: char) -> Option<char> {
        let found = self.input.next();
        match found {
            Some(c) => {
                if c == expected {
                    self.input.next()
                } else {
                    panic!("Expected {}, found {}", expected, c)
                }
            },
            None => panic!("Expected {}")
        }
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

    fn read_number(&mut self, ch: char) -> i32 {
        let mut n = String::new();
        n.push(ch);
        while let Some(c) = self.peek_char() {
            if c.is_numeric() {
                n.push(self.advance().unwrap())      // safe, because we just peeked it
            } else {
                break;
            }
        }
        return n.parse::<i32>().expect("Error parsing number")
    }

    fn read_str(&mut self) -> String {
        let mut s = String::new();
        while let Some(c) = self.peek_char() {
            if *c == '"' {           // escaped quotes, what are those?
                self.advance();     // consume the closing "
                break;
            }
            s.push(self.advance().expect("Unterminated string"));
        }
        return s;
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        // n.b. in other implementations, tokens carry information like position and lineno
        match self.advance() {
            Some('+') => Token::Plus,
            Some('-') => Token::Minus,
            Some('*') => Token::Star,
            Some('/') => Token::Slash,
            Some('^') => Token::Power,
            Some('(') => Token::LParen,
            Some(')') => Token::RParen,

            Some('"') => {
                let s = self.read_str();
                return Token::LiteralStr(s);
            }

            Some(ch @ _) => {
                if is_letter(ch) {
                    let name = self.read_name(ch);
                    return Token::Name(name);
                } else if ch.is_numeric() {
                    let num = self.read_number(ch);
                    return Token::LiteralInt(num);
                } else {
                    panic!("Illegal char")
                }
            }

            None => Token::EOF
        }
    }
}