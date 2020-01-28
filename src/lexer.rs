use std::str::Chars;
use std::iter::Peekable;

use crate::tokens::Token;

fn is_letter(ch: char) -> bool {
    ch.is_alphabetic() || ch == '_'
}

fn lookup_keyword(s: &str) -> Token {
    match s {
        "true" => Token::LiteralBool(true),
        "false" => Token::LiteralBool(false),

        "and" => Token::And,
        "or" => Token::Or,
        "not" => Token::Not,

        _ => Token::Word(s.to_string())
    }
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
            None => panic!("Unexpected end of input (expected {})", expected)
        }
    }

    fn peek(&mut self) -> Option<&char> {
        return self.input.peek()
    }

    fn skip_whitespace(&mut self) {
        while match self.peek() {
            Some(ch) => ch.is_whitespace(),
            _ => false,
        } {
            self.advance();
        }
    }

    fn read_name(&mut self, ch: char) -> String {
        let mut name = String::new();
        name.push(ch);
        while let Some(c) = self.peek() {
            if is_letter(*c) {
                name.push(self.advance().unwrap())      // safe, because we just peeked it
            } else {
                break;
            }
        }
        return name;
    }

    fn read_number(&mut self, ch: char) -> i64 {
        let mut n = String::new();
        n.push(ch);
        while let Some(c) = self.peek() {
            if c.is_numeric() {
                n.push(self.advance().unwrap())      // safe, because we just peeked it
            } else {
                break;
            }
        }
        return n.parse::<i64>().expect("Error parsing number")
    }

    fn read_str(&mut self) -> String {
        let mut s = String::new();
        while let Some(c) = self.peek() {
            if *c == '"' {           // escaped quotes, what are those?
                self.advance();      // consume the closing "
                break;
            }
            s.push(self.advance().expect("Unterminated string"));
        }
        return s;
    }

    // Returns a plain Token, not Option<Token> or Result,
    // leaving handling Token::EOF or Token::Illegal to the parser
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
                    lookup_keyword(&name)
                } else if ch.is_numeric() {
                    let num = self.read_number(ch);
                    return Token::LiteralInt(num);
                } else {
                    return Token::Illegal(ch);
                }
            }

            None => Token::EOF
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    // So you probably don't need to do the Token::EOF thing here
    // Peekable<Chars> gives you a None when it has run out of chars anyway
    // so in Lexer::next_token you match on that and yield Token::EOF,
    // and then here you match on that and turn it back to a None, which seems redundant
    // also then everything consuming lexer (i.e. parser) has to handle Options
    // but you're supposed to yield None for the Iterator trait to know when to stop
    fn next(&mut self) -> Option<Token> {
        let tok = self.next_token();
        match tok {
            Token::EOF => None,
            _ => Some(tok)
        }
    }
}

#[test]
fn test_lexer() {
    let input = "true and not false + foo * \"bar\" ^ 3";
    let lex = Lexer::new(input);

    let tokens: Vec<Token> = lex.into_iter().collect();


    assert_eq!(tokens, vec![
        Token::LiteralBool(true),
        Token::And,
        Token::Not,
        Token::LiteralBool(false),
        Token::Plus,
        Token::Word("foo".to_string()),
        Token::Star,
        Token::LiteralStr("bar".to_string()),
        Token::Power,
        Token::LiteralInt(3)
    ])
}

#[test]
fn test_lexer_illegal_char() {
    let input = "true and not false + foo ? \"bar\" ^ 3";
    let lex = Lexer::new(input);

    let tokens: Vec<Token> = lex.into_iter().collect();

    assert_eq!(tokens, vec![
        Token::LiteralBool(true),
        Token::And,
        Token::Not,
        Token::LiteralBool(false),
        Token::Plus,
        Token::Word("foo".to_string()),
        Token::Illegal('?'),
        Token::LiteralStr("bar".to_string()),
        Token::Power,
        Token::LiteralInt(3)
    ])
}