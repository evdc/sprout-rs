use std::str::Chars;
use std::iter::Peekable;

use crate::token::Token;
use crate::token::TokenType;

fn is_letter(ch: char) -> bool {
    ch.is_alphabetic() || ch == '_'
}

fn lookup_keyword(s: &str) -> TokenType {
    match s {
        "true" => TokenType::LiteralBool(true),
        "false" => TokenType::LiteralBool(false),
        "null"  => TokenType::LiteralNull,

        "and" => TokenType::And,
        "or" => TokenType::Or,
        "not" => TokenType::Not,

        _ => TokenType::Word(s.to_string())
    }
}

#[derive(Debug)]
pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    current_line: u32,
    current_col: u32
}

impl<'a> Lexer<'a> {
    pub fn new(input: &str) -> Lexer {
        Lexer { input: input.chars().peekable(), current_line: 0, current_col: 0 }
    }

    fn advance(&mut self) -> Option<char> {
        return self.input.next()
    }

//    fn consume(&mut self, expected: char) -> Option<char> {
//        let found = self.input.next();
//        match found {
//            Some(c) => {
//                if c == expected {
//                    self.input.next()
//                } else {
//                    panic!("Expected {}, found {}", expected, c)
//                }
//            },
//            None => panic!("Unexpected end of input (expected {})", expected)
//        }
//    }

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

    fn read_number(&mut self, ch: char) -> f64 {
        let mut n = String::new();
        n.push(ch);
        while let Some(c) = self.peek() {
            if c.is_numeric() {
                n.push(self.advance().unwrap())      // safe, because we just peeked it
            } else {
                break;
            }
        }
        return n.parse::<f64>().expect("Error parsing number")
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

        let typ = match self.advance() {
            Some('+') => TokenType::Plus,
            Some('-') => TokenType::Minus,
            Some('*') => TokenType::Star,
            Some('/') => TokenType::Slash,
            Some('^') => TokenType::Power,
            Some('(') => TokenType::LParen,
            Some(')') => TokenType::RParen,

            Some('"') => {
                let s = self.read_str();
                TokenType::LiteralStr(s)
            },

            Some(ch @ _) => {
                if is_letter(ch) {
                    let name = self.read_name(ch);
                    lookup_keyword(&name)
                } else if ch.is_numeric() {
                    let num = self.read_number(ch);
                    TokenType::LiteralNum(num)
                } else {
                    TokenType::Illegal(ch)
                }
            },

            None => TokenType::EOF
        };

        // TODO line/col no don't get incremented, yet
        Token { typ, line: self.current_line, col: self.current_col}
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
        match tok.typ {
            TokenType::EOF => None,
            _ => Some(tok)
        }
    }
}

// Helper fn for constructing these for testing
fn tok(typ: TokenType) -> Token {
    Token { typ, line: 0, col: 0}
}

#[test]
fn test_lexer() {
    let input = "true and not false + foo * \"bar\" ^ 3";
    let lex = Lexer::new(input);

    let tokens: Vec<Token> = lex.into_iter().collect();


    assert_eq!(tokens, vec![
        tok(TokenType::LiteralBool(true)),
        tok(TokenType::And),
        tok(TokenType::Not),
        tok(TokenType::LiteralBool(false)),
        tok(TokenType::Plus),
        tok(TokenType::Word("foo".to_string())),
        tok(TokenType::Star),
        tok(TokenType::LiteralStr("bar".to_string())),
        tok(TokenType::Power),
        tok(TokenType::LiteralNum(3.0))
    ])
}

//#[test]
//fn test_lexer_illegal_char() {
//    let input = "true and not false + foo ? \"bar\" ^ 3";
//    let lex = Lexer::new(input);
//
//    let tokens: Vec<Token> = lex.into_iter().collect();
//
//    assert_eq!(tokens, vec![
//        TokenType::LiteralBool(true),
//        TokenType::And,
//        TokenType::Not,
//        TokenType::LiteralBool(false),
//        TokenType::Plus,
//        TokenType::Word("foo".to_string()),
//        TokenType::Illegal('?'),
//        TokenType::LiteralStr("bar".to_string()),
//        TokenType::Power,
//        TokenType::LiteralNum(3.0)
//    ])
//}