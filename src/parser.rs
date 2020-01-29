use crate::tokens::Token;
use crate::ast::ASTNode;
use crate::lexer::Lexer;

type ParseResult = Result<ASTNode, ParseError>;
type PrefixFnType = fn(&mut Parser, Token) -> ParseResult;
type InfixFnType = fn(&mut Parser, Token, ASTNode, i32) -> ParseResult;

#[derive(Debug, Clone)]
pub enum ParseError {
    UnexpectedEndOfInput,
    NoPrefixFn(Token),
    NoInfixFn(Token)
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Parse error")
    }
}

impl std::error::Error for ParseError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}


struct ParseRule {
    precedence: i32,
    prefix_fn: Option<PrefixFnType>,
    infix_fn: Option<InfixFnType>
}

impl ParseRule {
    fn prefix_fn(&self, parser: &mut Parser, token: Token) -> ParseResult {
        match self.prefix_fn {
            None => Err(ParseError::NoPrefixFn(token)),
            Some(f) => f(parser, token)
        }
    }

    fn infix_fn(&self, parser: &mut Parser, token: Token, left: ASTNode) -> ParseResult {
        match self.infix_fn {
            None => Err(ParseError::NoInfixFn(token)),
            Some(f) => f(parser, token, left, self.precedence)
        }
    }
}


// These functions take ownership of a Token and give it to an ASTNode
fn literal(_parser: &mut Parser, token: Token) -> ParseResult {
    Ok(ASTNode { token, children: vec![] })
}

fn unary_prefix(parser: &mut Parser, token: Token) -> ParseResult {
    let child = parser.expression(100)?;
    Ok(ASTNode { token, children: vec![child] })
}

fn infix(parser: &mut Parser, token: Token, left: ASTNode, precedence: i32) -> ParseResult {
    let right = parser.expression(precedence)?;
    Ok(ASTNode { token, children: vec![left, right] })
}

fn infix_rassoc(parser: &mut Parser, token: Token, left: ASTNode, precedence: i32) -> ParseResult {
    let right = parser.expression(precedence-1)?;
    Ok(ASTNode { token, children: vec![left, right] })
}

fn get_parse_rule(token: &Token) -> ParseRule {
    return match token {
        Token::LiteralInt(_)  => ParseRule { precedence: 0, prefix_fn: Some(literal), infix_fn: None },
        Token::LiteralStr(_)  => ParseRule { precedence: 0, prefix_fn: Some(literal), infix_fn: None },
        Token::LiteralBool(_) => ParseRule { precedence: 0, prefix_fn: Some(literal), infix_fn: None },
        Token::Word(_)        => ParseRule { precedence: 0, prefix_fn: Some(literal), infix_fn: None },

        Token::Minus    => ParseRule { precedence: 10, prefix_fn: Some(unary_prefix), infix_fn: Some(infix) },
        Token::Plus     => ParseRule { precedence: 10, prefix_fn: None, infix_fn: Some(infix) },
        Token::Star     => ParseRule { precedence: 20, prefix_fn: None, infix_fn: Some(infix) },
        Token::Slash    => ParseRule { precedence: 20, prefix_fn: None, infix_fn: Some(infix) },
        Token::Power    => ParseRule { precedence: 30, prefix_fn: None, infix_fn: Some(infix_rassoc) },

        Token::And      => ParseRule { precedence: 5, prefix_fn: None, infix_fn: Some(infix) },
        Token::Or       => ParseRule { precedence: 5, prefix_fn: None, infix_fn: Some(infix) },
        Token::Not      => ParseRule { precedence: 5, prefix_fn: Some(unary_prefix), infix_fn: None },

        Token::EOF      => ParseRule { precedence: 0, prefix_fn: None, infix_fn: None },

        _ => panic!("No parse rule for {:?}", token)
    }
}

pub struct Parser<'a> {
    // Parser takes ownership of a Lexer, so they must have the same lifetime
    tokens: &'a mut Lexer<'a>,
    current_token: Token
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer<'a>) -> Parser<'a> {
        // We init the parser with a throwaway first token, then immediately advance & discard it.
        let mut p = Parser { tokens: lexer, current_token: Token::Illegal('\0') };
        p.advance();
        p
    }

    pub fn expression(&mut self, precedence: i32) -> Result<ASTNode, ParseError> {
        let mut token = self.advance();

        let mut parsed_so_far = get_parse_rule(&token).prefix_fn(self, token)?;

        while precedence < get_parse_rule(&self.current_token).precedence {
            token = self.advance();
            parsed_so_far = get_parse_rule(&token).infix_fn(self, token, parsed_so_far)?;
        }

        Ok(parsed_so_far)
    }

    fn advance(&mut self) -> Token {
        let t = self.current_token.clone();
        self.current_token = self.tokens.next_token();
        t
    }
}

#[test]
fn test_parser() {
    let input = "true and not false + foo * \"bar\" - 3";
    // parses as: (true and (((not false) + (foo * "bar")) - 3))
    let mut lex = Lexer::new(input);

    let mut p = Parser::new(&mut lex);

    let result = p.expression(0);

    println!("{:#?}", result);
}

#[test]
fn test_parser_error() {
    let input = "true and not false + ";
    let mut lex = Lexer::new(input);

    let mut p = Parser::new(&mut lex);

    let result = p.expression(0);

    println!("{:#?}", result);
}