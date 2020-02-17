use crate::token::Token;
use crate::token::TokenType;
use crate::ast::Expression;
use crate::ast::Statement;
use crate::lexer::Lexer;

type ParseResult = Result<Expression, ParseError>;
type PrefixFnType = fn(&mut Parser, Token) -> ParseResult;
type InfixFnType = fn(&mut Parser, Token, Expression, u8) -> ParseResult;

#[derive(Debug, Clone)]
pub enum ParseError {
    UnexpectedEndOfInput,
    NoPrefixFn(Token),
    NoInfixFn(Token),
    ExpectedIdentifier(Token),
    ExpectedButFound(TokenType, TokenType)
}

struct ParseRule {
    precedence: u8,
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

    fn infix_fn(&self, parser: &mut Parser, token: Token, left: Expression) -> ParseResult {
        match self.infix_fn {
            None => Err(ParseError::NoInfixFn(token)),
            Some(f) => f(parser, token, left, self.precedence)
        }
    }
}

// These functions take ownership of a Token and give it to an Expression
fn literal(_parser: &mut Parser, token: Token) -> ParseResult {
    Ok(Expression::Literal(token))
}

fn unary_prefix(parser: &mut Parser, token: Token) -> ParseResult {
    let child = parser.expression(100)?;
    Ok(Expression::Unary(token, Box::new(child)))
}

fn infix(parser: &mut Parser, token: Token, left: Expression, precedence: u8) -> ParseResult {
    let right = parser.expression(precedence)?;
    Ok(Expression::Infix(token, Box::new(left), Box::new(right)))
}

fn infix_rassoc(parser: &mut Parser, token: Token, left: Expression, precedence: u8) -> ParseResult {
    let right = parser.expression(precedence-1)?;
    Ok(Expression::Infix(token, Box::new(left), Box::new(right)))
}

fn grouping(parser: &mut Parser, _token: Token) -> ParseResult {
    let expr = parser.expression(0)?;
    parser.consume(TokenType::RParen).and(Ok(expr))
}

// todo: instead of None where there is no valid parse rule (a holdover from Python),
// use a parsing function that returns an error; it'll have more context
fn get_parse_rule(token: &Token) -> ParseRule {
    match token.typ {
        // Arithmetic ops
        TokenType::Power    => ParseRule { precedence: 50, prefix_fn: None, infix_fn: Some(infix_rassoc) },
        TokenType::Star     => ParseRule { precedence: 40, prefix_fn: None, infix_fn: Some(infix) },
        TokenType::Slash    => ParseRule { precedence: 40, prefix_fn: None, infix_fn: Some(infix) },
        TokenType::Minus    => ParseRule { precedence: 30, prefix_fn: Some(unary_prefix), infix_fn: Some(infix) },
        TokenType::Plus     => ParseRule { precedence: 30, prefix_fn: None, infix_fn: Some(infix) },

        // Comparisons
        TokenType::Lt       |
        TokenType::LtEq     |
        TokenType::Gt       |
        TokenType::GtEq     => ParseRule { precedence: 20, prefix_fn: None, infix_fn: Some(infix) },
        TokenType::NotEq    |
        TokenType::Eq       => ParseRule { precedence: 15, prefix_fn: None, infix_fn: Some(infix) },

        // Logical ops
        TokenType::And      => ParseRule { precedence: 10, prefix_fn: None, infix_fn: Some(infix) },
        TokenType::Or       => ParseRule { precedence: 10, prefix_fn: None, infix_fn: Some(infix) },
        TokenType::Not      => ParseRule { precedence: 10, prefix_fn: Some(unary_prefix), infix_fn: None },

        // Literals
        TokenType::LiteralNum(_)  |
        TokenType::LiteralStr(_)  |
        TokenType::LiteralBool(_) |
        TokenType::LiteralNull    |
        TokenType::Word(_)        => ParseRule { precedence: 0, prefix_fn: Some(literal), infix_fn: None },

        TokenType::LParen   => ParseRule { precedence: 0, prefix_fn: Some(grouping), infix_fn: None },
        TokenType::RParen   => ParseRule { precedence: 0, prefix_fn: None, infix_fn: None },

        TokenType::EOF      => ParseRule { precedence: 0, prefix_fn: None, infix_fn: None },

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
        let mut p = Parser {
            tokens: lexer,
            current_token: Token { typ: TokenType::Illegal('\0') , line: 0, col: 0 }
        };
        p.advance();
        p
    }

    // =============================================================================================

    pub fn statement(&mut self) -> Result<Statement, ParseError> {
        match self.current_token.typ {
            TokenType::Let => self.let_statement(),
            _ => self.expression(0).map(Statement::Expression)
        }
    }

    fn let_statement(&mut self) -> Result<Statement, ParseError> {
        self.advance();     // Consume 'let'

        // Consume identifier name
        if let TokenType::Word(name) = self.advance().typ {
            self.consume(TokenType::Assign)?;

            let expr = self.expression(0)?;

            Ok(Statement::Assign(name, expr))
        } else {
            Err(ParseError::ExpectedIdentifier(self.current_token.clone()))
        }
    }

    // =============================================================================================

    pub fn expression(&mut self, precedence: u8) -> Result<Expression, ParseError> {
        let mut token = self.advance();

        let mut parsed_so_far = get_parse_rule(&token).prefix_fn(self, token)?;

        while precedence < get_parse_rule(&self.current_token).precedence {
            token = self.advance();
            parsed_so_far = get_parse_rule(&token).infix_fn(self, token, parsed_so_far)?;
        }

        Ok(parsed_so_far)
    }

    // =============================================================================================

    fn advance(&mut self) -> Token {
        let t = self.current_token.clone();
        self.current_token = self.tokens.next_token();
        t
    }

    fn consume(&mut self, expected: TokenType) -> Result<(), ParseError> {
        let found = self.advance();
        if found.typ == expected {
            Ok(())
        } else {
            Err(ParseError::ExpectedButFound(expected, found.typ))
        }
    }

    fn match_next(&mut self, expected: TokenType) -> bool {
        if !self.check(expected) {
            false
        } else {
            self.advance();
            true
        }
    }

    #[inline]
    fn check(&mut self, expected: TokenType) -> bool {
        self.current_token.typ == expected
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
fn test_assignment() {
    let mut lex = Lexer::new("let foo = 3 + 4");
    let mut p = Parser::new(&mut lex);

    let result = p.statement();

    println!("{:#?}", result);
}

#[test]
fn test_grouping() {
    let input = "2 * (3 + 4)";
    let mut lex = Lexer::new(input);

    let mut p = Parser::new(&mut lex);

    let result = p.expression(0);

    println!("{:#?}", result);
}

#[test]
fn test_comparison() {
    let input = "2 < 3 and 5 > 4 == true";
    let mut lex = Lexer::new(input);

    let mut p = Parser::new(&mut lex);

    let result = p.expression(0);

    println!("{:#?}", result);
}

#[test]
fn test_parser_error() {
    let input = "true and not - ";
    let mut lex = Lexer::new(input);

    let mut p = Parser::new(&mut lex);

    let result = p.expression(0);

    println!("{:#?}", result);
}