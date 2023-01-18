use crate::compiler::lexer::Lexer;
use crate::compiler::token::{Token, TokenType};
use crate::compiler::expression::*;
use crate::compiler::codegen2::Compiler;
use crate::vm::vm::VM;
use crate::vm::value::Value;

type Precedence = i16;
type ParseResult = Result<Expression, ParseError>;
type PrefixFnType = fn(&mut Parser, Token) -> ParseResult;
type InfixFnType = fn(&mut Parser, Token, Expression, Precedence) -> ParseResult;

#[derive(Debug, Clone)]
pub enum ParseError {
    UnexpectedEndOfInput,
    NoPrefixFn(Token),
    NoInfixFn(Token),
    ExpectedIdentifier(Token),
    ExpectedButFound(Vec<TokenType>, TokenType)
}

struct ParseRule {
    precedence: Precedence,
    prefix_fn: PrefixFnType,
    infix_fn: InfixFnType
}

// These functions take ownership of a Token and give it to an Expression

fn prefix_error(_parser: &mut Parser, token: Token) -> ParseResult {
    Err(ParseError::NoPrefixFn(token))
}

fn infix_error(_parser: &mut Parser, token: Token, _left: Expression, _precedence: Precedence) -> ParseResult {
    Err(ParseError::NoInfixFn(token))
}

fn literal(_parser: &mut Parser, token: Token) -> ParseResult {
    Ok(Expression::literal(token))
}

fn unary_prefix(parser: &mut Parser, token: Token) -> ParseResult {
    let value = parser.expression(100)?;
    Ok(Expression::unary(token, value))
}

// essentially the same shape as UnaryPrefix but a closing quote (for now)
fn quoted(parser: &mut Parser, token: Token) -> ParseResult {
    let subexpr = parser.expression(0)?;
    parser.consume(TokenType::Quote)?;
    Ok(Expression::quoted(token, subexpr))
}

fn infix(parser: &mut Parser, token: Token, left: Expression, precedence: Precedence) -> ParseResult {
    let right = parser.expression(precedence)?;
    Ok(Expression::binary(token, left, right))
}

fn infix_rassoc(parser: &mut Parser, token: Token, left: Expression, precedence: Precedence) -> ParseResult {
    let right = parser.expression(precedence-1)?;
    Ok(Expression::binary(token, left, right))
}

fn grouping(parser: &mut Parser, token: Token) -> ParseResult {
    // Matches a ( in prefix position; in addition to defining a group, this may also define a tuple
    let expr = parser.expression(0)?;
    let mut exprs = vec![expr];
    while parser.check(&TokenType::Comma) & !parser.check(&TokenType::EOF) {
        parser.advance();   // eat the comma
        let expr = parser.expression(0)?;
        exprs.push(expr);
    }
    parser.consume(TokenType::RParen)?;

    // hacky?: if no commas found, return a single expression
    match exprs.len() {
        1 => Ok(exprs.remove(0)),   // note we could do this unsafely because we *just* matched len
        _ => Ok(Expression::tuple(token, exprs))
    }
}

fn var_declaration(parser: &mut Parser, token: Token) -> ParseResult {
    // Consume identifier name, '=', and expression. (Variables must be initialized)
    let target = parser.expression(0)?;
    parser.consume(TokenType::Assign)?;
    let value = parser.expression(0)?;
    Ok(Expression::assign(token, target, value))
}

fn conditional(parser: &mut Parser, token: Token) -> ParseResult {
    // token is the "if" token here
    let condition_expr = parser.expression(0)?;
    parser.consume(TokenType::Then)?;
    let true_expr = parser.expression(0)?;
    println!("{:#?}", parser.current_token);
    let false_expr = if parser.check(&TokenType::Else) {
        parser.advance();
        Some(parser.expression(0)?)
    } else {
        None
    };
    Ok(Expression::conditional(token, condition_expr, true_expr, false_expr))
}

fn arrow_func(parser: &mut Parser, token: Token, left: Expression, _precedence: Precedence) -> ParseResult {
    // Left is the arguments, which parses as either a literal (name) or a tuple
    let args = match left {
        Expression::Literal(_) => vec![left],
        Expression::Tuple(args) => args.items,
        _ => { return Err(ParseError::ExpectedIdentifier(token)); }     // todo: this actually points to the wrong token
    };
    let body = parser.statement()?;         // this can be a block or a single expr
    Ok(Expression::function(token, "func".to_string(), args, body))

}

fn for_expr(_parser: &mut Parser, _token: Token) -> ParseResult {
    // let results = for NAME in EXPR do STATEMENTS end ???
    todo!("need to add lists before we can use for-exprs");
}

fn eval_expr(parser: &mut Parser, token: Token) -> ParseResult {
    let subexpr = expr_to_block(parser.expression(100)?);
    println!("EVAL: Will evaluate: {:#?}", subexpr);
    let code = parser.compiler.compile(subexpr).unwrap();   // todo err handler
    println!("EVAL: Will run: {:?}", code);
    let value = parser.vm.run(code).unwrap();              // todo err handler
    
    // Now convert the result back into an Expression
    // Annoyingly for now we just use the line/col of the $ token not the inner-expr
    let result_expr = value_to_expr(value, token.line, token.col);
    Ok(result_expr)
}

fn value_to_expr(v: Value, line: u32, col: u32) -> Expression {
    // So we turn the value *back* into an expression (only to eval it back into a value later!)
    match v {
        Value::Null => Expression::literal(Token { typ: TokenType::LiteralNull, line, col }),
        Value::Bool(b) => Expression::literal(Token { typ: TokenType::LiteralBool(b), line, col }),
        Value::Num(n) => Expression::literal(Token { typ: TokenType::LiteralNum(n), line, col }),
        Value::Str(s) => Expression::literal(Token { typ: TokenType::LiteralStr(s), line, col }),

        // Expression::function expects a Statement for the body, but Value::Function has a Code for the body
        // we don't want to decompile it, so ... Function objs keep a ref to their original expr around somehow??
        // maybe if we did eval-macro in Compiler not Parser, we just use the resulting Function's code?
        Value::Function(f) => todo!("need to figure this out"),

        // If it's a quoted expr then just pull out the inner expr ?
        Value::Expression(expr) => *expr
    }
}


fn get_parse_rule(token: &Token) -> ParseRule {
    match token.typ {
        // Arithmetic ops
        TokenType::Power    => ParseRule { precedence: 50, prefix_fn: prefix_error, infix_fn: infix_rassoc },
        TokenType::Star     => ParseRule { precedence: 40, prefix_fn: prefix_error, infix_fn: infix },
        TokenType::Slash    => ParseRule { precedence: 40, prefix_fn: prefix_error, infix_fn: infix },
        TokenType::Minus    => ParseRule { precedence: 30, prefix_fn: unary_prefix, infix_fn: infix },
        TokenType::Plus     => ParseRule { precedence: 30, prefix_fn: prefix_error, infix_fn: infix },

        // Comparisons
        TokenType::Lt       |
        TokenType::LtEq     |
        TokenType::Gt       |
        TokenType::GtEq     => ParseRule { precedence: 20, prefix_fn: prefix_error, infix_fn: infix },
        TokenType::NotEq    |
        TokenType::Eq       => ParseRule { precedence: 15, prefix_fn: prefix_error, infix_fn: infix },

        // Logical ops
        TokenType::And      => ParseRule { precedence: 10, prefix_fn: prefix_error, infix_fn: infix },
        TokenType::Or       => ParseRule { precedence: 10, prefix_fn: prefix_error, infix_fn: infix },
        TokenType::Not      => ParseRule { precedence: 10, prefix_fn: unary_prefix, infix_fn: infix_error },

        // Literals
        TokenType::LiteralNum(_)  |
        TokenType::LiteralStr(_)  |
        TokenType::LiteralBool(_) |
        TokenType::LiteralNull    => ParseRule { precedence: 0, prefix_fn: literal, infix_fn: infix_error },

        TokenType::Let      => ParseRule { precedence: 0, prefix_fn: var_declaration, infix_fn: infix_error },
        TokenType::Name(_)  => ParseRule { precedence: 0, prefix_fn: literal, infix_fn: infix_error },

        // If is an expression
        TokenType::If       => ParseRule { precedence: 0, prefix_fn: conditional, infix_fn: infix_error },

        TokenType::Arrow    => ParseRule { precedence: 5, prefix_fn: prefix_error, infix_fn: arrow_func},     // todo: what's the precedence?

        // for expressions...?
        TokenType::For      => ParseRule { precedence: 0, prefix_fn: for_expr, infix_fn: infix_error },

        // Macro-expansion / eval
        TokenType::Quote    => ParseRule { precedence: 0, prefix_fn: quoted, infix_fn: infix_error },
        TokenType::Eval     => ParseRule { precedence: 0, prefix_fn: eval_expr, infix_fn: infix_error },

        TokenType::LParen   => ParseRule { precedence: 0, prefix_fn: grouping, infix_fn: infix_error },

        // Ignored tokens, whose parse rule should never be invoked.
        // if it is top (+inf) here it means we always hit the error but also even for TokenType::Illegal('\n') which we should fix
        _ => ParseRule { precedence: -1, prefix_fn: prefix_error, infix_fn: infix_error }
    }
}


pub struct Parser<'a> {
    // Parser takes ownership of a Lexer, so they must have the same lifetime
    tokens: &'a mut Lexer<'a>,
    current_token: Token,

    // also borows a compiler and VM in order to compile-time execute
    // Compiler and VM ought to outlive the Parser
    pub compiler: &'a mut Compiler,
    pub vm: &'a mut VM
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer<'a>, compiler: &'a mut Compiler, vm: &'a mut VM) -> Parser<'a> {
        // We init the parser with a throwaway first token, then immediately advance & discard it.
        let mut p = Parser {
            tokens: lexer,
            current_token: Token { typ: TokenType::Illegal('\0') , line: 0, col: 0 },
            compiler,
            vm
        };
        p.advance();
        p
    }

    pub fn parse(input: &str, compiler: &mut Compiler, vm: &mut VM) -> Result<Statement, ParseError> {
        let mut l = Lexer::new(input);
        let mut p = Parser::new(&mut l, compiler, vm);

        // Parse a sequence of statements aka a block.
        p.block()
    }

    // =============================================================================================

    pub fn statement(&mut self) -> Result<Statement, ParseError> {
        println!("Statement: {:?}", self.current_token.typ);
        let res = if self.check(&TokenType::LBrace) {
            self.advance(); // eat the brace
            let block = self.block()?;
            self.consume(TokenType::RBrace)?;
            Ok(block)
        } else {
            let expr = self.expression(0)?;
            Ok(Statement::Expression(Box::new(expr)))
        };
        println!("/Statement");
        res
    }

    pub fn block(&mut self) -> Result<Statement, ParseError> {
        println!("Block: {:?}", self.current_token.typ);
        let mut statements: Vec<Box<Statement>> = Vec::new();
        while !self.check(&TokenType::EOF) && !self.check(&TokenType::RBrace) {
            let st = self.statement()?;
            self.consume(TokenType::Semicolon)?;
            self.consume_many(TokenType::Newline);
            statements.push(Box::new(st));
        }
        println!("/Block");
        Ok(Statement::Block(statements))
    }

    pub fn expression(&mut self, precedence: Precedence) -> Result<Expression, ParseError> {
        println!("Expression: {:?}", self.current_token.typ);
        let mut token = self.advance();

        let mut parsed_so_far = (get_parse_rule(&token).prefix_fn)(self, token.clone())?;

        // println!("{:?} {:?} {:?}", token.typ, self.current_token.typ, parsed_so_far);
        while precedence < get_parse_rule(&self.current_token).precedence {
            token = self.advance();
            parsed_so_far = (get_parse_rule(&token).infix_fn)(self, token, parsed_so_far, precedence)?;
        }
        println!("/Expression");
        Ok(parsed_so_far)
    }

    // =============================================================================================

    fn advance(&mut self) -> Token {
        let t = self.current_token.clone();
        self.current_token = self.tokens.next_token();
        t
    }

    fn consume(&mut self, expected: TokenType) -> Result<Token, ParseError> {
        let found = self.advance();
        if found.typ == expected {
            Ok(found)
        } else {
            Err(ParseError::ExpectedButFound(vec![expected], found.typ))
        }
    }

//    fn consume_one_of(&mut self, expected: Vec<TokenType>) -> Result<Token, ParseError> {
//        let found = self.advance();
//        if expected.contains(&found.typ) {
//            Ok(found)
//        } else {
//            Err(ParseError::ExpectedButFound(expected, found.typ))
//        }
//    }

    fn consume_many(&mut self, expected: TokenType) {
        // Consume zero or more of a token type, without returning them
        while self.check(&expected) {
            self.advance();
        }
    }

//    fn advance_if(&mut self, expected: TokenType) -> bool {
//        if !self.check(&expected) {
//            false
//        } else {
//            self.advance();
//            true
//        }
//    }

    #[inline]
    fn check(&mut self, expected: &TokenType) -> bool {
        self.current_token.typ == *expected
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

    let result = p.expression(0);

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

#[test]
fn test_conditional() {
    let input = "let foo = if true and 1 == 2 then \"wut\" else \"okay\";";

    let result = Parser::parse(input);

    println!("{:#?}", result);
}