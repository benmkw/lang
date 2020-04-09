#![feature(box_patterns)]
#![warn(
    clippy::restriction,
    clippy::cargo,
    clippy::all,
    clippy::pedantic,
    clippy::nursery,
    clippy::clone_on_ref_ptr
)]
#![allow(
    clippy::shadow_unrelated,
    clippy::cognitive_complexity,
    clippy::too_many_lines,
    clippy::use_debug,
    clippy::dbg_macro,
    clippy::print_stdout,
    clippy::missing_docs_in_private_items,
    clippy::implicit_return,
    clippy::option_unwrap_used,
    clippy::enum_glob_use
)]
use once_cell::sync::Lazy;
use std::sync::Mutex;

#[cfg(test)]
mod tests;

mod ast;
pub use ast::Expr;

mod jit;
pub use jit::interpret_ast_jit;

static INTERNS: Lazy<Mutex<Vec<String>>> = Lazy::new(|| Mutex::new(vec![]));

macro_rules! rule {
    ($prefix : expr, $infix : expr ; $precedence : expr) => {{
        ParseRule {
            prefix: match $prefix {
                Some(prefix) => Some(prefix),
                None => None,
            },
            infix: match $infix {
                Some(infix) => Some(infix),
                None => None,
            },
            precedence: $precedence,
        }
    }};
}

macro_rules! binary_op {
    ($stack : expr => $operation : expr) => {{
        let first = $stack.pop().unwrap();
        let second = $stack.pop().unwrap();

        match (first, second) {
            (Number(a), Number(b)) => {
                $stack.push(Number($operation(b, a)));
            }
            _ => unreachable!(),
        }
    }};
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Token {
    Number(i64),
    Plus,
    Minus,
    Star,
    Slash,
    UpArrow,
    LParen,
    RParen,
    Eq,
    Identifier(StringID),
    Semi,
}

#[derive(Copy, Clone, PartialEq)]
pub struct StringID {
    id: usize,
}

impl std::fmt::Debug for StringID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "StringID id {:?}, String {:?}",
            self.id,
            self.get_string()
        )
    }
}

impl StringID {
    fn get_string(self) -> String {
        INTERNS.lock().unwrap()[self.id].to_string()
    }
}

fn intern_string(s: &str) -> StringID {
    let mut interns = INTERNS.lock().unwrap();
    let ok = interns.iter().position(|elem| elem == &s);

    match ok {
        Some(id) => StringID { id },
        None => {
            interns.push(s.to_string());
            let id = interns.len() - 1;
            StringID { id }
        }
    }
}

pub fn tokenize(s: &str) -> Vec<Token> {
    use Token::*;

    let mut tokens = vec![];

    // could skip utf-8 validation and work on bytes but not clear if its really worth it
    // benchmarks show different results, change is very easy though
    let mut iter = s.chars().peekable();
    while let Some(c) = iter.next() {
        match c {
            '+' => tokens.push(Plus),
            '-' => tokens.push(Minus),
            '*' => tokens.push(Star),
            '/' => tokens.push(Slash),
            '^' => tokens.push(UpArrow),
            '(' => tokens.push(LParen),
            ')' => tokens.push(RParen),
            '=' => tokens.push(Eq),
            ';' => tokens.push(Semi),
            ' ' | '\n' | '\r' => {}
            digit_c if digit_c.is_digit(10) => {
                debug_assert!(digit_c.is_digit(10));
                let mut num_acc = digit_c.to_digit(10).unwrap() as i64;

                while let Some(curr_number) = iter.peek().and_then(|&c| c.to_digit(10)) {
                    num_acc *= 10;
                    num_acc += curr_number as i64;
                    iter.next();
                }

                tokens.push(Number(num_acc));
            }
            c => {
                let mut ident = String::new();
                ident.push(c);
                while let Some(curr_char) = iter.peek() {
                    if curr_char.is_alphanumeric() {
                        ident.push(*curr_char);
                        iter.next();
                    } else {
                        break;
                    }
                }

                let id = intern_string(&ident);
                tokens.push(Identifier(id));
            }
        }
    }

    tokens
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Operation {
    Number(i64),
    Add,
    Sub,
    Mult,
    Div,
    Exp,
    Assign,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Precedence {
    NONE,
    ASSIGN,
    TERM,
    FACTOR,
    EXP,
    NEG,
    CALL,
    MAX,
}

impl Precedence {
    fn next_precedence(&self) -> Precedence {
        match self {
            Precedence::NONE => Precedence::ASSIGN,
            Precedence::ASSIGN => Precedence::TERM,
            Precedence::TERM => Precedence::FACTOR,
            Precedence::FACTOR => Precedence::EXP,
            Precedence::EXP => Precedence::NEG,
            Precedence::NEG => Precedence::CALL,
            Precedence::CALL => Precedence::MAX,
            Precedence::MAX => Precedence::MAX,
        }
    }
}

type ParseFn<'a> = dyn Fn(&'a [Token], &Option<Expr>) -> (Expr, &'a [Token]);
type OptParseFn<'a> = Option<&'a ParseFn<'a>>;

struct ParseRule<'a> {
    prefix: OptParseFn<'a>,
    infix: OptParseFn<'a>,
    precedence: Precedence,
}

#[must_use]
fn number<'a>(tokens: &'a [Token], _: &Option<Expr>) -> (Expr, &'a [Token]) {
    match tokens[0] {
        Token::Number(n) => {
            return (Expr::Number(n), &tokens[1..]);
        }
        x => panic!("found Token {:?} in call to number<'a>()... ", x),
    }
}

#[must_use]
fn identifier<'a>(tokens: &'a [Token], _: &Option<Expr>) -> (Expr, &'a [Token]) {
    match tokens[0] {
        Token::Identifier(id) => {
            return (Expr::Identifier(id), &tokens[1..]);
        }
        x => panic!("found Token {:?} in call to identifier<'a>()... ", x),
    }
}

#[must_use]
fn grouping<'a>(tokens: &'a [Token], _: &Option<Expr>) -> (Expr, &'a [Token]) {
    debug_assert_eq!(tokens[0], Token::LParen);
    let (expr_rhs, tokens) = parse_precedence(&tokens[1..], Precedence::TERM);

    debug_assert_eq!(tokens[0], Token::RParen);
    (expr_rhs, &tokens[1..])
}

#[must_use]
fn negate<'a>(tokens: &'a [Token], _: &Option<Expr>) -> (Expr, &'a [Token]) {
    debug_assert_eq!(tokens[0], Token::Minus);

    let (expr_rhs, tokens) = parse_precedence(&tokens[1..], Precedence::NEG);

    (Expr::Negate(Box::new(expr_rhs)), tokens)
}

#[must_use]
fn assign<'a>(tokens: &'a [Token], expr_lhs: &Option<Expr>) -> (Expr, &'a [Token]) {
    let curr_token = tokens[0];

    let rule = token_to_rule(curr_token);
    let (expr_rhs, tokens) = parse_precedence(&tokens[1..], rule.precedence);

    (
        Expr::Assign(Box::new((expr_lhs.clone().unwrap(), expr_rhs))),
        tokens,
    )
}

#[must_use]
fn binary<'a>(tokens: &'a [Token], expr_lhs: &Option<Expr>) -> (Expr, &'a [Token]) {
    let curr_token = tokens[0];

    let rule = token_to_rule(curr_token);
    let precedence = rule.precedence;
    let next_precedence = match curr_token {
        Token::Number(_) | Token::Plus | Token::Minus | Token::Star | Token::Slash => {
            precedence.next_precedence() // left associative
        }
        Token::UpArrow => precedence, // right associative
        _ => unreachable!(),
    };

    let (expr_rhs, tokens) = parse_precedence(&tokens[1..], next_precedence);
    // dbg!(&expr_rhs);

    let ret_expr = match curr_token {
        Token::Plus => Expr::Add(Box::new((expr_lhs.clone().unwrap(), expr_rhs))),
        Token::Minus => Expr::Sub(Box::new((expr_lhs.clone().unwrap(), expr_rhs))),
        Token::Star => Expr::Mult(Box::new((expr_lhs.clone().unwrap(), expr_rhs))),
        Token::Slash => Expr::Div(Box::new((expr_lhs.clone().unwrap(), expr_rhs))),
        Token::UpArrow => Expr::Exp(Box::new((expr_lhs.clone().unwrap(), expr_rhs))),
        Token::Eq => todo!(),
        Token::Identifier(_s) => todo!(),
        Token::Semi => todo!(),
        Token::Number(_) | Token::LParen | Token::RParen => unreachable!(),
    };

    (ret_expr, tokens)
}

#[must_use]
fn token_to_rule<'a>(token: Token) -> ParseRule<'a> {
    use Token::*;

    match token {
        // TokenType       prefix rule      infix rule       precedence (infix only)
        Number(_) => rule!(Some(&number), OptParseFn::None ; Precedence::NONE),

        Plus => rule!(OptParseFn::None, Some(&binary) ; Precedence::TERM),
        Minus => rule!(Some(&negate), Some(&binary) ; Precedence::TERM),

        Star => rule!(OptParseFn::None, Some(&binary) ; Precedence::FACTOR),
        Slash => rule!(OptParseFn::None, Some(&binary) ; Precedence::FACTOR),

        UpArrow => rule!(OptParseFn::None, Some(&binary) ; Precedence::EXP),

        LParen => rule!(Some(&grouping), OptParseFn::None ; Precedence::CALL),
        RParen => rule!(OptParseFn::None, OptParseFn::None ; Precedence::NONE),

        Eq => rule!(OptParseFn::None, Some(&assign) ; Precedence::ASSIGN),
        Identifier(_) => rule!(Some(&identifier), OptParseFn::None ; Precedence::NONE),
        Semi => rule!(OptParseFn::None, OptParseFn::None ; Precedence::NONE),
    }
}

// 3*2+5
// 3 2 * 5 +
#[must_use]
pub fn parse_precedence<'a>(tokens: &'a [Token], precedence: Precedence) -> (Expr, &'a [Token]) {
    // PREFIX RULE
    let rule = token_to_rule(tokens[0]);
    if let Some(prefix_rule) = &rule.prefix {
        let (mut expr_lhs, mut tokens) = prefix_rule(&tokens, &None);

        // INFIX RULES
        while !tokens.is_empty() {
            let token = tokens[0];
            if precedence > token_to_rule(token).precedence {
                // dong the check here instead of during the while condition has a huge performance impact
                // in my current benchmarks
                break;
            }

            if let Some(infix_rule) = token_to_rule(token).infix {
                let (expr_top, new_tokens) = infix_rule(tokens, &Some(expr_lhs));
                tokens = new_tokens;
                expr_lhs = expr_top;
            } else {
                panic!("no infix rule found for token: {:?}", token);
            }
        }

        return (expr_lhs.clone(), tokens);
    } else {
        panic!(
            "no prefix rule found for token: {:?} (precedence: {:?})",
            &rule.precedence, &tokens[0]
        );
    };
}

#[must_use]
pub fn run(input: &str) -> i64 {
    let tokens = tokenize(input);

    let (expr, tokens) = parse_precedence(&tokens, Precedence::NONE);
    debug_assert!(tokens.is_empty());
    // dbg!(&expr);

    // interpret_ast(&expr)
    interpret_ast_jit(&expr)
}

#[must_use]
pub fn interpret_ast(expr: &Expr) -> i64 {
    match expr {
        Expr::Number(x) => *x,
        Expr::Add(box (lhs, rhs)) => interpret_ast(lhs) + interpret_ast(rhs),
        Expr::Sub(box (lhs, rhs)) => interpret_ast(lhs) - interpret_ast(rhs),
        Expr::Mult(box (lhs, rhs)) => interpret_ast(lhs) * interpret_ast(rhs),
        Expr::Div(box (lhs, rhs)) => interpret_ast(lhs) / interpret_ast(rhs),
        Expr::Exp(box (a, b)) => i64::pow(interpret_ast(a), interpret_ast(b) as u32),
        Expr::Negate(val) => -interpret_ast(val),
        Expr::Assign(_) | Expr::Block(_) | Expr::Identifier(_) => todo!(),
    }
}

#[must_use]
pub fn interpret(ops: &[Operation]) -> i64 {
    debug_assert!(!ops.is_empty());

    let mut stack = vec![];

    use Operation::*;
    for op in ops {
        match op {
            Number(x) => stack.push(Number(*x)),
            Add => binary_op!(stack => {|a, b| a + b}),
            Sub => binary_op!(stack => {|a, b| a - b}),
            Mult => binary_op!(stack => { |a, b| a * b } ),
            Div => binary_op!(stack => { |a, b| a / b } ),
            Exp => binary_op!(stack =>{ |a :i64, b : i64| i64::pow(a,b as u32)} ),
            Assign => todo!(),
        }
    }

    if let Some(Number(x)) = stack.pop() {
        debug_assert!(stack.is_empty());
        x
    } else {
        panic!("could not interpret");
    }
}
