#[cfg(test)]
mod tests;

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
    ($curr_ops : expr,$ops : expr => $operation : expr) => {{
        if let Some(Number(a)) = $curr_ops.pop() {
            if let Some(Number(b)) = $curr_ops.pop() {
                $ops.push(Number($operation(a, b)));
            } else {
                panic!("could not pop two from curr_ops");
            };
        } else {
            panic!("could not pop one from curr_ops");
        };
    }};
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum Token {
    Number(i64),
    Plus,
    Minus,
    Star,
    Slash,
    UpArrow,
    LParen,
    RParen,
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
            ' ' => {}
            digit_c => {
                debug_assert!(digit_c.is_digit(10));
                let mut num_acc = digit_c.to_digit(10).unwrap() as i64;

                while let Some(curr_number) = iter.peek().and_then(|&c| c.to_digit(10)) {
                    num_acc *= 10;
                    num_acc += curr_number as i64;
                    iter.next();
                }

                tokens.push(Number(num_acc));
            }
        }
    }

    tokens
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Operation {
    Number(i64),
    Add,
    Sub,
    Mult,
    Div,
    Exp,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd)]
pub enum Precedence {
    NONE,
    TERM,
    FACTOR,
    EXP,
    CALL,
    MAX,
}

impl Precedence {
    fn next_precedence(&self) -> Precedence {
        match self {
            Precedence::NONE => Precedence::TERM,
            Precedence::TERM => Precedence::FACTOR,
            Precedence::FACTOR => Precedence::EXP,
            Precedence::EXP => Precedence::CALL,
            Precedence::CALL => Precedence::MAX,
            Precedence::MAX => Precedence::MAX,
        }
    }
}

type ParseFn<'a> = dyn Fn(&'a [Token], &mut Vec<Operation>) -> &'a [Token];
type OptParseFn<'a> = Option<&'a ParseFn<'a>>;

struct ParseRule<'a> {
    prefix: OptParseFn<'a>,
    infix: OptParseFn<'a>,
    precedence: Precedence,
}

#[must_use]
fn number<'a>(tokens: &'a [Token], ops: &mut Vec<Operation>) -> &'a [Token] {
    match tokens[0] {
        Token::Number(n) => ops.push(Operation::Number(n)),
        x => panic!("found Token {:?} in call to number<'a>()... ", x),
    }

    &tokens[1..]
}

#[must_use]
fn grouping<'a>(mut tokens: &'a [Token], mut ops: &mut Vec<Operation>) -> &'a [Token] {
    debug_assert_eq!(tokens[0], Token::LParen);
    tokens = parse_precedence(&tokens[1..], &mut ops, Precedence::TERM);

    debug_assert_eq!(tokens[0], Token::RParen);
    &tokens[1..]
}

#[must_use]
fn binary<'a>(mut tokens: &'a [Token], mut ops: &mut Vec<Operation>) -> &'a [Token] {
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

    tokens = parse_precedence(&tokens[1..], &mut ops, next_precedence);

    match curr_token {
        Token::Number(x) => ops.push(Operation::Number(x)),
        Token::Plus => ops.push(Operation::Add),
        Token::Minus => ops.push(Operation::Sub),
        Token::Star => ops.push(Operation::Mult),
        Token::Slash => ops.push(Operation::Div),
        Token::UpArrow => ops.push(Operation::Exp),
        Token::LParen | Token::RParen => unreachable!(),
    }

    tokens
}

#[must_use]
fn token_to_rule<'a>(token: Token) -> ParseRule<'a> {
    use Token::*;

    match token {
        Number(_) => rule!(Some(&number), OptParseFn::None ; Precedence::NONE),

        Plus => rule!(OptParseFn::None, Some(&binary) ; Precedence::TERM),
        Minus => rule!(OptParseFn::None, Some(&binary) ; Precedence::TERM),

        Star => rule!(OptParseFn::None, Some(&binary) ; Precedence::FACTOR),
        Slash => rule!(OptParseFn::None, Some(&binary) ; Precedence::FACTOR),

        UpArrow => rule!(OptParseFn::None, Some(&binary) ; Precedence::EXP),

        LParen => rule!(Some(&grouping), OptParseFn::None ; Precedence::CALL),
        RParen => rule!(OptParseFn::None, OptParseFn::None ; Precedence::NONE),
    }
}

#[must_use]
pub fn parse_precedence<'a>(
    mut tokens: &'a [Token],
    mut ops: &mut Vec<Operation>,
    precedence: Precedence,
) -> &'a [Token] {
    // PREFIX RULE
    let token = tokens[0];
    let rule = token_to_rule(token);

    if let Some(prefix_rule) = &rule.prefix {
        tokens = prefix_rule(&tokens, &mut ops);
    } else {
        panic!(
            "no prefix rule found for token: {:?} (precedence: {:?})",
            &rule.precedence, &token
        );
    }

    // INFIX RULES
    while !tokens.is_empty() {
        let token = tokens[0];
        if precedence > token_to_rule(token).precedence {
            // dong the check here instead of during the while condition has a huge performance impact
            // in my current benchmarks
            break;
        }

        if let Some(infix_rule) = token_to_rule(token).infix {
            tokens = infix_rule(tokens, &mut ops);
        } else {
            panic!("no infix rule found for token: {:?}", token);
        }
    }

    tokens
}

pub fn run(input: &str) -> i64 {
    // println!("{}", input);

    let tokens = tokenize(input);
    // dbg!(&tokens);

    let mut ops = vec![];
    let _ = parse_precedence(&tokens, &mut ops, Precedence::NONE);
    // dbg!(&ops);

    ops.reverse();

    interpret(&mut ops)
    // 0
}

// like parse_precedence, interpret runs right to left thus the arg has to be reversed first
pub fn interpret(ops: &mut Vec<Operation>) -> i64 {
    debug_assert!(!ops.is_empty());
    // dbg!(&ops);

    let mut curr_ops = vec![];

    use Operation::*;
    while let Some(op) = ops.pop() {
        match op {
            Number(x) => curr_ops.push(Number(x)),
            Add => binary_op!(curr_ops,ops => { |a, b| a + b } ),
            Sub => binary_op!(curr_ops,ops => { |a, b| b - a } ),
            Mult => binary_op!(curr_ops,ops => { |a, b| a * b } ),
            Div => binary_op!(curr_ops,ops => { |a, b| b / a } ),
            Exp => binary_op!(curr_ops,ops =>{ |a :i64, b : i64| i64::pow(b,a as u32)} ),
        }
    }

    if let Some(Number(x)) = curr_ops.pop() {
        debug_assert!(curr_ops.is_empty());
        x
    } else {
        panic!("could not interpret");
    }
}
