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
enum Token {
    Number(i64),
    Plus,
    Minus,
    Star,
    Slash,
    UpArrow,
    LParen,
    RParen,
}

fn tokenize(s: &str) -> Vec<Token> {
    use Token::*;

    let mut tokens = vec![];

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
                assert!(digit_c.is_digit(10));
                let mut num_acc = digit_c.to_digit(10).unwrap() as i64;

                while let Some(curr_number) = iter.peek().and_then(|c| c.to_digit(10)) {
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

type ParseFn = dyn Fn(&mut Vec<Token>, &mut Vec<Operation>, &Token);
type OptParseFn<'a> = Option<&'a ParseFn>;

struct ParseRule<'a> {
    prefix: OptParseFn<'a>,
    infix: OptParseFn<'a>,
    precedence: Precedence,
}

fn number(_: &mut Vec<Token>, ops: &mut Vec<Operation>, curr_token: &Token) {
    match curr_token {
        Token::Number(n) => ops.push(Operation::Number(*n)),
        x => panic!("{:?} no number found in number parse fn", x),
    }
}

fn grouping(mut tokens: &mut Vec<Token>, mut ops: &mut Vec<Operation>, curr_token: &Token) {
    assert_eq!(curr_token, &Token::LParen);

    parse_precedence(&mut tokens, &mut ops, Precedence::TERM);

    let token = tokens.pop();
    assert_eq!(token, Some(Token::RParen));
}

fn binary(mut tokens: &mut Vec<Token>, mut ops: &mut Vec<Operation>, curr_token: &Token) {
    let rule = token_to_rule(&curr_token);
    let precedence = &rule.precedence;

    let next_precedence = match curr_token {
        Token::Number(_) | Token::Plus | Token::Minus | Token::Star | Token::Slash => {
            precedence.next_precedence() // left associative
        }
        Token::UpArrow => precedence.clone(), // right associative
        _ => unreachable!(),
    };

    parse_precedence(&mut tokens, &mut ops, next_precedence);

    match curr_token {
        Token::Number(x) => ops.push(Operation::Number(*x)),
        Token::Plus => ops.push(Operation::Add),
        Token::Minus => ops.push(Operation::Sub),
        Token::Star => ops.push(Operation::Mult),
        Token::Slash => ops.push(Operation::Div),
        Token::UpArrow => ops.push(Operation::Exp),
        Token::LParen | Token::RParen => unreachable!(),
    }
}

fn token_to_rule(token: &Token) -> ParseRule {
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

// uses .pop and .last on vector thus operates right to left thus the elements get reversed first
fn parse_precedence(
    mut tokens: &mut Vec<Token>,
    mut ops: &mut Vec<Operation>,
    precedence: Precedence,
) {
    // dbg!(&ops);

    // PREFIX RULE
    assert!(!tokens.is_empty());

    let token = tokens.pop().unwrap();
    let rule = token_to_rule(&token);

    if let Some(prefix_rule) = &rule.prefix {
        prefix_rule(&mut tokens, &mut ops, &token);
    } else {
        panic!(
            "no suitable prefix rule found: {:?} for token: {:?}",
            &rule.precedence, &token
        );
    }

    // INFIX RULES
    while !tokens.is_empty() {
        if precedence > token_to_rule(&tokens.last().unwrap()).precedence {
            break;
        }

        let token = tokens.pop().unwrap();

        if let Some(infix_rule) = token_to_rule(&token).infix {
            infix_rule(&mut tokens, &mut ops, &token);
        } else {
            panic!(
                "expected to find infix rule but did not find one for token: {:?}",
                token
            );
        }
    }
}

pub fn run(input: &str) -> i64 {
    // println!("{}", input);

    let mut tokens = tokenize(input);
    // dbg!(&tokens);

    tokens.reverse();

    let mut ops = vec![];
    parse_precedence(&mut tokens, &mut ops, Precedence::NONE);
    // dbg!(&ops);

    ops.reverse();

    // print!("DONE running\n\n\n");
    interpret(&mut ops)
}

// like parse_precedence, interpret runs right to left thus the arg has to be reversed first
pub fn interpret(ops: &mut Vec<Operation>) -> i64 {
    assert!(!ops.is_empty());
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
        assert!(curr_ops.is_empty());
        x
    } else {
        panic!("could not interpret");
    }
}
