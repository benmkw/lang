// https://users.rust-lang.org/t/takewhile-iterator-over-chars-to-string-slice/11014/2
use itertools::*;
use std::collections;

#[cfg(test)]
mod tests;

// https://stackoverflow.com/a/28392068
macro_rules! hashmap {
    ($( $key: expr => $val: expr ),*) => {{
         let mut map= ::std::collections::HashMap::new();
         $( map.insert($key, $val); )*
         map
    }}
}

macro_rules! rule {
    ($prefix : expr, $infix : expr ; $precedence : expr) => {{
        ParseRule {
            prefix: match $prefix {
                Some(prefix) => Some(Box::new(prefix)),
                None => None,
            },
            infix: match $infix {
                Some(infix) => Some(Box::new(infix)),
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

    let mut tokens: Vec<Token> = Vec::new();

    let mut iter = s.chars();
    while let Some(c) = iter.clone().peekable().peek() {
        if c.to_string().parse::<i64>().is_ok() {
            let num: String = iter
                .by_ref()
                .take_while_ref(|&c| c.to_string().parse::<i64>().is_ok())
                .collect();

            if let Ok(num) = num.parse::<i64>() {
                tokens.push(Number(num));
            } else {
                panic!("{:?} {}", num, "could not handle the number\n");
            }
        }

        // take_while has advanced over the + sign here already,
        // we would need to call iter.curr() or something like that but alas it does not exist??
        // https://docs.rs/itertools/0.8.0/itertools/trait.Itertools.html#method.peeking_take_while solves this
        if let Some(c) = iter.next() {
            match c {
                '+' => tokens.push(Plus),
                '-' => tokens.push(Minus),
                '*' => tokens.push(Star),
                '/' => tokens.push(Slash),
                '^' => tokens.push(UpArrow),
                '(' => tokens.push(LParen),
                ')' => tokens.push(RParen),
                val => panic!("{}  case not handled", val),
            }
        }
    }

    tokens
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum MapToken {
    Number,
    Plus,
    Minus,
    Star,
    Slash,
    UpArrow,
    LParen,
    RParen,
}

impl Token {
    fn to_map_token(self) -> MapToken {
        use Token::*;
        match self {
            Number(_) => MapToken::Number,
            Plus => MapToken::Plus,
            Minus => MapToken::Minus,
            Star => MapToken::Star,
            Slash => MapToken::Slash,
            UpArrow => MapToken::UpArrow,
            LParen => MapToken::LParen,
            RParen => MapToken::RParen,
        }
    }
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

type RuleMap = collections::HashMap<MapToken, ParseRule>;

type ParseFn =
    Box<dyn Fn(&RuleMap, &[Token], &mut [Operation], &Token) -> (Vec<Token>, Vec<Operation>)>;
type OptParseFn = Option<ParseFn>;

struct ParseRule {
    prefix: OptParseFn,
    infix: OptParseFn,
    precedence: Precedence,
}

fn number(
    _: &RuleMap,
    tokens: &[Token],
    ops: &mut [Operation],
    curr_token: &Token,
) -> (Vec<Token>, Vec<Operation>) {
    let mut ops = ops.to_vec();

    match curr_token {
        Token::Number(n) => ops.push(Operation::Number(*n)),
        x => panic!("{:?}  no number found in number parse fn", x),
    }

    (tokens.to_vec(), ops)
}

fn grouping(
    rules: &RuleMap,
    tokens: &[Token],
    ops: &mut [Operation],
    curr_token: &Token,
) -> (Vec<Token>, Vec<Operation>) {
    assert_eq!(curr_token, &Token::LParen);

    let mut tokens = tokens.to_vec();
    let mut ops = ops.to_vec();

    let (new_tokens, new_ops) = parse_precedence(&rules, &tokens, &mut ops, Precedence::TERM);
    tokens = new_tokens;
    ops = new_ops;

    assert_eq!(tokens.pop(), Some(Token::RParen));
    (tokens.to_vec(), ops.to_vec())
}

fn binary(
    rules: &RuleMap,
    tokens: &[Token],
    ops: &mut [Operation],
    curr_token: &Token,
) -> (Vec<Token>, Vec<Operation>) {
    let mut tokens = tokens.to_vec();
    let mut ops = ops.to_vec();

    let rule = &rules[&curr_token.to_map_token()];
    let precedence = &rule.precedence;

    let next_precedence = match curr_token {
        Token::Number(_) | Token::Plus | Token::Minus | Token::Star | Token::Slash => {
            precedence.next_precedence() // left associative
        }
        Token::UpArrow => precedence.clone(), // right associative
        _ => unreachable!(),
    };

    let (new_tokens, new_ops) = parse_precedence(&rules, &tokens, &mut ops, next_precedence);
    tokens = new_tokens;
    ops = new_ops;

    match curr_token {
        Token::Number(x) => ops.push(Operation::Number(*x)),
        Token::Plus => ops.push(Operation::Add),
        Token::Minus => ops.push(Operation::Sub),
        Token::Star => ops.push(Operation::Mult),
        Token::Slash => ops.push(Operation::Div),
        Token::UpArrow => ops.push(Operation::Exp),
        _ => unreachable!(),
    }

    (tokens.to_vec(), ops.to_vec())
}

// uses .pop and .last on vector thus operates right to left thus the elements get reversed first
fn parse_precedence(
    rules: &RuleMap,
    tokens: &[Token],
    ops: &mut [Operation],
    precedence: Precedence,
) -> (Vec<Token>, Vec<Operation>) {
    let mut tokens = tokens.to_vec();
    let mut ops = ops.to_vec();

    // dbg!(&ops);

    // PREFIX RULE
    assert!(!tokens.is_empty());

    let token = tokens.pop().unwrap();
    let rule = &rules[&token.to_map_token()];

    if let Some(prefix_rule) = &rule.prefix {
        let (new_tokens, new_ops) = prefix_rule(&rules, &tokens, &mut ops, &token);
        tokens = new_tokens;
        ops = new_ops;
    } else {
        panic!(
            "no suitable prefix rule found: {:?} for token: {:?}",
            &rule.precedence, &token
        );
    }

    // INFIX RULES
    while !tokens.is_empty() {
        if precedence > rules[&tokens.last().unwrap().to_map_token()].precedence {
            break;
        }

        let token = tokens.pop().unwrap();

        if let Some(infix_rule) = &rules[&token.to_map_token()].infix {
            let (new_tokens, new_ops) = infix_rule(&rules, &tokens, &mut ops, &token);
            tokens = new_tokens;
            ops = new_ops;
        } else {
            panic!(
                "expected to find infix rule but did not find one for token: {:?}",
                &token
            );
        }
    }

    (tokens.to_vec(), ops.to_vec())
}

pub fn run(input: &str) -> i64 {
    // println!("{}", input);
    use MapToken::*;
    let rules = hashmap![
    Number => rule!(Some(number), OptParseFn::None ; Precedence::NONE),

    Plus => rule!(OptParseFn::None, Some(binary) ; Precedence::TERM),
    Minus => rule!(OptParseFn::None, Some(binary) ; Precedence::TERM),

    Star => rule!(OptParseFn::None, Some(binary) ; Precedence::FACTOR),
    Slash => rule!(OptParseFn::None, Some(binary) ; Precedence::FACTOR),

    UpArrow => rule!(OptParseFn::None, Some(binary) ; Precedence::EXP),

    LParen => rule!(Some(grouping), OptParseFn::None ; Precedence::CALL),
    RParen => rule!(OptParseFn::None, OptParseFn::None ; Precedence::NONE)
    ];

    let mut tokens: Vec<Token> = tokenize(input);
    // dbg!(&tokens);

    let tokens = &mut tokens[..];
    tokens.reverse();
    let tokens = tokens.to_vec();

    let mut ops = Vec::<Operation>::new();
    let (_, mut ops) = parse_precedence(&rules, &tokens, &mut ops, Precedence::NONE);
    // dbg!(&ops);

    ops[..].reverse();

    // print!("DONE running\n\n\n");
    interpret(&mut ops)
}

// like parse_precedence, interpret runs right to left thus the arg has to be reversed first
pub fn interpret(ops: &mut [Operation]) -> i64 {
    assert!(!ops.is_empty());
    let mut ops = ops.to_vec();
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
