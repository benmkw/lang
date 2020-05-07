use lang_lib::*;

fn main() {
    // used for debugging test failures
    std::env::set_var("RUST_BACKTRACE", "full");
    color_backtrace::install();

    assert!(Precedence::NONE < Precedence::TERM);

    // let input = "10+
    // 2*3";

    // let input = "asdf = 58 + 2";
    // let input = "a = 58 + 2";
    // let input = "1-2-3";
    let input = "-1";
    // let input = "1+2+3";
    // let input = "-4^2+2"; // 18 ok

    // assert_eq!(run(input), 10 + 2 * 3);

    // let input = "293474*4^5-50";
    let tokens = tokenize(input);
    // dbg!(&tokens);

    let (expr, tokens) = parse_precedence(&tokens, Precedence::NONE);

    // dbg!(&expr);
    assert!(tokens.is_empty());
    dbg!(obj::interpret(&expr));
    // dbg!(run(input));

    // let res = interpret(&expr);
    // dbg!(res);

    // dbg!(run(input));
    // assert_eq!(run(input), 300_517_376 - 50);

    // // used for cargo flamegraph
    // let base_input = "29*4^5-50";
    // let strlen = base_input.chars().count() + 1;

    // let size = 1_000;
    // let input: String = std::iter::repeat(format!("{}+", base_input))
    //     .take(size)
    //     .collect();
    // let input = format!("{}{}", input, base_input);
    // let byte_size = size * strlen - 1;
    // for _ in 1..100000 {
    //     run(&input);
    // }
}
