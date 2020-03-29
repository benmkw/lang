use lang_lib::*;

fn main() {
    // used for debugging test failures
    std::env::set_var("RUST_BACKTRACE", "full");
    color_backtrace::install();

    assert!(Precedence::NONE < Precedence::TERM);

    let input = "10+
    2*3";

    let input = "asdf = 58;
    asdf
    ";

    // assert_eq!(run(input), 10 + 2 * 3);

    // let input = "293474*4^5-50";
    let tokens = tokenize(input);
    dbg!(tokens);

    // let mut ops = vec![];
    // let _ = parse_precedence(&tokens, &mut ops, Precedence::NONE);
    // dbg!(&ops);

    // let res = interpret(&ops);
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
