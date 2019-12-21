use lang_lib::*;

fn main() {
    std::env::set_var("RUST_BACKTRACE", "full");
    color_backtrace::install();

    assert!(Precedence::NONE < Precedence::TERM);

    let input = "10*2+3";
    assert_eq!(run(input), 10 * 2 + 3);

    let input = "293474*4^5-50";
    assert_eq!(run(input), 300_517_376 - 50);
}
