use super::*;

#[test]
fn test() {
    assert!(Precedence::NONE < Precedence::TERM);

    let input = "10*2+3";
    assert_eq!(run(input), 10 * 2 + 3);

    // test that newlines work
    let input = "10+2
    *3";
    assert_eq!(run(input), 10 + 2 * 3);

    let input = "1*2+3*4+5-19*8/2";
    assert_eq!(run(input), 1 * 2 + 3 * 4 + 5 - 19 * 8 / 2);

    let input = "2*3+3+4+1";
    assert_eq!(run(input), 2 * 3 + 3 + 4 + 1);

    let input = "8/2";
    assert_eq!(run(input), 8 / 2);

    let input = "1-2-3";
    assert_eq!(run(input), -4);

    let input = "2^5";
    let val = run(input);
    assert_eq!(val, 32);

    let input = "2^3+3+4+1*9";
    let val = run(input);
    dbg!(val);
    assert_eq!(val, 24);

    let input = "2*4^5";
    assert_eq!(run(input), 2048);

    let input = "293474*4^5-50";
    assert_eq!(run(input), 300517376 - 50);

    let input = "10^(2+3)";
    assert_eq!(run(input), 100000);

    let input = "10*(2+3)";
    assert_eq!(run(input), 10 * (2 + 3));

    let input = "4^2^3";
    use std::convert::TryInto;
    assert_eq!(run(input), i64::pow(4, i64::pow(2, 3).try_into().unwrap()));
}
