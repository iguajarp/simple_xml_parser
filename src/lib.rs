#[derive(Clone, Debug, PartialEq, Eq)]
struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

/// example of parser
fn the_letter_a(input: &str) -> Result<(&str, ()), &str> {
    match input.chars().next() {
        // returns the rest of the str slice
        Some('a') => Ok((&input['a'.len_utf8()..], ())),
        _ => Err(input),
    }
}

/// Returns a function that receives &str and returns a Result<(&str, ()), &str>
fn match_literal(expected: &'static str) -> impl Fn(&str) -> Result<(&str, ()), &str> {
    // When the pattern matches successfully, the pattern guard expression is executed. If the expression evaluates to true, the pattern is successfully matched against. Otherwise, the next pattern, including other matches with the | operator in the same arm, is tested.
    move |input| match input.get(0..expected.len()) {
        Some(next) if next == expected => Ok((&input[expected.len()..], ())),
        _ => Err(input),
    }
}

#[test]
fn literal_parser() {
    let parse_joe = match_literal("Hello Joe!");
    // remember: if the text and arguments match, returns the rest of the string
    assert_eq!(
        Ok(("", ())),
        parse_joe("Hello Joe!")
    );
    assert_eq!(
        Ok((" Hello Robert!", ())),
        parse_joe("Hello Joe! Hello Robert!")
    );
    assert_eq!(
        Err("Hello Mike!"),
        parse_joe("Hello Mike!")
    );
}