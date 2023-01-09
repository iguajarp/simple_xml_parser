#[derive(Clone, Debug, PartialEq, Eq)]
struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

/// example of parser
fn _the_letter_a(input: &str) -> Result<(&str, ()), &str> {
    match input.chars().next() {
        // returns the rest of the str slice
        Some('a') => Ok((&input['a'.len_utf8()..], ())),
        _ => Err(input),
    }
}

/// Returns a function that receives &str and returns a Result<(&str, ()), &str>
fn match_literal(expected: &'static str) -> impl Fn(&str) -> Result<(&str, ()), &str> {
    // When the pattern matches successfully, the pattern guard expression is executed. If the expression evaluates to true, the pattern is successfully matched against. Otherwise, the next pattern, including other matches with the | operator in the same arm, is tested.
    move |input| {
        if input.starts_with(expected) {
            Ok((&input[expected.len()..], ()))
        } else {
            Err(input)
        }
    }
}


/// match valid identifiers, where it starts with an alphabetic value and continue with alphanumeric
/// returns the matched value as a String
fn identifier(input: &str) -> Result<(&str, String), &str> {
    // every valid character is pushed to the string
    let mut matched = String::new();
    let mut chars = input.chars();

    // if first element is alphabetic
    match chars.next() {
        Some(next) if next.is_alphabetic() => matched.push(next),
        _ =>  return Err(input),
    }

    while let Some(next) = chars.next() {
        if next.is_alphanumeric() || next == '-' {
            matched.push(next);
        } else {
            break;
        }
    }

    let next_index = matched.len();
    Ok((&input[next_index..], matched))
}

#[test]
fn literal_parser() {
    let parse_joe = match_literal("Hello Joe!");
    // remember: if the text and arguments match, returns the rest of the string
    assert_eq!(Ok(("", ())), parse_joe("Hello Joe!"));
    assert_eq!(
        Ok((" Hello Robert!", ())),
        parse_joe("Hello Joe! Hello Robert!")
    );
    assert_eq!(Err("Hello Mike!"), parse_joe("Hello Mike!"));
}
