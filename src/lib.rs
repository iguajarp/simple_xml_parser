#[derive(Clone, Debug, PartialEq, Eq)]
struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

type ParseResult<'a, Output> = Result<(&'a str, Output), &'a str>;

trait Parser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output>;
}

/// Generic implementation for closures that are implementing Fn(&'a str) -> `ParseResult<Output>`.
/// The implementation gives the default method `parse` to the respectives closures
/// 
/// A closure automatically implements a trait if it matches the signature defined in the impl of the tait.
/// In this case using the Fn(), FnOnce(), FnMut() type. That matches functions and closures.
/// So, if it matches, it'll have the default method defined in this implementations of the Trait,
/// in this case `parse`
impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a str) -> ParseResult<Output>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self(input)
    }
}

/// example of parser
fn _the_letter_a(input: &str) -> Result<(&str, ()), &str> {
    match input.chars().next() {
        // returns the rest of the str slice
        Some('a') => Ok((&input['a'.len_utf8()..], ())),
        _ => Err(input),
    }
}

/// Returns a closure that receives &str and returns a Result<(&str, ()), &str>
fn match_literal<'a>(expected: &'static str) -> impl Parser<'a, ()> {
    // When the pattern matches successfully, the pattern guard expression is executed. If the expression evaluates to true, the pattern is successfully matched against. Otherwise, the next pattern, including other matches with the | operator in the same arm, is tested.
    move |input:&'a str| {
        if input.starts_with(expected) {
            Ok((&input[expected.len()..], ()))
        } else {
            Err(input)
        }
    }
}

/// match valid identifiers, where it starts with an alphabetic value and continue with alphanumeric
/// returns the matched value as a String
fn identifier(input: &str) -> ParseResult<String> {
    // every valid character is pushed to the string
    let mut matched = String::new();
    let mut chars = input.chars();

    // if first element is alphabetic
    match chars.next() {
        Some(next) if next.is_alphabetic() => matched.push(next),
        _ => return Err(input),
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

/// parser combinator, combines two parsers into one
/// receives two closures, and return one closure that returns a tuple of the first and second result
fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, (R1, R2)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input| {
        // and_then is like map but return a new Result
        parser1.parse(input).and_then(|(next_input, result1)| {
            parser2
                .parse(next_input)
                .map(|(last_input, result2)| (last_input, (result1, result2)))
        })
    }
}

fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    F: Fn(A) -> B,
{
    // map is done to the Result from parser
    // we need to call .parse because we don't know if the type P is a function
    move |input| {
        parser
            .parse(input)
            .map(|(next_input, result)| (next_input, map_fn(result)))
    }
}

fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(left, _right)| left)
}

fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(_left, right)| right)
}

#[test]
fn literal_parser() {
    let parse_joe = match_literal("Hello Joe!");
    // remember: if the text and arguments match, returns the rest of the string
    assert_eq!(Ok(("", ())), parse_joe.parse("Hello Joe!"));
    assert_eq!(
        Ok((" Hello Robert!", ())),
        parse_joe.parse("Hello Joe! Hello Robert!")
    );
    assert_eq!(Err("Hello Mike!"), parse_joe.parse("Hello Mike!"));
}

#[test]
fn identifier_parser() {
    assert_eq!(
        Ok(("", "i-am-an-identifier".to_string())),
        identifier.parse("i-am-an-identifier")
    );
    assert_eq!(
        Ok((" entirely an identifier", "not".to_string())),
        identifier.parse("not entirely an identifier")
    );
    assert_eq!(
        Err("!not at all an identifier"),
        identifier.parse("!not at all an identifier")
    );
}

#[test]
fn pair_combinator() {
    // path < then obtain the identifier name
    let tag_opener = pair(match_literal("<"), identifier);
    assert_eq!(
        // match_literal returns nothing, identifier returns the name of the identifier
        Ok(("/>", ((), "my-first-element".to_string()))),
        tag_opener.parse("<my-first-element/>")
    );
    assert_eq!(Err("oops"), tag_opener.parse("oops"));
    assert_eq!(Err("!oops"), tag_opener.parse("<!oops"));
}

#[test]
fn right_combinator() {
    let tag_opener = right(match_literal("<"), identifier);
    assert_eq!(
        Ok(("/>", "my-first-element".to_string())),
        tag_opener.parse("<my-first-element/>")
    );
    assert_eq!(Err("oops"), tag_opener.parse("oops"));
    assert_eq!(Err("!oops"), tag_opener.parse("<!oops"));
}