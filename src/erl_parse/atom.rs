use nom::error::{FromExternalError, ParseError};

/// Parse a backslash, followed by any amount of whitespace. This is used later
/// to discard any escaped whitespace.
fn parse_escaped_whitespace<'a, E: ParseError<&'a str>>(
  input: &'a str,
) -> nom::IResult<&'a str, &'a str, E> {
  nom::sequence::preceded(nom::character::complete::char('\\'),
                          nom::character::complete::multispace1)(input)
}

/// Parse a unicode sequence, of the form u{XXXX}, where XXXX is 1 to 6
/// hexadecimal numerals. We will combine this later with parse_escaped_char
/// to parse sequences like \u{00AC}.
fn pqa_parse_unicode<'a, E>(input: &'a str) -> nom::IResult<&'a str, char, E>
  where
      E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
{
  // `take_while_m_n` parses between `m` and `n` bytes (inclusive) that match
  // a predicate. `parse_hex` here parses between 1 and 6 hexadecimal numerals.
  let parse_hex = nom::bytes::complete::take_while_m_n(1, 6, |c: char| c.is_ascii_hexdigit());

  // `preceeded` takes a prefix parser, and if it succeeds, returns the result
  // of the body parser. In this case, it parses u{XXXX}.
  let parse_delimited_hex = nom::sequence::preceded(
    nom::character::complete::char('u'),
    // `delimited` is like `preceded`, but it parses both a prefix and a suffix.
    // It returns the result of the middle parser. In this case, it parses
    // {XXXX}, where XXXX is 1 to 6 hex numerals, and returns XXXX
    nom::sequence::delimited(
      nom::character::complete::char('{'),
      parse_hex,
      nom::character::complete::char('}')),
  );

  // `map_res` takes the result of a parser and applies a function that returns
  // a Result. In this case we take the hex bytes from parse_hex and attempt to
  // convert them to a u32.
  let parse_u32 = nom::combinator::map_res(
    parse_delimited_hex,
    move |hex| u32::from_str_radix(hex, 16));

  // map_opt is like map_res, but it takes an Option instead of a Result. If
  // the function returns None, map_opt returns an error. In this case, because
  // not all u32 values are valid unicode code points, we have to fallibly
  // convert to char with from_u32.
  nom::combinator::map_opt(parse_u32, std::char::from_u32)(input)
}

/// Parse an escaped character: \n, \t, \r, \u{00AC}, etc.
fn pqa_parse_escaped_char<'a, E>(input: &'a str) -> nom::IResult<&'a str, char, E>
  where
      E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
{
  nom::sequence::preceded(
    nom::character::complete::char('\\'),
    // `alt` tries each parser in sequence, returning the result of
    // the first successful match
    nom::branch::alt((
      pqa_parse_unicode,
      // The `value` parser returns a fixed value (the first argument) if its
      // parser (the second argument) succeeds. In these cases, it looks for
      // the marker characters (n, r, t, etc) and returns the matching
      // character (\n, \r, \t, etc).
      nom::combinator::value('\n', nom::character::complete::char('n')),
      nom::combinator::value('\r', nom::character::complete::char('r')),
      nom::combinator::value('\t', nom::character::complete::char('t')),
      nom::combinator::value('\u{08}', nom::character::complete::char('b')),
      nom::combinator::value('\u{0C}', nom::character::complete::char('f')),
      nom::combinator::value('\\', nom::character::complete::char('\\')),
      nom::combinator::value('/', nom::character::complete::char('/')),
      nom::combinator::value('"', nom::character::complete::char('"')),
      nom::combinator::value('\'', nom::character::complete::char('\'')),
    )),
  )(input)
}

/// Parse a non-empty block of text that doesn't include \ or "
fn pqa_parse_literal<'a, E: ParseError<&'a str>>(input: &'a str) -> nom::IResult<&'a str, &'a str, E> {
  // `is_not` parses a string of 0 or more characters that aren't one of the
  // given characters.
  let not_quote_slash = nom::bytes::complete::is_not("\'\\");

  // `verify` runs a parser, then runs a verification function on the output of
  // the parser. The verification function accepts out output only if it
  // returns true. In this case, we want to ensure that the output of is_not
  // is non-empty.
  nom::combinator::verify(not_quote_slash, |s: &str| !s.is_empty())(input)
}

/// A string fragment contains a fragment of a string being parsed: either
/// a non-empty Literal (a series of non-escaped characters), a single
/// parsed escaped character, or a block of escaped whitespace.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StringFragment<'a> {
  Literal(&'a str),
  EscapedChar(char),
  EscapedWS,
}

/// Combine parse_literal, parse_escaped_whitespace, and parse_escaped_char
/// into a StringFragment.
fn pqa_parse_fragment<'a, E>(input: &'a str) -> nom::IResult<&'a str, StringFragment<'a>, E>
  where
      E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
{
  nom::branch::alt((
    // The `map` combinator runs a parser, then applies a function to the output
    // of that parser.
    nom::combinator::map(pqa_parse_literal, StringFragment::Literal),
    nom::combinator::map(pqa_parse_escaped_char, StringFragment::EscapedChar),
    nom::combinator::value(StringFragment::EscapedWS, parse_escaped_whitespace),
  ))(input)
}

fn pqa_build_quoted_atom_body(input: &str) -> nom::IResult<&str, String> {
  // fold_many0 is the equivalent of iterator::fold. It runs a parser in a loop,
  // and for each output value, calls a folding function on each output value.
  nom::multi::fold_many0(
    // Our parser function– parses a single string fragment
    pqa_parse_fragment,
    // Our init value, an empty string
    String::new(),
    // Our folding function. For each fragment, append the fragment to the
    // string.
    |mut output: String, fragment| {
      match fragment {
        StringFragment::Literal(s) => output.push_str(s),
        StringFragment::EscapedChar(c) => output.push(c),
        StringFragment::EscapedWS => {}
      }
      output
    },
  )(input)
}

pub fn parse_quoted_atom(input: &str) -> nom::IResult<&str, String> {
  nom::sequence::delimited(
    nom::character::complete::char('\''),
    pqa_build_quoted_atom_body,
    nom::character::complete::char('\''),
  )(input)
}

fn is_atom_character(c: char) -> bool {
  c.is_alphanumeric() || c == '_'
}

fn parse_lowercase_alpha(i: &str) -> nom::IResult<&str, char> {
  nom::combinator::verify(
    nom::character::complete::one_of("abcdefghijklmnopqrstuvwxyz"),
    |s: &char| -> bool { s.is_lowercase() },
  )(i)
}

fn parse_simple_atom(i: &str) -> nom::IResult<&str, String> {
  // TODO: starts not with a capital letter (may be not necessary for Flame logs)
  nom::combinator::map_res(
    nom::sequence::tuple(
      (parse_lowercase_alpha, // must start with a lowercase
       nom::bytes::complete::take_while(is_atom_character))
    ),
    |s: (char, &str)| {
      let value = s.0.to_string() + s.1;
      Ok::<String, nom::error::Error<&str>>(value)
    },
  )(i)
}

/// Try parse atom as a sequence of letters or a quoted atom in 'single quotes'
pub fn parse_atom(i: &str) -> nom::IResult<&str, String> {
  nom::branch::alt((
    nom::combinator::map_res(
      parse_quoted_atom,
      move |s: String| -> Result<String, nom::error::Error<&str>> {
        Ok(s)
      }),
    parse_simple_atom
  ))(i)
}
