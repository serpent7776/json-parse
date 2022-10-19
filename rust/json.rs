use std::collections::HashMap;
use std::string::FromUtf8Error;
use std::fmt;

#[derive(Debug, PartialEq)]
pub enum Json {
    Null,
    Bool(bool),
    Number{
        integer: i64,
        fraction: i64,
        precision: usize,
        exponent: i64,
    },
    String(String),
    Array(Vec<Json>),
    Object(Dict),
}

impl fmt::Display for Json {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Json::Null => write!(fmt, "null"),
            Json::Bool(b) => write!(fmt, "{}", b),
            Json::Number{integer, fraction, precision, exponent} => print_number(fmt, *integer, *fraction, *precision, *exponent),
            Json::String(s) => write!(fmt, "\"{}\"", escape(s)),
            Json::Array(arr) => print_json_array(arr, fmt),
            Json::Object(dict) => print_json_object(dict, fmt),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Error {
    EmptyString,
    CharMismatch { expected: u8, actual: u8 },
    HexCharExpected,
    UtfDecodingError(FromUtf8Error),
    NullExpected,
    TrueExpected,
    FalseExpected,
    ExponentRequired,
    UnrecognisedEscapeSequence(u8),
    InvalidValue,
    OutOfBounds,
    Garbage,
}

impl fmt::Display for Error {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::EmptyString => write!(fmt, "Empty string parsed"),
            Error::CharMismatch{expected, actual} => write!(fmt, "Expected {}, but read {}", expected, actual),
            Error::HexCharExpected => write!(fmt, "Expected hex char"),
            Error::UtfDecodingError(error) => write!(fmt, "{}", error),
            Error::NullExpected => write!(fmt, "Expected null"),
            Error::TrueExpected => write!(fmt, "Expected true"),
            Error::FalseExpected => write!(fmt, "Expected false"),
            Error::ExponentRequired => write!(fmt, "Exponent required"),
            Error::UnrecognisedEscapeSequence(ch) => write!(fmt, "Unrecognised escape sequence \\{}", ch),
            Error::InvalidValue => write!(fmt, "Invalid value"),
            Error::OutOfBounds => write!(fmt, "Out of bounds read attempt"),
            Error::Garbage => write!(fmt, "garbage found at the end of string"),
        }
    }
}

type Idx = usize;

pub type Dict = HashMap<String, Json>;

type JsonPart<'a> = Result<(Json, &'a [u8]), (Error, &'a [u8])>;

type JsonResult = Result<Json, (Error, Idx)>;

fn is_alpha(ch: &u8) -> bool {
    *ch >= b'a' && *ch <= b'z' || *ch >= b'A' && *ch <= b'Z'
}

fn is_digit(ch: &u8) -> bool {
    *ch >= b'0' && *ch <= b'9'
}

fn is_hex(ch: &u8) -> bool {
    *ch >= b'0' && *ch <= b'9' || *ch >= b'a' && *ch <= b'f' || *ch >= b'A' && *ch <= b'F'
}

fn is_ws(c: &u8) -> bool {
    *c == b' ' || *c == b'\t' || *c == b'\n' || *c == b'\r'
}

fn take(s: &[u8], f: impl Fn(&u8) -> bool) -> Result<(&[u8], &[u8]), (Error, &[u8])> {
    let mut i = 0;
    while i < s.len() && f(&s[i]) {
        i += 1;
    }
    if i > 0 {
        Ok((&s[0..i], &s[i..]))
    } else {
        Err((Error::EmptyString, s))
    }
}

fn ask(
    s: &[u8],
    f: impl Fn(&[u8]) -> Result<(Option<u8>, &[u8]), (Error, &[u8])>,
) -> Result<(String, &[u8]), (Error, &[u8])> {
    let mut data = Vec::<u8>::new();
    let mut cont = s;
    loop {
        if cont.is_empty() {
            break;
        }
        if let (Some(c), s) = f(cont)? {
            data.push(c);
            cont = s;
        } else {
            break;
        }
    }
    match String::from_utf8(data) {
        Ok(string) => Ok((string, cont)),
        Err(error) => Err((Error::UtfDecodingError(error), cont)),
    }
}

fn skip(s: &[u8], f: impl Fn(&u8) -> bool) -> &[u8] {
    let mut s = s;
    while let Some(c) = s.first() {
        if f(c) {
            s = &s[1..]
        } else {
            break;
        }
    }
    s
}

fn skip_ws(s: &[u8]) -> &[u8] {
    skip(s, is_ws)
}

fn chr(s: &[u8], ch: u8) -> Result<(u8, &[u8]), (Error, &[u8])> {
    match s.first() {
        Some(c) => {
            if *c == ch {
                Ok((ch, &s[1..]))
            } else {
                Err((
                    Error::CharMismatch {
                        expected: ch,
                        actual: *c,
                    },
                    s,
                ))
            }
        }
        None => Err((Error::OutOfBounds, s)),
    }
}

fn parse_null(s: &[u8]) -> JsonPart {
    match take(s, is_alpha) {
        Ok((&[b'n', b'u', b'l', b'l'], s)) => Ok((Json::Null, s)),
        _ => Err((Error::NullExpected, s)),
    }
}

fn parse_true(s: &[u8]) -> JsonPart {
    match take(s, is_alpha) {
        Ok((&[b't', b'r', b'u', b'e'], s)) => Ok((Json::Bool(true), s)),
        _ => Err((Error::TrueExpected, s)),
    }
}

fn parse_false(s: &[u8]) -> JsonPart {
    match take(s, is_alpha) {
        Ok((&[b'f', b'a', b'l', b's', b'e'], s)) => Ok((Json::Bool(false), s)),
        _ => Err((Error::FalseExpected, s)),
    }
}

fn parse_fraction(s: &[u8]) -> Result<(i64, usize, &[u8]), (Error, &[u8])> {
    match take(s, is_digit) {
        Ok((fractions, s)) => Ok((to_i64(fractions), fractions.len(), s)),
        _ => Ok((0, 0, s)),
    }
}

fn parse_exponent(s: &[u8]) -> Result<(i64, &[u8]), (Error, &[u8])> {
    match take(s, is_digit) {
        Ok((exponent, s)) => Ok((to_i64(exponent), s)),
        _ => Err((Error::ExponentRequired, s)),
    }
}

fn to_i64(s: &[u8]) -> i64 {
    std::str::from_utf8(s).unwrap().parse::<i64>().unwrap()
}

fn parse_number_parts(s: &[u8]) -> Result<(i64, i64, usize, i64, &[u8]), (Error, &[u8])> {
    let (ints, s) = take(s, is_digit)?;
    let ints = to_i64(ints);
    let (fractions, precision, s) = match chr(s, b'.') {
        Ok((_, s)) => parse_fraction(s)?,
        _ => (0, 0, s),
    };
    let (exponent, s) = match chr(s, b'e') {
        Ok((_, s)) => parse_exponent(s)?,
        _ => (0, s),
    };
    Ok((ints, fractions, precision, exponent, s))
}

fn parse_number(s: &[u8]) -> JsonPart {
    let (ints, fractions, precision, exponent, s) = parse_number_parts(s)?;
    let json = Json::Number{
        integer: ints,
        fraction: fractions,
        precision: precision,
        exponent: exponent,
    };
    Ok((json, s))
}

fn parse_negative_number(s: &[u8]) -> JsonPart {
    let (_, s) = chr(s, b'-')?;
    let (ints, fractions, precision, exponent, s) = parse_number_parts(s)?;
    let json = Json::Number{
        integer: -ints,
        fraction: fractions,
        precision: precision,
        exponent: exponent,
    };
    Ok((json, s))
}

fn hex(s: &[u8]) -> Result<(u8, &[u8]), (Error, &[u8])> {
    match s.first() {
        None => Err((Error::OutOfBounds, s)),
        Some(c) => {
            if is_hex(c) {
                Ok((*c, &s[1..]))
            } else {
                Err((Error::HexCharExpected, s))
            }
        }
    }
}

fn hexword(s: &[u8]) -> Result<(u8, &[u8]), (Error, &[u8])> {
    let (_, s) = hex(s)?;
    let (_, s) = hex(s)?;
    let (_, s) = hex(s)?;
    let (_, s) = hex(s)?;
    Ok((b'?', s)) // We don't support unicode, just return question mark
}

fn string_char(s: &[u8]) -> Result<(Option<u8>, &[u8]), (Error, &[u8])> {
    match s.first() {
        None => Err((Error::OutOfBounds, s)),
        Some(b'"') => Ok((None, &s)),
        Some(b'\\') => {
            match s[1..].first() {
                None => Err((Error::OutOfBounds, s)),
                Some(b'"') => Ok((Some(b'"'), &s[2..])),
                Some(b'\\') => Ok((Some(b'\\'), &s[2..])),
                Some(b'/') => Ok((Some(b'/'), &s[2..])),
                Some(b'b') => Ok((Some(0x08), &s[2..])), // rust doesn't support \b
                Some(b'f') => Ok((Some(0x0C), &s[2..])), // rust doesn't support \f
                Some(b'n') => Ok((Some(b'\n'), &s[2..])),
                Some(b'r') => Ok((Some(b'\r'), &s[2..])),
                Some(b't') => Ok((Some(b'\t'), &s[2..])),
                Some(b'u') => {
                    let (c, s) = hexword(&s[2..])?;
                    Ok((Some(c), s))
                }
                _ => Err((Error::UnrecognisedEscapeSequence(s[1]), s)),
            }
        }
        Some(c) => Ok((Some(*c), &s[1..])),
    }
}

fn parse_string_raw(s: &[u8]) -> Result<(String, &[u8]), (Error, &[u8])> {
    let (_, s) = chr(s, b'"')?;
    let (contents, s) = ask(s, string_char)?;
    let (_, s) = chr(s, b'"')?;
    Ok((contents, s))
}

fn parse_string(s: &[u8]) -> JsonPart {
    let (contents, s) = parse_string_raw(s)?;
    Ok((Json::String(String::from(contents)), s))
}

fn parse_array_items(s: &[u8], value: Json) -> Result<(Vec<Json>, &[u8]), (Error, &[u8])> {
    let mut items = vec![value];
    let mut cont = s;
    while let Ok((_, s)) = chr(skip_ws(cont), b',') {
        let (value, s) = parse_value(s)?;
        items.push(value);
        cont = s;
    }
    Ok((items, cont))
}

fn parse_array(s: &[u8]) -> JsonPart {
    let (_, s) = chr(s, b'[')?;
    let (items, s) = match parse_value(s) {
        Err((_, s)) => Ok((vec![], s)),
        Ok((value, s)) => parse_array_items(s, value),
    }?;
    let s = skip_ws(s);
    let (_, s) = chr(s, b']')?;
    Ok((Json::Array(items), s))
}

fn parse_key_value_pair(s: &[u8]) -> Result<(String, Json, &[u8]), (Error, &[u8])> {
    let s = skip_ws(s);
    let (key, s) = parse_string_raw(s)?;
    let s = skip_ws(s);
    let (_, s) = chr(s, b':')?;
    let (value, s) = parse_value(s)?;
    Ok((key, value, s))
}

fn parse_object_items(s: &[u8], key: String, value: Json) -> Result<(Dict, &[u8]), (Error, &[u8])> {
    let mut items = Dict::new();
    items.insert(key, value);
    let mut cont = s;
    while let Ok((_, s)) = chr(skip_ws(cont), b',') {
        let (key, value, s) = parse_key_value_pair(s)?;
        items.insert(key, value);
        cont = s;
    }
    Ok((items, cont))
}

fn parse_object(s: &[u8]) -> JsonPart {
    let (_, s) = chr(s, b'{')?;
    let s = skip_ws(s);
    let (pairs, s) = if let Some(b'"') = s.first() {
        let (key, value, s) = parse_key_value_pair(s)?;
        parse_object_items(s, key, value)?
    } else {
        (Dict::new(), s)
    };
    let s = skip_ws(s);
    let (_, s) = chr(s, b'}')?;
    Ok((Json::Object(pairs), s))
}

fn parse_value(s: &[u8]) -> JsonPart {
    let s = skip_ws(s);
    match s.first() {
        Some(b'n') => parse_null(s),
        Some(b't') => parse_true(s),
        Some(b'f') => parse_false(s),
        Some(b'0'..=b'9') => parse_number(s),
        Some(b'-') => parse_negative_number(s),
        Some(b'"') => parse_string(s),
        Some(b'[') => parse_array(s),
        Some(b'{') => parse_object(s),
        None => Err((Error::OutOfBounds, s)),
        _ => Err((Error::InvalidValue, s)),
    }
}

pub fn parse(s: String) -> JsonResult {
    let len = s.as_bytes().len();
    let s = skip_ws(s.as_bytes());
    if s.is_empty() {
        return Err((Error::EmptyString, 0));
    }
    match parse_value(s) {
        Ok((json, s)) => {
            let s = skip_ws(s);
            if s.is_empty() {
                Ok(json)
            } else {
                Err((Error::Garbage, (len - s.len())))
            }
        }
        Err((error, s)) => Err((error, (len - s.len()))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fails<F: FnOnce(String) -> JsonResult>(f: F, arg: &str, expected_err: Error) {
        let actual = f(String::from(arg));
        match actual {
            Err((actual_err, _)) => assert!(
                expected_err == actual_err,
                "\n\tFailed for {}: Expected to fail with {:?}, but failed with {:?} instead",
                arg,
                expected_err,
                actual_err
            ),
            Ok(actual_json) => assert!(
                false,
                "\n\tFailed for {}: Expected to fail with {:?}, but got {:?}",
                arg, expected_err, actual_json
            ),
        }
    }

    fn ok<F: FnOnce(String) -> JsonResult>(f: F, arg: &str, expected_json: Json) {
        let actual = f(String::from(arg));
        match actual {
            Ok(actual_json) => assert!(
                expected_json == actual_json,
                "\n\tFailed for {}: Expected to return {:?}, but returned {:?} instead",
                arg,
                expected_json,
                actual_json
            ),
            Err((actual_err, _)) => assert!(
                false,
                "\n\tFailed for {}: Expected to succeed as {:?}, but failed with {:?}",
                arg, expected_json, actual_err
            ),
        }
    }

    macro_rules! fails {
        ($f: ident, $str: expr, $err: expr, $name: ident) => {
            #[test]
            fn $name() {
                fails($f, $str, $err);
            }
        };
    }

    macro_rules! ok {
        ($f: ident, $str: expr, $err: expr, $name: ident) => {
            #[test]
            fn $name() {
                ok($f, $str, $err);
            }
        };
    }

    fails!(parse, "", Error::EmptyString, empty_string_fails_to_parse);
    fails!(parse, "x", Error::InvalidValue, incorrect_value_fails);

    // literals
    ok!(parse, "null", Json::Null, null_parses_ok);
    ok!(parse, "true", Json::Bool(true), true_parses_ok);
    ok!(parse, "false", Json::Bool(false), false_parses_ok);
    fails!(parse, "truefalse", Error::TrueExpected, unknown_ident_truefalse);

    // numbers
    ok!(parse, "42", Json::Number{integer: 42, fraction: 0, precision: 0, exponent: 0}, number_parses_ok);
    ok!(parse, "0", Json::Number{integer: 0, fraction: 0, precision: 0, exponent: 0}, zero_parses_ok);
    ok!(parse, "-1", Json::Number{integer: -1, fraction: 0, precision: 0, exponent: 0}, negative_parses_ok);
    ok!(parse, "1.23", Json::Number{integer: 1, fraction: 23, precision: 2, exponent: 0}, float_parses_ok);
    ok!(parse, "1.230", Json::Number{integer: 1, fraction: 230, precision: 3, exponent: 0}, float_with_trailing_zero_parses_ok);
    ok!(parse, "1.", Json::Number{integer: 1, fraction: 0, precision: 0, exponent: 0}, float_without_fraction_parses_ok);
    ok!(parse, "0.", Json::Number{integer: 0, fraction: 0, precision: 0, exponent: 0}, zero_without_fraction_parses_ok);
    ok!(parse, "-0.", Json::Number{integer: 0, fraction: 0, precision: 0, exponent: 0}, negative_zero_without_fraction_parses_ok);
    ok!(parse, "6.999e3", Json::Number{integer: 6, fraction: 999, precision: 3, exponent: 3}, float_with_exponent_parses_ok);
    ok!(parse, "-1.2e9", Json::Number{integer: -1, fraction: 2, precision: 1, exponent: 9}, negative_float_with_exponent_parses_ok);
    fails!(parse, "6.999e", Error::ExponentRequired, float_without_exponent_fails_to_parse);
    ok!(parse, "1.e1", Json::Number{integer: 1, fraction: 0, precision: 0, exponent: 1}, float_with_exponent_but_without_fractin_parses_ok);
    fails!(parse, "1.x", Error::Garbage, float_with_invalid_fraction_fails_to_parse);
    fails!(parse, "-1.y", Error::Garbage, negative_float_with_invalid_fraction_fails_to_parse);
    fails!(parse, ".12", Error::InvalidValue, float_without_integer_part_fails_to_parse);
    fails!(parse, "-.12", Error::EmptyString, negative_float_without_integer_part_fails_to_parse);

    // strings
    ok!(parse, r#""""#, Json::String(String::from("")), empty_string_parses_ok);
    fails!(parse, r#"""#, Error::OutOfBounds, missing_ending_quote_in_empty_string);
    ok!(parse, r#""foobar""#, Json::String(String::from("foobar")), nonempty_string_parses_ok);
    ok!(parse, r#""a\nb""#, Json::String(String::from("a\nb")), string_with_newline_parses_ok);
    ok!(parse, r#""foo\\bar""#, Json::String(String::from(r#"foo\bar"#)), string_with_escaped_backslash_parses_ok);
    ok!(parse, r#""foo bar""#, Json::String(String::from("foo bar")), string_with_space_parses_ok);
    ok!(parse, r#""foo/bar""#, Json::String(String::from("foo/bar")), string_with_slash_parses_ok);
    fails!(parse, r#""foobar"#, Error::OutOfBounds, missing_ending_quote_in_nonempty_string);
    fails!(parse, r#""foo"bar"#, Error::Garbage, extra_letters_after_string);
    fails!(parse, r#""foo\"bar"#, Error::OutOfBounds, missing_ending_quote_with_inner_escaped_quote);
    ok!(parse, r#""a b c""#, Json::String(String::from("a b c")), string_with_inner_spaces_parses_ok);
    ok!(parse, r#"" a b c ""#, Json::String(String::from(" a b c ")), string_with_leading_trailing_spaces_parses_ok);
    ok!(parse, r#""foo\"bar""#, Json::String(String::from(r#"foo"bar"#)), string_with_escaped_quote_parses_ok);
    ok!(parse, r#""\u1234""#, Json::String(String::from("?")), unicode_symbol_parses_ok);
    ok!(parse, r#""\u1234\uabcd""#, Json::String(String::from("??")), string_with_two_unicode_symbols_parses_ok);
    ok!(parse, r#""\u1234\uabcd\u00Ff""#, Json::String(String::from("???")), string_with_three_unicode_symbols_parses_ok);
    ok!(parse, r#""foo\u12cdbar""#, Json::String(String::from("foo?bar")), string_with_inner_unicode_symbol_parses_ok);
    fails!(parse, r#""\u12cx""#, Error::HexCharExpected, invalid_unicode_sequence);
    fails!(parse, r#""\"#, Error::OutOfBounds, missing_ending_quote_with_escaped_quote);

    // arrays
    ok!(parse, "[]", Json::Array(Vec::new()), empty_array_parses_ok);
    ok!(parse, "[null]", Json::Array(vec![ Json::Null ]), array_with_null_parses_ok);
    ok!(parse, "[[null]]", Json::Array(vec![ Json::Array(vec![ Json::Null ]) ]), nested_array_with_null_parses_ok);
    ok!(parse, "[true]", Json::Array(vec![Json::Bool(true)]), array_with_true_parses_ok);
    ok!(parse, "[false]", Json::Array(vec![Json::Bool(false)]), array_with_false_parses_ok);
    ok!(parse, "[true,false]", Json::Array(vec![Json::Bool(true), Json::Bool(false)]), array_with_true_and_false_parses_ok);
    ok!(parse, "[1.2]", Json::Array(vec![ (Json::Number{integer : 1, fraction : 2, precision : 1, exponent : 0}) ]), array_with_single_number_parses_ok);
    ok!(parse, r#"["abc"]"#, Json::Array(vec![ Json::String(String::from("abc")) ]), array_with_string_parses_ok);
    ok!(parse, "[[[]]]", Json::Array(vec![ Json::Array(vec![ Json::Array(Vec::new()) ]) ]), deeply_nested_empty_array_parses_ok);
    ok!(parse, "[[[\"a\"]]]", Json::Array(vec![ Json::Array(vec![ Json::Array(vec![ Json::String(String::from("a")) ]) ]) ]), deeply_nested_array_with_string_parses_ok);
    fails!(parse, "[", Error::OutOfBounds, sole_opening_bracket_fails_to_parse_as_array);
    fails!(parse, "]", Error::InvalidValue, sole_closing_bracket_fails_to_parse_as_array);
    fails!(parse, "[[[", Error::OutOfBounds, multiple_opening_brackets_fail_to_parse_as_array);
    fails!(parse, "]]]", Error::InvalidValue, multiple_closing_brackets_fail_to_parse_as_array);
    ok!(parse, "[1,2,3]", Json::Array (vec![
                                       Json::Number{integer : 1, fraction : 0, precision : 0, exponent : 0},
                                       Json::Number{integer : 2, fraction : 0, precision : 0, exponent : 0},
                                       Json::Number{integer : 3, fraction : 0, precision : 0, exponent : 0},
    ]), array_with_numbers_parses_ok);
    fails!(parse, r#"[""#, Error::OutOfBounds, unclosed_string_in_unclosed_array_fail_to_parse);
    ok!(parse, "[true,false,null,0]", Json::Array (vec![
                                                   (Json::Bool(true)), (Json::Bool(false)), Json::Null,
                                                   Json::Number{integer : 0, fraction : 0, precision : 0, exponent : 0},
    ]), array_with_different_types_parses_ok);
    ok!(parse, "[[],[]]", Json::Array(vec![ Json::Array(Vec::new()), Json::Array(Vec::new()) ]), nested_array_parses_ok);
    fails!(parse, "[1,", Error::OutOfBounds, missing_closing_bracket_after_first_element);
    fails!(parse, "[1,]", Error::InvalidValue, missing_second_element_in_array);
    fails!(parse, "[1,2,]", Error::InvalidValue, missing_third_element_in_array);
    fails!(parse, "[1,2,", Error::OutOfBounds, missing_closing_bracket_after_second_element);

    // objects
    ok!(parse, "{}", Json::Object(Dict::new()), empty_object_parses_ok);
    ok!(parse, r#"{"1":1}"#, Json::Object(Dict::from([(String::from("1"), Json::Number{integer : 1, fraction : 0, precision : 0, exponent : 0})]) ), object_with_single_key_value_pair_parses_ok);
    ok!(parse, r#"{"foo":"bar"}"#, Json::Object(Dict::from([ (String::from("foo"), Json::String(String::from("bar"))) ])), object_with_string_value_parses_ok);
    ok!(parse, r#"{"":""}"#, Json::Object(Dict::from([ (String::from(""), Json::String(String::from(""))) ])), object_with_empty_key_parses_ok);
    ok!(parse, r#"{"12":[]}"#, Json::Object(Dict::from([ (String::from("12"), Json::Array(Vec::new()) ) ])), object_with_empty_array_value_parses_ok);
    ok!(parse, r#"{"a":1,"b":2,"c":3}"#, Json::Object(Dict::from([
                                                               (String::from("a"), Json::Number{integer : 1, fraction : 0, precision : 0, exponent : 0} ),
                                                               (String::from("b"), Json::Number{integer : 2, fraction : 0, precision : 0, exponent : 0} ),
                                                               (String::from("c"), Json::Number{integer : 3, fraction : 0, precision : 0, exponent : 0} ),
    ])), object_with_many_integer_values_parses_ok);
    ok!(parse, r#"{"x":9.8e7}"#, Json::Object(Dict::from([ (String::from("x"), Json::Number{integer : 9, fraction : 8, precision : 1, exponent : 7} ) ])), object_with_float_number_value_parses_ok);
    fails!(parse, r#"{"1":1"#, Error::OutOfBounds, missing_closing_paren);
    fails!(parse, r#"{"foo"}"#, Error::CharMismatch{expected: b':', actual: b'}'}, missing_colon_and_value_after_key);
    fails!(parse, r#"{"foo":}"#, Error::InvalidValue, missing_value_after_key);

    // values with spaces
    fails!(parse, "   ", Error::EmptyString, just_spaces_fail_to_parse);
    fails!(parse, " [  ", Error::OutOfBounds, missing_closing_bracket_with_spaces);
    fails!(parse, " ]  ", Error::InvalidValue, missing_opening_bracket_with_spaces);
    ok!(parse, "   null   ", Json::Null, null_with_spaces_parses_ok);
    ok!(parse, "   true   ", Json::Bool(true), true_with_spaces_parses_ok);
    ok!(parse, "  false  ", Json::Bool(false), false_with_spaces_parses_ok);
    ok!(parse, r#"" \u1234 \uabcd \u00Ff ""#, Json::String(String::from(" ? ? ? ")), string_with_unicodes_with_spaces_parses_ok);
    ok!(parse, " [ true, false, null ] ", Json::Array(vec![ Json::Bool(true), Json::Bool(false), Json::Null ]), array_with_spaces_parses_ok);
    ok!(parse, " [ true , false , null ] ", Json::Array(vec![ Json::Bool(true), Json::Bool(false), Json::Null ]), array_with_more_spaces_parses_ok);
    ok!(parse, r#" { "a" : true , "b" : false , "c" : null } "#, Json::Object(Dict::from([ (String::from("a"), Json::Bool(true)), (String::from("b"), Json::Bool(false)), (String::from("c"), Json::Null) ])), object_with_spaces_parses_ok);
    ok!(parse, r#" {  } "#, Json::Object(Dict::new()) , empty_object_with_spaces_parses_ok);
    ok!(parse, r#" [  ] "#, Json::Array(Vec::new()) , empty_array_with_spaces_parses_ok);

}

fn escape(s: &String) -> String {
    let esc = |c: char| match c {
        '\"' => ("\\\"").chars().collect(),
        '\\' => ("\\\\").chars().collect(),
        '\r' => ("\\r").chars().collect(),
        '\n' => ("\\n").chars().collect(),
        '\t' => ("\\t").chars().collect(),
        '\u{0008}' => ("\\b").chars().collect(),
        '\u{000C}' => ("\\f").chars().collect(),
        c => vec![c],
    };
    s.chars().flat_map(esc).collect()
}

fn print_number(fmt: &mut fmt::Formatter, integer: i64, fraction: i64, precision: usize, exponent: i64) -> fmt::Result {
    let r = write!(fmt, "{}", integer);
    if precision > 0 {
        write!(fmt, ".{}", fraction)?;
    }
    if exponent != 0 {
        write!(fmt, "e{}", exponent)?;
    }
    r
}

fn print_json_array(arr: &Vec<Json>, fmt: &mut fmt::Formatter) -> fmt::Result {
    write!(fmt, "[")?;
    if arr.len() > 0 {
        write!(fmt, "{}", arr[0])?;
        if arr.len() > 1 {
            for item in arr.iter() {
                write!(fmt, ", {}", item)?;
            }
        }
    }
    write!(fmt, "]")
}

fn print_json_object(obj: &Dict, fmt: &mut fmt::Formatter) -> fmt::Result {
    write!(fmt, "{{")?;
    let mut iter = obj.iter();
    if let Some((key, value)) = iter.next() {
        write!(fmt, "\"{}\": {}", escape(key), value)?;
        for (key, value) in iter {
            write!(fmt, ", \"{}\": {}", escape(key), value)?;
        }
    }
    write!(fmt, "}}")
}
