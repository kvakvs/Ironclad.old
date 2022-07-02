//! Defines a special new error type for parser errors, compatible with Nom

use crate::erl_syntax::parsers::lang_construct::{LangConstruct, LangConstructs};
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::erl_syntax::parsers::token_stream::keyword::Keyword;
use crate::erl_syntax::parsers::token_stream::token_type::TokenType;
use nom::error::ErrorKind;
use std::fmt::{Debug, Display, Formatter};

/// Produced by failing parsers
#[derive(Debug, Clone)]
pub enum ErlParserErrorKind {
  /// A list of language structures expected, but not found. Reported for failed `alt()` parsers
  LanguageConstructsExpected(Vec<LangConstruct>),
  /// A standard Nom error wrapped
  Nom(ErrorKind),
  /// Added by `context()` parser combinator
  Context(&'static str),
  /// Added by `char()` parser combinator
  Char(char),
  /// Need an atom of specific value
  AtomExpected(String),
  /// Any atom
  AnyAtomExpected,
  /// Need a specific keyword
  KeywordExpected(Keyword),
  /// Used for preprocessor directives and module attributes, after `-` a keyword or atom is expected
  AnyKeywordOrAtomExpected,
  /// Need a specific token
  TokenExpected(TokenType),
  /// Any integer
  IntegerLiteralExpected,
  /// Any float
  FloatLiteralExpected,
  /// A string literal is needed
  StringLiteralExpected,
  /// A variable started with `_` or a capital letter (or a typename) is needed
  VariableExpected,
  /// Expected a `-module(NAME).` attribute
  ModuleStartAttributeExpected,
}

/// Gathers multiple errors and contexts together
// pub type ErlParserError<'a> = nom::error::VerboseError<ParserInput<'a>>;
#[derive(Clone, Debug)]
pub struct ErlParserError<'a> {
  /// List of errors accumulated by `ErlParserError`, containing the affected
  /// part of input data, and some context
  pub errors: Vec<(ParserInput<'a>, ErlParserErrorKind)>,
}

impl<'a> nom::error::ContextError<ParserInput<'a>> for ErlParserError<'a> {
  fn add_context(input: ParserInput<'a>, ctx: &'static str, mut other: Self) -> Self {
    other.errors.push((input, ErlParserErrorKind::Context(ctx)));
    other
  }
}

impl<'a> nom::error::ParseError<ParserInput<'a>> for ErlParserError<'a> {
  fn from_error_kind(input: ParserInput<'a>, kind: ErrorKind) -> Self {
    ErlParserError {
      errors: vec![(input, ErlParserErrorKind::Nom(kind))],
    }
  }

  fn append(input: ParserInput<'a>, kind: ErrorKind, mut other: Self) -> Self {
    other.errors.push((input, ErlParserErrorKind::Nom(kind)));
    other
  }

  fn from_char(input: ParserInput<'a>, c: char) -> Self {
    ErlParserError { errors: vec![(input, ErlParserErrorKind::Char(c))] }
  }
}

impl<'a> ErlParserError<'a> {
  /// Create an atom expected error
  #[inline]
  pub fn atom_expected(input: ParserInput<'a>, atom: &str) -> Self {
    ErlParserError {
      errors: vec![(input, ErlParserErrorKind::AtomExpected(atom.to_string()))],
    }
  }

  /// Create a "token expected" error
  #[inline]
  pub fn token_expected(input: ParserInput<'a>, tt: TokenType) -> Self {
    ErlParserError {
      errors: vec![(input, ErlParserErrorKind::TokenExpected(tt))],
    }
  }

  /// Create an "any atom expected" error
  #[inline]
  pub fn any_atom_expected(input: ParserInput<'a>) -> Self {
    ErlParserError {
      errors: vec![(input, ErlParserErrorKind::AnyAtomExpected)],
    }
  }

  /// Create a Keyword expected error
  #[inline]
  pub fn keyword_expected(input: ParserInput<'a>, k: Keyword) -> Self {
    ErlParserError {
      errors: vec![(input, ErlParserErrorKind::KeywordExpected(k))],
    }
  }

  /// Create a Keyword-or-Atom expected error
  #[inline]
  pub fn any_keyword_or_atom_expected(input: ParserInput<'a>) -> Self {
    ErlParserError {
      errors: vec![(input, ErlParserErrorKind::AnyKeywordOrAtomExpected)],
    }
  }

  /// Create a "integer literal expected" error
  #[inline]
  pub fn integer_literal_expected(input: ParserInput<'a>) -> Self {
    ErlParserError {
      errors: vec![(input, ErlParserErrorKind::IntegerLiteralExpected)],
    }
  }

  /// Create a "float literal expected" error
  #[inline]
  pub fn float_literal_expected(input: ParserInput<'a>) -> Self {
    ErlParserError {
      errors: vec![(input, ErlParserErrorKind::FloatLiteralExpected)],
    }
  }

  /// Create a "string literal expected" error
  #[inline]
  pub fn string_literal_expected(input: ParserInput<'a>) -> Self {
    ErlParserError {
      errors: vec![(input, ErlParserErrorKind::StringLiteralExpected)],
    }
  }

  /// Create a "variable expected" error
  #[inline]
  pub fn variable_expected(input: ParserInput<'a>) -> Self {
    ErlParserError {
      errors: vec![(input, ErlParserErrorKind::VariableExpected)],
    }
  }

  /// Create a "-module() expected" error
  #[inline]
  pub fn module_start_attribute_expected(input: ParserInput<'a>) -> Self {
    ErlParserError {
      errors: vec![(input, ErlParserErrorKind::ModuleStartAttributeExpected)],
    }
  }

  /// Create a "none of the constructs matched" error
  #[inline]
  pub fn alt(input: ParserInput<'a>, constr: &[LangConstruct]) -> Self {
    ErlParserError {
      errors: vec![(input, ErlParserErrorKind::LanguageConstructsExpected(constr.into()))],
    }
  }
}

impl Display for ErlParserErrorKind {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      ErlParserErrorKind::Nom(e) => e.fmt(f),
      ErlParserErrorKind::AtomExpected(a) => write!(f, "Atom expected: {}", a),
      ErlParserErrorKind::AnyAtomExpected => write!(f, "Atom expected"),
      ErlParserErrorKind::KeywordExpected(k) => write!(f, "Keyword expected: {}", k),
      ErlParserErrorKind::TokenExpected(tt) => {
        write!(f, "Token expected: {} ({})", tt, tt.as_explanation_str())
      }
      ErlParserErrorKind::IntegerLiteralExpected => write!(f, "An integer literal expected"),
      ErlParserErrorKind::FloatLiteralExpected => write!(f, "A float literal expected"),
      ErlParserErrorKind::StringLiteralExpected => write!(f, "A string literal expected"),
      ErlParserErrorKind::VariableExpected => write!(f, "A variable name expected"),
      ErlParserErrorKind::ModuleStartAttributeExpected => {
        write!(f, "Module start attribute -module(NAME) expected")
      }
      ErlParserErrorKind::LanguageConstructsExpected(structures) => {
        write!(
          f,
          "Could not parse any of the following constructs: {}",
          LangConstructs(&structures)
        )
      }
      _ => unimplemented!("Don't know how to format {:?}", self),
    }
  }
}
