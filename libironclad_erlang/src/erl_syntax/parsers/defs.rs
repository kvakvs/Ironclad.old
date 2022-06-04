//! Definitions for parser

use crate::erl_syntax::erl_ast::AstNode;
// use std::ops::Deref;

/// Used as input to all parsers, and contains the chain of inputs (for nested parsing), and current
/// position for the current parser.
pub type ParserInput<'a> = &'a str;
// pub struct ParserInput<'a> {
//   // TODO: Chain of Arc<String> for inputs to allow nested parsing of includes and macro fragments?
//   /// Input
//   pub text: &'a str,
// }

// impl<'a> Deref for ParserInput<'a> {
//   type Target = str;
//
//   fn deref(&self) -> &Self::Target {
//     self.text
//   }
// }
//
// impl<'a> ParserInput<'a> {
//   /// Access text in an abstract way, leaving more freedom to reimplement the internal text storage
//   pub fn as_str(&'a self) -> &str {
//     self.text
//   }
// }

/// Gathers multiple errors and contexts together
pub type ErlParserError<'a> = nom::error::VerboseError<ParserInput<'a>>;

/// Generic return value from a Nom parser which takes a `ParserInput` and returns `Out`
pub type ParserResult<'a, Out> = nom::IResult<ParserInput<'a>, Out, ErlParserError<'a>>;

/// Return value from a Nom parser which takes &str and returns `Vec<AstNode>`
pub type VecAstParserResult<'a> = ParserResult<'a, Vec<AstNode>>;

/// Return value from a Nom parser which takes &str and returns `&str`
pub type StrSliceParserResult<'a> = ParserResult<'a, &'a str>;
