//! Definitions for parser

use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::parsers::parser_input::CustomParserInput;

pub type ParserInput = CustomParserInput;

/// Gathers multiple errors and contexts together
pub type ErlParserError = nom::error::VerboseError<ParserInput>;

/// Generic return value from a Nom parser which takes a `ParserInput` and returns `Out`
pub type ParserResult<Out> = nom::IResult<ParserInput, Out, ErlParserError>;

/// Return value from a Nom parser which takes &str and returns `Vec<AstNode>`
pub type VecAstParserResult = ParserResult<Vec<AstNode>>;

/// Return value from a Nom parser which takes &str and returns `&str`
pub type StrSliceParserResult = ParserResult<ParserInput>;
