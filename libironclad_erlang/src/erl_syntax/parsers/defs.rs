//! Definitions for parser

use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::erl_syntax::token_stream::token::Token;

/// Gathers multiple errors and contexts together
pub type ErlParserError<'a> = nom::error::VerboseError<ParserInput<'a>>;

/// Generic return value from a Nom parser which takes a `ParserInput` and returns `Out`
pub type ParserResult<'a, Out> = nom::IResult<ParserInput<'a>, Out, ErlParserError<'a>>;

/// Use when Nom's char is imported and this confuses the editor
pub type Char = char;

/// Return value from a Nom parser which takes &str and returns `Vec<AstNode>`
pub type VecAstParserResult<'a> = ParserResult<'a, Vec<AstNode>>;

/// Return value from a Nom parser which takes &str and returns `&str`
pub type StrSliceParserResult<'a> = ParserResult<'a, ParserInput<'a>>;
