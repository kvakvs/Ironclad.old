//! Groups type definitions shared by all preprocessor parse modules
use crate::preprocessor_syntax::pp_ast::PpAst;
use std::sync::Arc;

/// Gathers multiple errors and contexts together
pub type PpParserError<'a> = nom::error::VerboseError<&'a str>;

/// Generic return value from a Nom parser which takes &str and returns `Out`
pub type PpParserResult<'a, Out> = nom::IResult<&'a str, Out, PpParserError<'a>>;

/// Return value from a Nom parser which takes &str and returns `Arc<PpAst>`
pub type PpAstParserResult<'a> = PpParserResult<'a, Arc<PpAst>>;

/// Return value from a Nom parser which takes &str and returns `Vec<Arc<PpAst>>`
pub type VecPpAstParserResult<'a> = PpParserResult<'a, Vec<Arc<PpAst>>>;

/// Return value from a Nom parser which takes &str and returns `String`
pub type PpStringParserResult<'a> = PpParserResult<'a, String>;

/// Return value from a Nom parser which takes &str and returns `&str`
pub type StrSliceParserResult<'a> = PpParserResult<'a, &'a str>;

/// Return value from a Nom parser which takes &str and returns `()`
pub type VoidParserResult<'a> = PpParserResult<'a, ()>;

/// Groups code for parsing preprocessor directives
pub struct PreprocessorParser {}
