//! Groups type definitions shared by all preprocessor parse modules
use crate::preprocessor_syntax::pp_ast::PpAst;
use std::sync::Arc;

/// Gathers multiple errors and contexts together
#[deprecated = "Belonged to libironclad_preprocessor crate"]
pub type PpParserError<'a> = nom::error::VerboseError<&'a str>;

/// Generic return value from a Nom parser which takes &str and returns `Out`
#[deprecated = "Belonged to libironclad_preprocessor crate"]
pub type PpParserResult<'a, Out> = nom::IResult<&'a str, Out, PpParserError<'a>>;

/// Return value from a Nom parser which takes &str and returns `Arc<PpAst>`
#[deprecated = "Belonged to libironclad_preprocessor crate"]
pub type PpAstParserResult<'a> = PpParserResult<'a, Arc<PpAst>>;

/// Return value from a Nom parser which takes &str and returns `Vec<Arc<PpAst>>`
#[deprecated = "Belonged to libironclad_preprocessor crate"]
pub type VecPpAstParserResult<'a> = PpParserResult<'a, Vec<Arc<PpAst>>>;

/// Return value from a Nom parser which takes &str and returns `String`
#[deprecated = "Belonged to libironclad_preprocessor crate"]
pub type PpStringParserResult<'a> = PpParserResult<'a, String>;

/// Return value from a Nom parser which takes &str and returns `&str`
#[deprecated = "Belonged to libironclad_preprocessor crate"]
pub type StrSliceParserResult<'a> = PpParserResult<'a, &'a str>;

/// Return value from a Nom parser which takes &str and returns `()`
#[deprecated = "Belonged to libironclad_preprocessor crate"]
pub type VoidParserResult<'a> = PpParserResult<'a, ()>;

/// Groups code for parsing preprocessor directives
#[deprecated = "Belonged to libironclad_preprocessor crate"]
pub struct PreprocessorParser {}
