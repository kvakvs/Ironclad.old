//! Tokenizer input type

/// Tokenizer input type
pub type TokInput<'a> = &'a str;
/// Tokenizer parsers re
pub type TokResult<'a, Out> =
  nom::IResult<TokInput<'a>, Out, nom::error::VerboseError<TokInput<'a>>>;
/// Gathers multiple errors and contexts together
pub type TokenizerError<'a> = nom::error::VerboseError<TokInput<'a>>;
