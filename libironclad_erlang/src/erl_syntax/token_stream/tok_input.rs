//! Tokenizer input type

/// Tokenizer input type
pub type TokenizerInput<'a> = &'a str;

/// Tokenizer parsers re
pub type TokensResult<'a, Out> =
  nom::IResult<TokenizerInput<'a>, Out, nom::error::VerboseError<TokenizerInput<'a>>>;

/// Gathers multiple errors and contexts together
pub type TokenizerError<'a> = nom::error::VerboseError<TokenizerInput<'a>>;
