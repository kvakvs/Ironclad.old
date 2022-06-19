//! Preprocessing support for `ErlModule`

use crate::erl_syntax::token_stream::token::Token;
use crate::error::ic_error::IcResult;
use crate::project::module::mod_impl::{ErlModule, ErlModuleImpl};

impl ErlModuleImpl {
  /// Filter through the tokens array and produce a new token array with preprocessor directives
  /// eliminated, files included and macros substituted.
  pub fn preprocess(_module: &ErlModule, tokens: &[Token]) -> IcResult<Vec<Token>> {
    // TODO: Interpret -define/undef -if/ifdef/ifndef/else
    // TODO: Interpret -include and -include_lib
    // TODO: Parse and store other module attributes
    Ok(
      tokens
        .iter()
        // .filter(|t| !t.is_newline()) // throw away newlines
        .cloned()
        .collect(),
    )
  }
}
