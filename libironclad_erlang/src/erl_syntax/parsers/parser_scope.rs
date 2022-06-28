//! Parser scope for the current translation unit, contains currently known macros, records etc
use crate::erl_syntax::preprocessor::pp_define::{PreprocessorDefine, PreprocessorDefineImpl};
use libironclad_util::mfarity::MFArity;
use std::collections::HashMap;

/// Collection of preprocessor defines with arity as key
pub type PreprocessorDefinesMap = HashMap<MFArity, PreprocessorDefine>;

#[deprecated]
#[allow(missing_docs)]
pub struct ParserScopeImpl {}

impl ParserScopeImpl {
  /// Parse defines in the configuration file, or from command line specified as -DNAME or -DNAME=XXX
  pub(crate) fn new_from_config_lines(inputs: &[String]) -> PreprocessorDefinesMap {
    inputs
      .iter()
      .map(|inp| {
        let new_def = PreprocessorDefineImpl::new_from_command_line(inp);
        (new_def.get_name_arity(), new_def)
      })
      .collect()
  }

  /// Create a new scope from a parsed project configuration
  pub fn new_from_config(
    maybe_inputs: Option<Vec<String>>,
    defaults: &PreprocessorDefinesMap,
  ) -> PreprocessorDefinesMap {
    if let Some(inputs) = &maybe_inputs {
      Self::new_from_config_lines(inputs)
    } else {
      defaults.clone()
    }
  }

  /// Merges two pdef maps
  pub fn overlay(
    one: &PreprocessorDefinesMap,
    another: &PreprocessorDefinesMap,
  ) -> PreprocessorDefinesMap {
    let mut result = one.clone();

    for (na, def) in another.iter() {
      result.insert(na.clone(), def.clone());
    }

    result
  }
}
