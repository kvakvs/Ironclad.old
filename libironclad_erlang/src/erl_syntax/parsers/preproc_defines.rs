//! Parser scope for the current translation unit, contains currently known macros, records etc
use crate::erl_syntax::preprocessor::pp_define::{PreprocessorDefine, PreprocessorDefineImpl};
use libironclad_util::mfarity::MFArity;
use std::collections::HashMap;

/// Collection of preprocessor defines with arity as key
#[derive(Default, Debug, Clone)]
pub struct PreprocessorDefinesMap {
  /// The data
  pub data: HashMap<MFArity, PreprocessorDefine>,
}

impl FromIterator<(MFArity, PreprocessorDefine)> for PreprocessorDefinesMap {
  fn from_iter<T>(iter: T) -> Self
  where
    T: IntoIterator<Item = (MFArity, PreprocessorDefine)>,
  {
    let mut data = HashMap::new();
    for (arity, define) in iter {
      data.insert(arity, define);
    }
    Self { data }
  }
}

impl PreprocessorDefinesMap {
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
      PreprocessorDefinesMap::new_from_config_lines(inputs)
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

    for (na, def) in another.data.iter() {
      result.data.insert(na.clone(), def.clone());
    }

    result
  }
}
