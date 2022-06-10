//! Preprocessor definition, sometimes with args

use crate::erl_syntax::preprocessor::pp_name_arity::NameArity;
use libironclad_util::pretty::Pretty;
use std::sync::Arc;

/// A preprocessor definition created by `-define(X...)` or from the project settings or
/// the command line `-DNAME=xxx`
pub struct PreprocessorDefineImpl {
  /// The name of the macro in `-define(NAME, ...)` or in command line `-DNAME` etc
  pub name: String,
  /// Arg names for the macro like `-define(MACRO(ARG1, ARG2, ...)...)`
  pub args: Vec<String>,
  /// The substitution, if provided, otherwise the symbol just evaluates as true in ifdefs and ifs
  pub text: String,
}

/// Wrapper for `Arc<>`
pub type PreprocessorDefine = Arc<PreprocessorDefineImpl>;

impl std::fmt::Debug for PreprocessorDefineImpl {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "-define({}", &self.name)?;
    Pretty::display_paren_list(&self.args, f)?;
    write!(f, ", {})", &self.text)
  }
}

impl PreprocessorDefineImpl {
  /// Create a new empty preprocessor definition without body and args
  pub(crate) fn new(name: String, args: &[String], text: &str) -> PreprocessorDefine {
    Self { name, args: args.into(), text: text.to_string() }.into()
  }

  /// Given NAME=VALUE or NAME style option, convert it into a record in preprocessor definition
  /// symbols table. This will be passed then to preprocessor parser.
  pub(crate) fn new_from_command_line(key_value: &str) -> PreprocessorDefine {
    println!("TODO: new preproc-define from: {}", key_value);
    Self::new(key_value.to_string(), &Vec::default(), "")
  }

  /// Return the name/arity pair for this macro
  pub(crate) fn get_arity(&self) -> usize {
    self.args.len()
  }

  /// Construct name/arity pair
  pub(crate) fn get_name_arity(&self) -> NameArity {
    NameArity { name: self.name.clone(), arity: self.get_arity() }
  }
}
