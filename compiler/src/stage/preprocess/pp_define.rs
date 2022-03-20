//! Preprocessor definition, sometimes with args

use std::sync::Arc;

/// A key to preprocessor defines dictionary, as defines can coexist with same name but different
/// number of args
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct NameArity {
  /// Name for namearity pair
  pub name: String,
  /// The count of arguments, arity
  pub arity: usize,
}

/// A preprocessor definition created by `-define(X...)` or from the project settings or
/// the command line `-DNAME=xxx`
#[derive(Debug)]
pub struct PreprocessorDefine {
  /// The name of the macro in `-define(NAME, ...)` or in command line `-DNAME` etc
  pub name: String,
  /// Arg names for the macro like `-define(MACRO(ARG1, ARG2, ...)...)`
  pub args: Option<Vec<String>>,
  /// The substitution, if provided, otherwise the symbol just evaluates as true in ifdefs and ifs
  pub text: Option<String>,
}

impl PreprocessorDefine {
  /// Create a new empty preprocessor definition without body and args
  pub fn new(name: &str, args: Option<Vec<String>>, text: Option<String>) -> Arc<Self> {
    Self { name: name.to_string(), args, text }.into()
  }

  /// Given NAME=VALUE or NAME style option, convert it into a record in preprocessor definition
  /// symbols table. This will be passed then to preprocessor parser.
  pub fn new_from_command_line(key_value: &str) -> Arc<PreprocessorDefine> {
    println!("TODO: new preproc-define from: {}", key_value);
    Self::new(key_value, None, None)
  }

  /// Return the name/arity pair for this macro
  pub fn get_arity(&self) -> usize {
    if let Some(args) = &self.args {
      args.len()
    } else {
      0
    }
  }

  /// Construct name/arity pair
  pub fn get_name_arity(&self) -> NameArity {
    NameArity { name: self.name.clone(), arity: self.get_arity() }
  }
}
