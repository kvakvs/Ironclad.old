//! Preprocessor definition, sometimes with args

use crate::erl_syntax::preprocessor::pp_name_arity::NameArity;
use std::sync::Arc;

/// A preprocessor definition created by `-define(X...)` or from the project settings or
/// the command line `-DNAME=xxx`
#[derive(Debug)]
pub struct PreprocessorDefine {
  /// The name of the macro in `-define(NAME, ...)` or in command line `-DNAME` etc
  pub name: String,
  /// Arg names for the macro like `-define(MACRO(ARG1, ARG2, ...)...)`
  pub args: Vec<String>,
  /// The substitution, if provided, otherwise the symbol just evaluates as true in ifdefs and ifs
  pub text: String,
}

impl PreprocessorDefine {
  /// Create a new empty preprocessor definition without body and args
  pub fn new(name: String, args: &[String], text: &str) -> Arc<Self> {
    Self { name, args: args.into(), text: text.to_string() }.into()
  }

  /// Given NAME=VALUE or NAME style option, convert it into a record in preprocessor definition
  /// symbols table. This will be passed then to preprocessor parser.
  pub fn new_from_command_line(key_value: &str) -> Arc<PreprocessorDefine> {
    println!("TODO: new preproc-define from: {}", key_value);
    Self::new(key_value.to_string(), &Vec::default(), "")
  }

  /// Return the name/arity pair for this macro
  pub fn get_arity(&self) -> usize {
    self.args.len()
  }

  /// Construct name/arity pair
  pub fn get_name_arity(&self) -> NameArity {
    NameArity { name: self.name.clone(), arity: self.get_arity() }
  }
}