//! Preprocessor scope for the current file, currently available defines

use std::collections::HashMap;
use std::sync::{Arc, RwLock};

use crate::erl_syntax::preprocessor::pp_define::{PreprocessorDefine, PreprocessorDefineImpl};
use crate::erl_syntax::preprocessor::pp_name_arity::NameArity;

/// Currently available defines for a file, as the file is scanned, this is constantly updated with
/// defines added `-define` and removed `-undef`.
#[derive(Default, Clone)]
pub struct PreprocessorScopeImpl {
  /// Available macros
  pub defines: HashMap<NameArity, PreprocessorDefine>,
}

/// Wrapped with `Arc<>` for convenience.
pub type PreprocessorScope = Arc<RwLock<PreprocessorScopeImpl>>;

impl std::fmt::Debug for PreprocessorScopeImpl {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "Scope[ defines={:?} ]", &self.defines)
  }
}

impl PreprocessorScopeImpl {
  /// Parse defines in the configuration file, or from command line specified as -DNAME or -DNAME=XXX
  pub(crate) fn new_from_config_lines(inputs: &[String]) -> PreprocessorScope {
    let parsed = inputs
      .iter()
      .map(|inp| {
        let new_def = PreprocessorDefineImpl::new_from_command_line(inp);
        (new_def.get_name_arity(), new_def)
      })
      .collect();
    RwLock::new(PreprocessorScopeImpl { defines: parsed }).into()
  }

  /// Create a new scope from a parsed project configuration
  pub fn new_from_config(
    maybe_inputs: Option<Vec<String>>,
    defaults: &PreprocessorScope,
  ) -> PreprocessorScope {
    if let Some(inputs) = &maybe_inputs {
      Self::new_from_config_lines(inputs)
    } else {
      defaults.clone()
    }
  }

  /// Clones self and overlays values from `other`, merging them together.
  pub fn overlay(one: &Self, another: &Self) -> PreprocessorScope {
    let mut result = one.clone();
    for (na, def) in another.defines.iter() {
      result.defines.insert(na.clone(), def.clone());
    }
    RwLock::new(result).into()
  }

  /// Check if name of any arity exists in the scope
  #[allow(dead_code)]
  pub(crate) fn is_defined(&self, name: &str) -> bool {
    self
      .defines
      .iter()
      .any(|(name_arity, _)| name_arity.name == name)
  }

  /// Check if name of arity exists in the scope
  pub(crate) fn is_defined_with_arity(&self, name: &str, arity: usize) -> bool {
    println!("Is defined {}/{}? self={:?}", name, arity, self);
    self
      .defines
      .iter()
      .any(|(name_arity, _)| name_arity.name == name && name_arity.arity == arity)
  }

  /// Clone self and insert a new macro definition
  pub(crate) fn define(&mut self, name: &str, args: &[String], text: &str) {
    let pp_def = PreprocessorDefineImpl::new(name.to_string(), args, text);
    self.defines.insert(pp_def.get_name_arity(), pp_def);
  }

  // /// Clone self and remove the name
  // #[allow(dead_code)]
  // pub(crate) fn undefine(&self, name: &str) -> PreprocessorScope {
  //   let mut defines: HashMap<NameArity, PreprocessorDefine> = Default::default();
  //   for (na, ppdef) in self.defines.iter() {
  //     if na.name != name {
  //       defines.insert(na.clone(), ppdef.clone());
  //     }
  //   }
  //   PreprocessorScopeImpl { defines }.into()
  // }

  /// For macro with 0 arguments, produce its substitute which goes into AST tree.
  /// Returns `Some(string)` if macro() is defined, else `None`.
  pub(crate) fn get_value(&self, name: &str, arity: usize) -> Option<PreprocessorDefine> {
    self.defines.get(&NameArity::new(name, arity)).cloned()
  }
}
