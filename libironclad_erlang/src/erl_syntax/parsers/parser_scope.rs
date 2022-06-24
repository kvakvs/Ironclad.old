//! Parser scope for the current translation unit, contains currently known macros, records etc

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};

use crate::erl_syntax::preprocessor::pp_define::{PreprocessorDefine, PreprocessorDefineImpl};
use crate::erl_syntax::preprocessor::pp_name_arity::NameArity;
use crate::erl_syntax::token_stream::token::Token;
use crate::error::ic_error::{IcResult, IroncladError};
use crate::error::ic_error_trait::IcError;
use crate::file_cache::FileCache;
use crate::project::project_impl::ErlProjectImpl;
use crate::project::ErlProject;
use crate::source_file::SourceFileImpl;
use crate::source_loc::SourceLoc;

/// Collection of preprocessor defines with arity as key
pub type PreprocessorDefinesMap = HashMap<NameArity, PreprocessorDefine>;

/// Currently available defines for a file, as the file is scanned, this is constantly updated with
/// defines added `-define` and removed `-undef`.
/// This is mutated as we descend into module AST and meet more `-define/undef` directives and
/// include more files.
#[deprecated = "merged with RootScope"]
pub struct ParserScopeImpl {
  /// Available macros
  pub defines: RwLock<PreprocessorDefinesMap>,
  /// Access to the file loader
  pub file_cache: FileCache,
  /// Access to the project (compiler options, inputs global and per file etc)
  pub project: ErlProject,
}

/// Wrapped with `Arc<>` for convenience.
#[deprecated]
pub type ParserScope = Arc<ParserScopeImpl>;

impl Clone for ParserScopeImpl {
  fn clone(&self) -> Self {
    Self {
      defines: self.defines.read().unwrap().clone().into(),
      file_cache: self.file_cache.clone(),
      project: self.project.clone(),
    }
  }
}

impl std::fmt::Debug for ParserScopeImpl {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "Scope[ defines={:?} ]", &self.defines.read().unwrap())
  }
}

impl ParserScopeImpl {
  /// Create new parser scope with no project and no cache, useful for testing
  pub fn new_empty() -> Self {
    Self {
      defines: Default::default(),
      file_cache: FileCache::default(),
      project: ErlProjectImpl::default().into(),
    }
  }

  /// Create new parser scope from a project
  pub fn new_from_project(project: ErlProject, path: &Path) -> ParserScope {
    Self {
      defines: project.get_preprocessor_scope(path).into(),
      file_cache: FileCache::default(), // TODO get the cache?
      project,
    }
    .into()
  }

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

  /// Check if name of any arity exists in the scope
  pub fn is_defined(&self, name: &str) -> bool {
    if let Ok(r_defines) = self.defines.read() {
      r_defines
        .iter()
        .any(|(name_arity, _)| name_arity.name == name)
    } else {
      panic!("Can't lock ParserScope.defines for reading")
    }
  }

  /// Check if name of arity exists in the scope
  #[allow(dead_code)]
  pub(crate) fn is_defined_with_arity(&self, name: &str, arity: usize) -> bool {
    println!("Is defined {}/{}? self={:?}", name, arity, self);
    if let Ok(r_defines) = self.defines.read() {
      r_defines
        .iter()
        .any(|(name_arity, _)| name_arity.name == name && name_arity.arity == arity)
    } else {
      panic!("Can't lock ParserScope.defines for reading")
    }
  }

  /// Clone self and insert a new macro definition
  #[allow(dead_code)]
  pub(crate) fn define(&self, name: &str, args: &[String], tokens: &[Token]) {
    let pp_def = PreprocessorDefineImpl::new(name.to_string(), args, tokens);
    if let Ok(mut w_defines) = self.defines.write() {
      w_defines.insert(pp_def.get_name_arity(), pp_def);
    }
  }

  /// For macro with 0 arguments, produce its substitute which goes into AST tree.
  /// Returns `Some(string)` if macro() is defined, else `None`.
  pub fn get_value(&self, name: &str, arity: usize) -> Option<PreprocessorDefine> {
    if let Ok(r_defines) = self.defines.read() {
      r_defines.get(&NameArity::new(name, arity)).cloned()
    } else {
      panic!("Can't lock ParserScope.defines for get_value read")
    }
  }

  #[allow(dead_code)]
  pub(crate) fn load_include(
    &self,
    location: SourceLoc,
    find_file: &Path,
    from_file: Option<PathBuf>,
  ) -> IcResult<Arc<SourceFileImpl>> {
    let resolved_path = self
      .project
      .find_include(location.clone(), find_file, from_file)?;

    // Check if already loaded in the File Cache?
    if let Ok(mut cache) = self.file_cache.write() {
      cache.get_or_load(&resolved_path).map_err(IcError::from)
    } else {
      IroncladError::file_not_found(location, find_file, "loading an include file")
    }
  }

  /// `include_lib` is similar to `include`, but should not point out an absolute file. Instead,
  /// the first path component (possibly after variable substitution) is assumed to be the name
  /// of an application.
  ///
  /// Example:
  ///     -include_lib("kernel/include/file.hrl").
  ///
  /// The code server uses `code:lib_dir(kernel)` to find the directory of the current (latest)
  /// version of Kernel, and then the subdirectory include is searched for the file `file.hrl`.
  #[allow(dead_code)]
  fn find_include_lib(
    &self,
    _project: &ErlProjectImpl,
    location: SourceLoc,
    path: &Path,
  ) -> IcResult<PathBuf> {
    IroncladError::file_not_found(location, path, "searching for an -include_lib() path")
  }
}
