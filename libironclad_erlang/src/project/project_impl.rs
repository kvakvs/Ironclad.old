//! Erlang project (with inputs defined in the config file)

use crate::erl_syntax::parsers::parser_scope::{ParserScopeImpl, PreprocessorDefinesMap};
use crate::error::ic_error::{IcResult, IroncladError, IroncladResult};
use crate::project::compiler_opts::CompilerOpts;
use crate::project::conf::ProjectConf;
use crate::project::input_opts::InputOpts;
use crate::project::project_inputs::ErlProjectInputs;
use crate::source_loc::SourceLoc;
use std::collections::HashSet;
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};

/// Same as ErlProjectConf but no Option<> fields
#[derive(Default, Debug)]
pub struct ErlProjectImpl {
  /// Inputs and compile options provided from the project file and command line
  pub inputs: RwLock<ErlProjectInputs>,
}

impl ErlProjectImpl {
  /// Create a starting preprocessor scope for the first line of a given file.
  /// Initial scope values are derived from the commandline and the project settings for the file,
  /// and the scope evolves as the preprocessor/parser goes through the file.
  pub fn get_preprocessor_scope(&self, path: &Path) -> PreprocessorDefinesMap {
    let mut result_scope = if let Ok(r_inputs) = self.inputs.read() {
      r_inputs.compiler_opts.scope.clone()
    } else {
      panic!("Can't lock project inputs for read")
    };

    // Find opts (if exist) for current file, and apply them over project global opts
    if let Ok(r_inputs) = self.inputs.read() {
      if let Some(per_file_opts) = r_inputs.compiler_opts_per_file.get(path) {
        let new_scope = ParserScopeImpl::overlay(&result_scope, &per_file_opts.scope);
        result_scope = new_scope;
      }
    } else {
      panic!("Can't lock project inputs for read")
    }

    result_scope
  }

  /// Get a clone of project libironclad options
  /// TODO: special override options if user specifies extras as a module attribute
  pub fn get_compiler_options_for(&self, path: &Path) -> Arc<CompilerOpts> {
    if let Ok(r_inputs) = self.inputs.read() {
      if let Some(per_file_opts) = r_inputs.compiler_opts_per_file.get(path) {
        // If found per-file settings, combine global with per-file
        r_inputs.compiler_opts.overlay(per_file_opts.deref()).into()
      } else {
        // If not found per-file settings, just provide a clone of global settings
        r_inputs.compiler_opts.clone()
      }
    } else {
      panic!("Can't lock project inputs to read the compiler opts")
    }
  }

  /// Default file dict capacity
  pub const DEFAULT_CAPACITY: usize = 1024; // preallocate this many inputs in the file_list

  /// Traverse directories starting from each of the inputs.directories;
  /// Add files from inputs if not duplicate.
  pub fn build_file_list(&self) -> IroncladResult<()> {
    let mut file_set: HashSet<PathBuf> = HashSet::with_capacity(ErlProjectImpl::DEFAULT_CAPACITY);
    let mut file_list = Vec::new();

    if let Ok(r_inputs) = self.inputs.read() {
      for file_mask in &r_inputs.input_opts.files {
        for dir in &r_inputs.input_opts.directories {
          let file_glob = String::from(dir) + "/**/" + file_mask.as_str();

          for entry in glob::glob(&file_glob)? {
            match entry {
              Ok(path) => Self::maybe_add_path(&mut file_set, &mut file_list, path)?,
              Err(err) => return Err(IroncladError::from(err)),
            }
          } // for glob search results
        } // for input dirs
      } // for input file masks
    } else {
      panic!("Can't lock project inputs to build file list")
    }

    if let Ok(mut w_inputs) = self.inputs.write() {
      w_inputs.inputs = file_list;
    } else {
      panic!("Can't lock project inputs to update the file list")
    }
    Ok(())
  }

  /// Check exclusions in the Self.input. Hashset is used to check for duplicates. Add to Vec.
  fn maybe_add_path(
    file_set: &mut HashSet<PathBuf>,
    file_list: &mut Vec<PathBuf>,
    path: PathBuf,
  ) -> IroncladResult<()> {
    // Check duplicate
    let abs_path = std::fs::canonicalize(path)?;
    if file_set.contains(&abs_path) {
      return Ok(());
    }

    // Success: checks passed, add to the input list
    file_set.insert(abs_path.clone());
    file_list.push(abs_path);

    Ok(())
  }

  fn find_include_in(sample: &Path, try_dirs: &[String]) -> Option<PathBuf> {
    for dir in try_dirs {
      let try_path = Path::new(&dir).join(sample);
      println!("Trying path: {}", try_path.to_string_lossy());
      if try_path.exists() {
        return Some(try_path);
      }
    }
    None
  }

  /// Check include paths to find the file.
  pub(crate) fn find_include(
    &self,
    location: SourceLoc,
    find_file: &Path,
    from_file: Option<PathBuf>,
  ) -> IcResult<PathBuf> {
    if let Ok(r_inputs) = self.inputs.read() {
      println!("inp {:?}", r_inputs);

      // Try find in local search paths for file
      if let Some(from_file1) = &from_file {
        if let Some(opts_per_file) = r_inputs.compiler_opts_per_file.get(from_file1) {
          if let Some(try_loc) = Self::find_include_in(find_file, &opts_per_file.include_paths) {
            return Ok(try_loc);
          }
        }
      }

      // Try find in global search paths
      if let Some(try_glob) =
        Self::find_include_in(find_file, &r_inputs.compiler_opts.include_paths)
      {
        Ok(try_glob)
      } else {
        IroncladError::file_not_found(location, find_file, "searching for an -include() path")
      }
    } else {
      panic!("Can't lock project inputs to resolve include path")
    }
  }
}

impl From<ProjectConf> for ErlProjectImpl {
  fn from(conf: ProjectConf) -> Self {
    let inputs = ErlProjectInputs {
      compiler_opts: CompilerOpts::new_from_maybe_opts(conf.compiler_opts).into(),
      compiler_opts_per_file: Default::default(),
      input_opts: InputOpts::from(conf.inputs),
      inputs: Vec::default(),
    };
    Self { inputs: RwLock::new(inputs) }
  }
}
