//! Erlang project contains all settings for input files and compiler options
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::path::{Path, PathBuf};
use std::sync::{Arc};

use glob::glob;

use crate::erl_error::{ErlError, ErlResult};
use crate::project::compiler_opts::CompilerOpts;
use crate::project::conf::ProjectConf;
use crate::project::input_opts::InputOpts;
use crate::stage::file_preload::FilePreloadStage;
use crate::stage::parse::ErlParseStage;
use crate::stage::preprocess::ErlPreprocessStage;

pub mod conf;
pub mod compiler_opts;
pub mod input_opts;
pub mod source_file;
pub mod module;

/// Same as ErlProjectConf but no Option<> fields
#[derive(Debug)]
pub struct ErlProject {
  /// Input search paths, output paths, flags, ... etc. Shared with all modules which use default
  /// compile options
  pub compiler_opts: Arc<CompilerOpts>,

  /// Input files and directories (wildcards are allowed)
  input_opts: InputOpts,

  /// Prepared paths, scanned from Self::inputs, and with exclusions filtered out
  pub inputs: Vec<PathBuf>,
}

impl ErlProject {
  /// Return preprocessor symbols defined for a given file
  pub(crate) fn get_preprocessor_symbols(&self, _path: &Path) -> HashMap<String, String> {
    let mut result: HashMap<String, String> = Default::default();

    self.compiler_opts.defines.iter()
        .for_each(|def| {
          Self::parse_preprocessor_define(def, &mut result);
        });

    result
  }

  /// Given NAME=VALUE or NAME style option, convert it into a record in preprocessor definition
  /// symbols table. This will be passed then to preprocessor parser.
  fn parse_preprocessor_define(key_value: &str, _symbols: &mut HashMap<String, String>) {
    println!("Config preproc opt: {}", key_value);
  }

  /// Get a clone of project compiler options
  /// TODO: special override options if user specifies extras as a module attribute
  pub(crate) fn get_compiler_options_for(&self, _path: &Path) -> Arc<CompilerOpts> {
    self.compiler_opts.clone()
  }

  /// Default file dict capacity
  pub const DEFAULT_CAPACITY: usize = 1024; // preallocate this many inputs in the file_list

  /// Traverse directories starting from each of the inputs.directories;
  /// Add files from inputs if not duplicate.
  pub fn build_file_list(&self) -> ErlResult<Vec<PathBuf>> {
    let mut file_set: HashSet<PathBuf> = HashSet::with_capacity(ErlProject::DEFAULT_CAPACITY);
    let mut file_list = Vec::new();

    for file_mask in &self.input_opts.files {
      for dir in &self.input_opts.directories {
        let file_glob = String::from(dir) + "/**/" + file_mask.as_str();

        for entry in glob(&file_glob)? {
          match entry {
            Ok(path) => Self::maybe_add_path(&mut file_set, &mut file_list, path)?,
            Err(err) => return Err(ErlError::from(err))
          }
        } // for glob search results
      } // for input dirs
    } // for input file masks

    Ok(file_list)
  }

  /// Check exclusions in the Self.input. Hashset is used to check for duplicates. Add to Vec.
  fn maybe_add_path(file_set: &mut HashSet<PathBuf>,
                    file_list: &mut Vec<PathBuf>,
                    path: PathBuf) -> ErlResult<()> {
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

  /// Preprocesses and attempts to parse AST in all input files
  pub fn compile(&mut self, inputs: Vec<PathBuf>) -> ErlResult<()> {
    self.inputs = inputs;

    // Load files and store contents in the hashmap
    let file_cache = FilePreloadStage::run(self)?;

    // Preprocess erl files, and store preprocessed PpAst in a new hashmap
    let _pp_ast_cache = ErlPreprocessStage::run(self, file_cache.clone())
        .unwrap();

    // Parse all ERL and HRL files
    let _erl_code_cache = ErlParseStage::run(self, file_cache)
        .unwrap();

    Ok(())
  }
}

impl From<ProjectConf> for ErlProject {
  fn from(conf: ProjectConf) -> Self {
    Self {
      compiler_opts: Arc::new(CompilerOpts::from(conf.compiler_opts)),
      input_opts: InputOpts::from(conf.inputs),
      inputs: Vec::default(),
    }
  }
}
