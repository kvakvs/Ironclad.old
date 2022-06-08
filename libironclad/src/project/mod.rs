//! Erlang project contains all settings for input files and libironclad options
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use glob::glob;
use libironclad_erlang::error::ic_error::{IcResult, IroncladError, IroncladResult};

use crate::project::compiler_opts::CompilerOpts;
use crate::project::conf::ProjectConf;
use crate::project::input_opts::InputOpts;
use crate::stage::file_preload::FilePreloadStage;
use crate::stage::parse::ErlParseStage;
use crate::stage::preprocess::pp_scope::PreprocessorScope;
#[cfg(feature = "separate_preprocessor_lib")]
use crate::stage::preprocess::pp_stage::PreprocessStage;

pub mod compiler_opts;
pub mod conf;
pub mod input_opts;
pub mod module;

/// Groups inputs for the processing (filenames, compiler options etc)
#[derive(Debug)]
pub struct ErlProjectInputs {
  /// Input search paths, output paths, flags, ... etc. Shared with all modules which use default
  /// compile options
  pub compiler_opts: Arc<CompilerOpts>,
  /// Compiler options but overrides on a per-file basis
  pub compiler_opts_per_file: HashMap<PathBuf, Arc<CompilerOpts>>,
  /// Input files and directories (wildcards are allowed)
  pub input_opts: InputOpts,
  /// Prepared paths, scanned from Self::inputs, and with exclusions filtered out
  pub inputs: Vec<PathBuf>, // TODO: rename: input_paths
}

/// Same as ErlProjectConf but no Option<> fields
#[derive(Debug)]
pub struct ErlProject {
  /// Inputs and compile options provided from the project file and command line
  pub inputs: ErlProjectInputs,
}

impl ErlProject {
  /// Create a starting preprocessor scope for the first line of a given file.
  /// Initial scope values are derived from the commandline and the project settings for the file,
  /// and the scope evolves as the preprocessor/parser goes through the file.
  pub fn get_preprocessor_scope(&self, path: &Path) -> Arc<PreprocessorScope> {
    let mut result_scope = self.inputs.compiler_opts.scope.clone();

    // Find opts (if exist) for current file, and apply them over project global opts
    if let Some(per_file_opts) = self.inputs.compiler_opts_per_file.get(path) {
      result_scope = result_scope.overlay(&per_file_opts.scope);
    }

    result_scope
  }

  /// Get a clone of project libironclad options
  /// TODO: special override options if user specifies extras as a module attribute
  pub fn get_compiler_options_for(&self, path: &Path) -> Arc<CompilerOpts> {
    if let Some(per_file_opts) = self.inputs.compiler_opts_per_file.get(path) {
      // If found per-file settings, combine global with per-file
      self
        .inputs
        .compiler_opts
        .overlay(per_file_opts.deref())
        .into()
    } else {
      // If not found per-file settings, just provide a clone of global settings
      self.inputs.compiler_opts.clone()
    }
  }

  /// Default file dict capacity
  pub const DEFAULT_CAPACITY: usize = 1024; // preallocate this many inputs in the file_list

  /// Traverse directories starting from each of the inputs.directories;
  /// Add files from inputs if not duplicate.
  pub fn build_file_list(&self) -> IroncladResult<Vec<PathBuf>> {
    let mut file_set: HashSet<PathBuf> = HashSet::with_capacity(ErlProject::DEFAULT_CAPACITY);
    let mut file_list = Vec::new();

    for file_mask in &self.inputs.input_opts.files {
      for dir in &self.inputs.input_opts.directories {
        let file_glob = String::from(dir) + "/**/" + file_mask.as_str();

        for entry in glob(&file_glob)? {
          match entry {
            Ok(path) => Self::maybe_add_path(&mut file_set, &mut file_list, path)?,
            Err(err) => return Err(IroncladError::from(err)),
          }
        } // for glob search results
      } // for input dirs
    } // for input file masks

    Ok(file_list)
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

  /// Preprocesses and attempts to parse AST in all input files
  pub fn compile(&mut self, inputs: Vec<PathBuf>) -> IcResult<()> {
    self.inputs.inputs = inputs;

    //----------------------------------
    // READING (except includes)
    //----------------------------------
    // Load files and store contents in the hashmap
    let mut preload_stage = FilePreloadStage::default();
    let file_cache = match preload_stage.run(&self.inputs.inputs) {
      Ok(fc) => fc,
      Err(e) => return Err(Box::new(e)),
    };

    // //-------------------------
    // // PREPROCESSING
    // //-------------------------
    // // Preprocess erl files, and store preprocessed PpAst in a new hashmap
    // let mut pp_stage = PreprocessStage::new();
    // let _pp_ast_cache = pp_stage.run(self, file_cache.clone()).unwrap();

    //-------------------------
    // PARSING ERLANG AST
    //-------------------------
    // Parse all ERL files and their included includes
    ErlParseStage::run(self, file_cache).unwrap();

    Ok(())
  }
}

impl From<ProjectConf> for ErlProject {
  fn from(conf: ProjectConf) -> Self {
    Self {
      inputs: ErlProjectInputs {
        compiler_opts: Arc::new(CompilerOpts::from(conf.compiler_opts)),
        compiler_opts_per_file: Default::default(),
        input_opts: InputOpts::from(conf.inputs),
        inputs: Vec::default(),
      },
    }
  }
}
