use std::fmt::Debug;

use crate::erl_error::{ErlError, ErlResult};
use crate::project::compiler_opts::CompilerOpts;
use crate::project::input_opts::InputOpts;
use crate::project::conf::ErlProjectConf;
use glob::{glob};
use std::path::{PathBuf, Path};
use std::collections::{HashSet, HashMap};
use crate::stage;
use std::sync::Arc;

pub mod conf;
pub mod compiler_opts;
pub mod input_opts;
pub mod source_file;

/// Same as ErlProjectConf but no Option<> fields
#[derive(Debug)]
pub struct ErlProject {
  /// Input search paths, output paths, flags, ... etc. Shared with all modules which use default
  /// compile options
  pub compiler_opts: Arc<CompilerOpts>,

  /// Input files and directories (wildcards are allowed)
  inputs: InputOpts,

  /// Prepared paths, scanned from Self::inputs, and with exclusions filtered out
  pub file_set: HashSet<PathBuf>,
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

  pub const DEFAULT_CAPACITY: usize = 1024; // preallocate this many inputs in the file_list

  /// Traverse directories starting from each of the inputs.directories;
  /// Add files from inputs if not duplicate.
  pub fn build_file_list(&self) -> ErlResult<HashSet<PathBuf>> {
    let mut file_set: HashSet<PathBuf> = HashSet::with_capacity(ErlProject::DEFAULT_CAPACITY);

    for file_mask in &self.inputs.files {
      for dir in &self.inputs.directories {
        let file_glob = String::from(dir) + "/**/" + file_mask.as_str();
        println!("Globbing: {}", file_glob);

        for entry in glob(&file_glob)? {
          match entry {
            Ok(path) => Self::maybe_add_path(&mut file_set, path)?,
            Err(err) => return Err(ErlError::from(err))
          }
        } // for glob search results
      } // for input dirs
    } // for input file masks

    Ok(file_set)
  }

  /// Check exclusions in the Self.input and also check duplicate paths
  fn maybe_add_path(file_set: &mut HashSet<PathBuf>, path: PathBuf) -> ErlResult<()> {
    // Check duplicate
    let abs_path = std::fs::canonicalize(path)?;
    if file_set.contains(&abs_path) {
      // println!("- skip dup: {:?}", abs_path);
      return Ok(());
    }

    // Success: checks passed, add to the input list
    // println!("Input: {:?}", abs_path);
    file_set.insert(abs_path);

    Ok(())
  }

  pub fn compile(mut project: ErlProject) -> ErlResult<()> {
    // Load files and store contents in the hashmap
    let file_cache = stage::s0_preload::run(&mut project)?;

    // Preprocess erl files, and store preprocessed PpAst in a new hashmap
    let _pp_ast_cache = stage::s1_preprocess::run(&mut project, file_cache.clone())
        .unwrap();

    // Parse all ERL and HRL files
    let _erl_ast_cache = stage::s2_parse::run(&mut project, file_cache)
        .unwrap();

    Ok(())
  }
}

impl From<ErlProjectConf> for ErlProject {
  fn from(conf: ErlProjectConf) -> Self {
    Self {
      compiler_opts: Arc::new(CompilerOpts::from(conf.compiler_opts)),
      inputs: InputOpts::from(conf.inputs),
      file_set: HashSet::new(),
    }
  }
}