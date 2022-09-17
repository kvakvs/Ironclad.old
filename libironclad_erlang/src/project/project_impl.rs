//! Erlang project (with inputs defined in the config file)

use crate::error::ic_error::{IroncladError, IroncladResult};
use crate::project::compiler_opts::{CompilerOpts, CompilerOptsImpl};
use crate::project::conf::ProjectConf;
use crate::project::input_opts::InputOpts;
use crate::project::module::module_impl::ErlModule;
use crate::project::project_inputs::ErlProjectInputs;
use libironclad_util::io::file_cache::FileCache;
use libironclad_util::rw_hashmap::RwHashMap;
use libironclad_util::rw_vec::RwVec;
use libironclad_util::source_file::SourceFile;
use std::collections::HashSet;
use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::path::{Path, PathBuf};

/// Same as ErlProjectConf but no Option<> fields
#[derive(Default, Debug)]
pub struct ErlProjectImpl {
  /// Inputs and compile options provided from the project file and command line
  pub project_inputs: ErlProjectInputs,
  /// Collection of loaded modules
  pub modules: RwHashMap<String, ErlModule>,
  /// Stores files recently loaded from disk
  pub file_cache: FileCache,
}

impl ErlProjectImpl {
  /// Get a clone of project libironclad options
  /// TODO: special override options if user specifies extras as a module attribute
  pub fn get_compiler_options_for(&self, path: &Path) -> CompilerOpts {
    // sad reality of generic get having arg of &PathBuf and not &Path
    let pb = PathBuf::from(path);

    if let Some(per_file_opts) = self.project_inputs.compiler_opts_per_file.get(&pb) {
      // If found per-file settings, combine global with per-file
      self
        .project_inputs
        .compiler_opts
        .overlay(per_file_opts.deref())
        .into()
    } else {
      // If not found per-file settings, just provide a clone of global settings
      self.project_inputs.compiler_opts.clone()
    }
  }

  /// Default file dict capacity
  pub const DEFAULT_CAPACITY: usize = 1024; // preallocate this many inputs in the file_list

  /// Traverse directories starting from each of the inputs.directories;
  /// Add files from inputs if not duplicate.
  pub fn build_file_list(&self) -> IroncladResult<()> {
    let mut file_set: HashSet<PathBuf> = HashSet::with_capacity(ErlProjectImpl::DEFAULT_CAPACITY);
    let mut file_list = Vec::new();

    for file_mask in &self.project_inputs.input_opts.files {
      for dir in &self.project_inputs.input_opts.directories {
        let file_glob = String::from(dir) + "/**/" + file_mask.as_str();

        for entry in glob::glob(&file_glob).map_err(|e| IroncladError::from(e))? {
          match entry {
            Ok(path) => Self::maybe_add_path(&mut file_set, &mut file_list, path)?,
            Err(err) => return Err(IroncladError::from(err).into()),
          }
        } // for glob search results
      } // for input dirs
    } // for input file masks

    self
      .project_inputs
      .input_paths
      .replace(file_list.iter().cloned());
    Ok(())
  }

  /// Check exclusions in the Self.input. Hashset is used to check for duplicates. Add to Vec.
  fn maybe_add_path(
    file_set: &mut HashSet<PathBuf>,
    file_list: &mut Vec<PathBuf>,
    path: PathBuf,
  ) -> IroncladResult<()> {
    // Check duplicate
    let abs_path = std::fs::canonicalize(path).map_err(|e| IroncladError::from(e))?;
    if file_set.contains(&abs_path) {
      return Ok(());
    }

    // Success: checks passed, add to the input list
    file_set.insert(abs_path.clone());
    file_list.push(abs_path);

    Ok(())
  }

  /// Register a new module
  pub fn register_new_module(&self, module: &ErlModule) {
    let m_name = module.get_name();
    self.modules.add(m_name, module.clone())
  }

  /// Retrieve a source file from the file cache, load if necessary
  pub fn get_source_file(&self, path: &Path) -> IroncladResult<SourceFile> {
    self
      .file_cache
      .get_or_load(path)
      .map_err(|e| IroncladError::from(e).into())
  }
}

impl From<ProjectConf> for ErlProjectImpl {
  fn from(conf: ProjectConf) -> Self {
    let inputs = ErlProjectInputs {
      compiler_opts: CompilerOptsImpl::new_from_maybe_opts(conf.compiler_options).into(),
      compiler_opts_per_file: Default::default(),
      input_opts: InputOpts::from(conf.inputs),
      input_paths: RwVec::default(),
    };
    Self {
      project_inputs: inputs,
      modules: RwHashMap::default(),
      file_cache: FileCache::default(),
    }
  }
}

impl Display for ErlProjectImpl {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    let paths = &self.project_inputs.compiler_opts.include_paths;
    write!(f, "ErlProject[glob_include={:?}]", paths)
  }
}
