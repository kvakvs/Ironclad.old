//! Parses Erlang source into AST

use crate::project::ErlProject;
use std::sync::{Arc, RwLock};
use libironclad_error::ic_error::IcResult;
use crate::stage::file_contents_cache::FileContentsCache;
use crate::project::module::ErlModule;

/// Handles parsing loaded Erlang files in the project
pub struct ErlParseStage {}

impl ErlParseStage {
  /// Parse stage
  /// * Parse loaded ERL files as Erlang.
  /// Returns: Collection of AST trees for all affected ERL modules
  pub fn run(project: &mut ErlProject,
             contents_cache: Arc<RwLock<FileContentsCache>>) -> IcResult<()> {
    if let Ok(contents_cache_r) = contents_cache.read() {
      for (path, source_file) in &contents_cache_r.all_files {
        let path_s = path.to_string_lossy();

        // Take only .erl and .hrl files
        if path_s.ends_with(".erl") || path_s.ends_with(".hrl") {
          let compiler_opts = project.get_compiler_options_for(path);

          let mut parsed = ErlModule::from_module_source(&source_file.file_name, &source_file.text)?;
          parsed.compiler_options = compiler_opts;
        }
      }
    }

    Ok(())
  }
}