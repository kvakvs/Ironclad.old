#![cfg(feature = "separate_preprocessor_lib")]
//! Preprocessor stage shared state for all files

use crate::project::ErlProject;
use crate::stage::file_contents_cache::FileContentsCache;
use crate::stage::preprocess::pp_stage_file::PreprocessFile;
use crate::stats::preprocessor_stats::PreprocessorStats;
use libironclad_error::ic_error::IcResult;
use libironclad_preprocessor::preprocessor_syntax::pp_ast::PpAstCache;
use std::path::PathBuf;
use std::sync::{Arc, RwLock};

/// Preprocessor stage
pub struct PreprocessStage {
  /// Counters for files/bytes read, caches, duration etc.
  stats: PreprocessorStats,
}

impl PreprocessStage {
  /// Create a preprocess state struct for processing a file.
  /// Preprocessor symbols are filled from the command line and project TOML file settings.
  pub fn new() -> Self {
    Self { stats: PreprocessorStats::new() }
  }

  /// Stage 1 - Preprocessor stage
  /// ----------------------------
  /// * Preparse loaded ERL files ignoring the syntax only paying attention to preprocess tokens.
  /// * Preparse include files AST and paste preprocess AST into include locations.
  /// * Drop AST branches covered by the conditional compile directives.
  ///
  /// Side effects: Updates file contents cache
  /// Returns preprocessed collection of module sources
  pub fn run(
    &mut self,
    project: &mut ErlProject,
    file_cache: Arc<RwLock<FileContentsCache>>,
  ) -> IcResult<Arc<RwLock<PpAstCache>>> {
    let ast_cache = RwLock::new(PpAstCache::default()).into();

    // Take only .erl files, includes will be loaded and cached while parsing
    let all_files: Vec<PathBuf> = {
      if let Ok(file_cache_r) = file_cache.read() {
        file_cache_r.all_files.keys().cloned().collect()
      } else {
        panic!("Can't lock file cache for reading")
      }
    };

    let erl_files: Vec<PathBuf> = all_files
      .into_iter()
      .filter(|path| path.to_string_lossy().ends_with(".erl"))
      .collect();

    for path in erl_files.iter() {
      // For all input files, run preprocess parse and interpret the preprocess directives
      // Loaded and parsed HRL files are cached to be inserted into every include location
      // Create starting scope (from project settings and command line)
      let starting_scope = project.get_preprocessor_scope(path);

      let mut stage_file = PreprocessFile::new(&ast_cache, &file_cache, starting_scope);
      stage_file.preprocess_file(&mut self.stats, project, path)?;
    }

    // Print stage counters
    self.stats.time.stage_finished(); // mark end time
    print!("{}", self.stats);

    Ok(ast_cache)
  }
}
