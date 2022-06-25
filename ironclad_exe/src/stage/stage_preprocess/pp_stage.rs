#![cfg(feature = "separate_preprocessor_lib")]
//! Preprocessor stage shared state for all files

use crate::project::ErlProject;
use ironclad::stage_preprocess::pp_stage_file::PreprocessFile;
use libironclad_erlang::error::ic_error::IcResult;
use libironclad_preprocessor::preprocessor_syntax::pp_ast::PpAstCache;
use libironclad_util::io::file_cache::FileCache;
use libironclad_util::stats::preprocessor_stats::PreprocessorStats;
use std::path::PathBuf;
use std::sync::{Arc, RwLock};

/// Preprocessor stage
#[deprecated = "Not used atm"]
pub struct PreprocessStage {
  /// Counters for files/bytes read, caches, duration etc.
  stats: PreprocessorStats,
}

impl PreprocessStage {
  /// Create a stage_preprocess state struct for processing a file.
  /// Preprocessor symbols are filled from the command line and project TOML file settings.
  pub fn new() -> Self {
    Self { stats: PreprocessorStats::new() }
  }

  /// Stage 1 - Preprocessor stage
  /// ----------------------------
  /// * Preparse loaded ERL files ignoring the syntax only paying attention to stage_preprocess tokens.
  /// * Preparse include files AST and paste stage_preprocess AST into include locations.
  /// * Drop AST branches covered by the conditional compile directives.
  ///
  /// Side effects: Updates file contents cache
  /// Returns preprocessed collection of module sources
  #[deprecated]
  pub fn run_pp_stage(
    &mut self,
    project: &mut ErlProject,
    file_cache: FileCache,
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
      // For all input files, run stage_preprocess parse and interpret the stage_preprocess directives
      // Loaded and parsed HRL files are cached to be inserted into every include location
      // Create starting scope (from project settings and command line)
      let starting_scope = project.get_preprocessor_scope(path);

      let mut stage_file = PreprocessFile::new(&ast_cache, &file_cache, starting_scope);
      stage_file.preprocess_file(&mut self.stats, project, path)?;
    }

    // Print stage counters
    self.stats.time.stage_finished(); // mark end time
    print!("{}", self.stats.read().unwrap());

    Ok(ast_cache)
  }
}