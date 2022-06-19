//! Parses Erlang source into AST

use libironclad_erlang::error::ic_error::IcResult;
use libironclad_erlang::file_cache::FileCache;
use libironclad_erlang::project::erl_module::ErlModuleImpl;
use libironclad_erlang::project::ErlProject;
use libironclad_erlang::stats::time_stats::TimeStatsImpl;

/// Handles parsing loaded Erlang files in the project
pub struct ErlParseStage {}

impl ErlParseStage {
  /// Parse stage
  /// * Parse loaded ERL files as Erlang.
  /// Returns: Collection of AST trees for all affected ERL modules
  pub fn run_parse_stage(project: &ErlProject, contents_cache: FileCache) -> IcResult<()> {
    let mut stage_time = TimeStatsImpl::default();

    if let Ok(contents_cache_r) = contents_cache.read() {
      for (path, source_file) in &contents_cache_r.all_files {
        let path_s = path.to_string_lossy();

        // Take only .erl and .hrl files
        if path_s.ends_with(".erl") || path_s.ends_with(".hrl") {
          let compiler_opts = project.get_compiler_options_for(path);

          let mut file_time = TimeStatsImpl::default();
          // let module = ErlModuleImpl::new(compiler_opts, source_file.clone());
          // let tok_stream =
          //   module.tokenize_helper(project.clone(), source_file.clone(), tokenize_source)?;

          let module =
            ErlModuleImpl::from_module_source(project, source_file, Some(compiler_opts.clone()))?;

          project.register_new_module(module);

          file_time.stop_timer();
          println!("FILE {} - {}", stage_time, source_file.file_name.to_string_lossy());
        }

        if cfg!(debug_assertions) {
          break;
        }
      }
    }

    stage_time.stop_timer();
    println!("PARSE stage: {}", stage_time);

    Ok(())
  }
}
