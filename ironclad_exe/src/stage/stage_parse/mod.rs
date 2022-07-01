//! Parses Erlang source into AST

use libironclad_erlang::error::ic_error::IcResult;
use libironclad_erlang::project::module::module_impl::ErlModuleImpl;
use libironclad_erlang::project::ErlProject;
use libironclad_util::stats::time_stats::TimeStatsImpl;
use std::path::Path;

/// Handles parsing loaded Erlang files in the project
pub struct ErlParseStage {}

impl ErlParseStage {
  /// Files acceptable as translation units (HRL headers are included independently without checking
  /// for file extension)
  #[inline]
  fn is_acceptable_input(path: &Path) -> bool {
    path.extension().unwrap_or_default() == "erl"
  }

  /// Parse stage
  /// * Parse loaded ERL files as Erlang.
  /// Returns: Collection of AST trees for all affected ERL modules
  pub fn run_parse_stage(project: &ErlProject) -> IcResult<()> {
    let mut stage_time = TimeStatsImpl::default();
    let inputs = project.project_inputs.input_paths.clone_contents();

    for path in inputs.iter() {
      // Take only .erl and .hrl files
      if Self::is_acceptable_input(path) {
        let compiler_opts = project.get_compiler_options_for(path);
        let mut operation_timer = TimeStatsImpl::default();

        let source_file = project.get_source_file(path)?;
        // println!("FILE {}", source_file.file_name.to_string_lossy());

        let module =
          ErlModuleImpl::from_module_source(project, &source_file, Some(compiler_opts.clone()))?;
        project.register_new_module(&module);

        // Check for possible errors, like expressions containing wrong types of nodes
        module.verify_parsed_integrity()?;

        operation_timer.stop_timer();
        println!("FILE {} - {}", operation_timer, source_file.file_name.to_string_lossy());

        if module.has_errors() {
          module.print_errors()
        }
      }
    }

    stage_time.stop_timer();
    println!("PARSE stage: {}", stage_time);

    Ok(())
  }
}
