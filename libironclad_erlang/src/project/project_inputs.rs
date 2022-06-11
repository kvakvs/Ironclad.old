//! Project inputs (scanned directories for input files)

use crate::project::compiler_opts::CompilerOpts;
use crate::project::input_opts::InputOpts;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

/// Groups inputs for the processing (filenames, compiler options etc)
#[derive(Debug, Default)]
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
