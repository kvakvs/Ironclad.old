//! Project inputs (scanned directories for input files)

use crate::project::compiler_opts::CompilerOpts;
use crate::project::input_opts::InputOpts;
use libironclad_util::rw_hashmap::RwHashMap;
use libironclad_util::rw_vec::RwVec;
use std::path::PathBuf;

/// Groups inputs for the processing (filenames, compiler options etc)
#[derive(Debug, Default)]
pub struct ErlProjectInputs {
  /// Input search paths, output paths, flags, ... etc. Shared with all modules which use default
  /// compile options
  pub compiler_opts: CompilerOpts,
  /// Compiler options but overrides on a per-file basis
  pub compiler_opts_per_file: RwHashMap<PathBuf, CompilerOpts>,
  /// Input files and directories (wildcards are allowed)
  pub input_opts: InputOpts,
  /// Prepared paths, scanned from Self::inputs, and with exclusions filtered out
  pub input_paths: RwVec<PathBuf>, // TODO: rename: input_paths
}
