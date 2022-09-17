//! Handling of include files
use crate::error::ic_error::{IroncladError, IroncladResult};
use crate::project::project_impl::ErlProjectImpl;
use crate::source_loc::SourceLoc;
use std::path::{Path, PathBuf};

impl ErlProjectImpl {
  fn find_include_in(sample: &Path, try_dirs: &[String]) -> Option<PathBuf> {
    for dir in try_dirs {
      let try_path = Path::new(&dir).join(sample);
      println!("Trying path: {}", try_path.to_string_lossy());
      if try_path.exists() {
        return Some(try_path);
      }
    }
    None
  }

  /// Check include paths to find the file.
  /// File, a string, is to point out a file. The contents of this file are included as is, at the
  /// position of the directive.
  ///
  /// Include files are typically used for record and macro definitions that are shared by several
  /// modules. It is recommended to use the file name extension .hrl for include files.
  ///
  /// File can start with a path component $VAR, for some string VAR. If that is the case, the value
  /// of the environment variable VAR as returned by os:getenv(VAR) is substituted for $VAR.
  /// If os:getenv(VAR) returns false, $VAR is left as is.
  ///
  /// If the filename File is absolute (possibly after variable substitution), the include file
  /// with that name is included. Otherwise, the specified file is searched for in the following
  /// directories, and in this order:
  ///
  /// * The current working directory
  /// * The directory where the module is being compiled
  /// * The directories given by the include option
  pub(crate) fn find_include(
    &self,
    location: SourceLoc,
    find_file: &Path,
    from_file: Option<PathBuf>,
  ) -> IroncladResult<PathBuf> {
    if find_file.is_absolute() {
      return Ok(find_file.to_path_buf());
    }
    // TODO: Support $VAR components in paths

    // Try current working directory
    if let Some(try_loc) = Self::find_include_in(find_file, &[".".to_string()]) {
      return Ok(try_loc);
    }

    // Try current compiled file directory
    if let Some(current_source) = &from_file {
      let current_source_dir = current_source
        .parent()
        .unwrap()
        .to_string_lossy()
        .to_string();
      if let Some(try_loc) = Self::find_include_in(find_file, &[current_source_dir]) {
        return Ok(try_loc);
      }
    }

    // Try find in local search paths for file
    if let Some(from_file1) = &from_file {
      if let Some(opts_per_file) = self.project_inputs.compiler_opts_per_file.get(from_file1) {
        if let Some(try_loc) = Self::find_include_in(find_file, &opts_per_file.include_paths) {
          return Ok(try_loc);
        }
      }
    }

    // Try find in global search paths for project
    if let Some(try_glob) =
      Self::find_include_in(find_file, &self.project_inputs.compiler_opts.include_paths)
    {
      Ok(try_glob)
    } else {
      IroncladError::file_not_found(location, find_file, "searching for an -include() path")
    }
  }
}
