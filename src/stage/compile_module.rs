use std::path::{PathBuf, Path};
use crate::project::compiler_opts::CompilerOpts;
use crate::types::ArcRw;

pub struct CompileModule {
  pub in_file: PathBuf,

  /// Input filename with extension replaced with .BEAM
  pub out_file: PathBuf,

  /// The module name from -module()
  pub module_name: String,

  pub encoding: String, // TODO: use crate encoding

  pub options: ArcRw<CompilerOpts>,
  // pub mod_options: Arc<CompilerOpts> // for compile_info()

  // pub ast... forms from the parser
  // pub errors...
  // pub warnings...
}

impl CompileModule {
  pub fn new(in_file: &Path, options: ArcRw<CompilerOpts>)  -> Self {
    Self {
      in_file: in_file.to_path_buf(),
      out_file: Default::default(), // will be set later
      module_name: "".to_string(),
      encoding: "".to_string(),
      options
    }
  }
}