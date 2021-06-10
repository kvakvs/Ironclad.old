use std::path::PathBuf;
use crate::project::compiler_opts::CompilerOpts;
use std::sync::Arc;

pub struct CompileModule {
  pub in_file: PathBuf,

  /// Input filename with extension replaced with .BEAM
  pub out_file: PathBuf,

  /// The module name from -module()
  pub module_name: String,

  pub encoding: String, // TODO: use crate encoding

  pub options: Arc<CompilerOpts>,
  // pub mod_options: Arc<CompilerOpts> // for compile_info()

  // pub ast... forms from the parser
  // pub errors...
  // pub warnings...
}