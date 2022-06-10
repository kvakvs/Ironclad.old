//! Module compile state contains all inputs necessary to compile a module. Environment is passed
//! in separately (as the module will need to know about other modules and their inferred types)

use crate::project::compiler_opts::CompilerOpts;
use libironclad_erlang::erl_syntax::erl_ast::AstNode;
use libironclad_erlang::source_file::SourceFile;
use std::path::PathBuf;
use std::sync::Arc;

/// Compile state, used as input to begin the compilation
pub struct CompileModule {
  /// Path for the file
  pub in_file: PathBuf,

  /// Input filename with extension replaced with .BEAM
  pub out_file: PathBuf,

  /// The module name from -module()
  pub module_name: String,

  /// Module source encoding
  pub encoding: String, // TODO: use crate encoding

  /// Compiler options used by this module
  pub options: Arc<CompilerOpts>,
  // pub mod_options: Arc<CompilerOpts> // for compile_info()

  // pub ast... forms from the parser
  // pub errors...
  // pub warnings...
}

impl CompileModule {
  /// Creates new state for module compilation
  pub(crate) fn new(in_file: &SourceFile, options: Arc<CompilerOpts>) -> Self {
    Self {
      in_file: in_file.file_name.to_path_buf(),
      out_file: Default::default(), // will be set later
      module_name: "".to_string(),
      encoding: "".to_string(),
      options,
    }
  }

  /// Given a parsed AST tree start work on the compilation
  // TODO: Also will need the environment with access to other modules and their inferred types
  pub(crate) fn compile(&mut self, _ast: AstNode) {
    unimplemented!("compile module")
  }
}
