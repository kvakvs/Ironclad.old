//! Trait for grouping together postprocessing functions in AST content structs
use crate::erl_module::ErlModule;
use crate::erl_error::ErlResult;

/// Implement this on AST node content structs
pub trait TPostProcess {
  /// Possibly replace some fields in self and return new AST, or return original AST. Env arg
  /// is providing the environment access (to find functions, other modules, etc).
  fn postprocess_ast(&mut self, env: &mut ErlModule) -> ErlResult<()>;
}
