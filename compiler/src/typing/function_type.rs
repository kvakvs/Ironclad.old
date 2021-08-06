use crate::typing::erl_type::ErlType;

/// ErlType variant for a function or a lambda
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct FunctionType {
  /// Name if known, for module level functions, or unnamed for anonymous funs
  pub name: Option<String>,
  /// Types of input args
  pub arg_types: Vec<ErlType>,
  /// Return type
  pub ret_type: Box<ErlType>,
}
