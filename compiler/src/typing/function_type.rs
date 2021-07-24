use crate::typing::erl_type::ErlType;

/// ErlType variant for a function or a lambda
#[derive(Clone, PartialEq)]
pub struct FunctionType {
  /// Name if known, for module level functions, or unnamed for anonymous funs
  pub name: Option<String>,
  /// Types of input args
  pub arg_ty: Vec<ErlType>,
  /// Return type
  pub ret: Box<ErlType>,
}
