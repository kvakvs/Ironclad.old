//! Substitution for type variables from list of type variables, following the `when` clause in a typespec
use crate::typing::erl_type::{ErlType, TypeImpl};

impl TypeImpl {
  /// For `ErlType` which is a `TypeVar`, substitute it with a type from `when` clause.
  /// Other types are returned unchanged. Moves its `t` argument to the output.
  pub fn substitute_var_move(t: ErlType, when_clause: &[ErlType]) -> ErlType {
    if let Some(tv) = &t.typevar {
      todo!("substitute typevars in t")
    } else {
      t
    }
  }
}
