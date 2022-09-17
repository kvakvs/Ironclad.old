//! Substitution for type variables from list of type variables, following the `when` clause in a typespec
use crate::error::ic_error::IroncladResult;
use crate::typing::erl_type::{ErlType, TypeImpl};
use crate::typing::type_error::TypeError;
use std::ops::Deref;

impl TypeImpl {
  fn find_typevar(tv: &str, when_clause: &[ErlType]) -> Option<ErlType> {
    when_clause
      .iter()
      .find(|each| each.has_typevar_name(tv))
      .cloned()
  }

  /// For `ErlType` which is a `TypeVar`, substitute it with a type from `when` clause.
  /// Other types are returned unchanged. Moves its `t` argument to the output.
  pub fn substitute_var(t: ErlType, when_clause: &[ErlType]) -> IroncladResult<ErlType> {
    if let Some(tv_name) = &t.typevar {
      // Try find a typevar in `when` clause
      match Self::find_typevar(tv_name.as_str(), when_clause) {
        Some(found) => {
          // Check that t's own kind matches the result
          if !t.is_any() && !t.is_subtype_of(&found) {
            return Err(TypeError::new_spec_error(
              None,
              format!(
                "Type variable {} has type {} incompatible with `when` definition: {}",
                tv_name,
                t.deref(),
                found.deref()
              ),
            ));
          }
          // Rebuild a new type with old name and new knowledge about its type from the when clause
          let result = TypeImpl::new_named(tv_name.clone(), found.kind.clone());
          Ok(result)
        }
        None => Ok(t),
      }
    } else {
      Ok(t)
    }
  }
}
