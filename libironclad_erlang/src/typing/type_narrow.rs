//! Contains logic to narrow a wider type to a more limited type.

use crate::typing::erl_type::TypeImpl;

impl TypeImpl {
  // /// Handle situation when `self` type is used somewhere.
  // /// `Self` type containing the `used_as` type is a successful check and can return updated `self`
  // /// type, for example to update the variable scope.
  // pub(crate) fn narrow_type(&self, usages: &[ErlType]) -> IcResult<Option<ErlType>> {
  //   if usages.iter().all(|usage| usage.is_subtype_of(self)) {
  //     // compatible with all usages
  //     return Ok(None)
  //   } else {
  //     ErlError::internal("type_error: incompatible".to_string())
  //   }
  // }
}
