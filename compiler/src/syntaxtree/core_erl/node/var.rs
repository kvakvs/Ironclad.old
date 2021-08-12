//! A variable with possibly missing name and unique typevar
use crate::typing::typevar::TypeVar;

/// Represents a variable with an optional name (`None` for generated variables), or a string name,
/// and a new unique type variable.
pub struct Var {
  /// Optional name, `None` means the name is numbered from `Self::ty`
  pub name: Option<String>,
  /// Unique type variable
  pub ty: TypeVar,
}
