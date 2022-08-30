//! A type variable: name and a type pair.

use crate::typing::erl_type::{ErlType, ErlTypeImpl};
use ::function_name::named;
use libironclad_util::pretty::Pretty;
use std::fmt::{Display, Formatter};

/// Represents a function argument, a type variable in a typespec or a member of `when` clause
/// in a function spec. Name is optional.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Typevar {
  /// Name of the typevar, or None if unnamed
  pub name: Option<String>,
  /// Type of the typevar, or any()
  pub ty: ErlType,
}

/// Wrapper for typevar array to implement things like Display
pub struct Typevars<'a>(&'a [Typevar]);

impl Typevar {
  /// Construct a new typevar, with possibly a type, otherwise any() will be used.
  pub(crate) fn new(name: Option<String>, maybe_type: Option<ErlType>) -> Self {
    Self {
      name,
      ty: maybe_type.unwrap_or_else(ErlTypeImpl::any),
    }
  }

  /// Creates an unnamed typevar with a given type
  pub(crate) fn from_erltype(t: &ErlType) -> Self {
    Self { name: None, ty: t.clone() }
  }

  /// Create an unnamed anytype.
  #[allow(dead_code)]
  pub(crate) fn new_unnamed_any() -> Self {
    Self { name: None, ty: ErlTypeImpl::any() }
  }

  /// Given a var, and a list of typevars in when clause, try find one matching
  pub(crate) fn substitute_var_from_when_clause<'a>(
    item_a: &'a Typevar,
    b: &'a [Typevar],
  ) -> &'a Typevar {
    // if element in A has a name, and...
    if item_a.name.is_some() {
      if let Some(found_b) = b.iter().find(|item_b| {
        // if some element in B has a name which matches A
        // and element B's type is same or more narrow than type of element A
        item_b.name.is_some() && item_b.name == item_a.name && item_b.ty.is_subtype_of(&item_a.ty)
      }) {
        // Then replace element A with element from B
        found_b
      } else {
        item_a
      }
    } else {
      item_a
    }
  }

  /// Merges lists a and b, by finding typevar names from a in b
  #[named]
  pub(crate) fn merge_lists(a: &[Typevar], b: &[Typevar]) -> Vec<Typevar> {
    println!("{}: Merging {} <> {}", function_name!(), Typevars(a), Typevars(b));
    let result: Vec<Typevar> = a
      .iter()
      .map(|each_a| Self::substitute_var_from_when_clause(each_a, b))
      .cloned()
      .collect();
    println!("{}: Merge result {}", function_name!(), Typevars(&result));
    result
  }

  /// Consumes argument.
  /// Converts vector of typevar into vector of erltype::typevars
  pub(crate) fn vec_of_typevars_into_types(typevars: Vec<Typevar>) -> Vec<ErlType> {
    typevars.into_iter().map(ErlTypeImpl::new_typevar).collect()
  }
}

impl Display for Typevar {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match &self.name {
      Some(n) => write!(f, "{} :: ", n)?,
      None => {}
    }
    self.ty.fmt(f)
  }
}

impl<'a> Display for Typevars<'a> {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    Pretty::display_comma_separated(self.0.iter(), f)
  }
}
