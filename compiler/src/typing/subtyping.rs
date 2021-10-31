//! Checks whether types are subtypes of other types

use std::ops::Deref;
use std::sync::Arc;
use crate::literal::Literal;
use crate::typing::erl_type::ErlType;

/// Hosts code to check the is-subtype-of relation
pub struct SubtypeChecker {}

impl SubtypeChecker {
  /// Checks whether sub_ty is a subtype of super_ty
  pub fn is_subtype(sub_ty: &ErlType, super_ty: &ErlType) -> bool {
    match super_ty.deref() {
      equal_supertype if equal_supertype.eq(sub_ty) => true, // equal types are mutual subtypes
      ErlType::Any => true, // any includes all subtypes
      ErlType::None => false, // none includes nothing
      ErlType::Atom => Self::is_subtype_of_atom(sub_ty),
      ErlType::Boolean => Self::is_subtype_of_boolean(sub_ty),
      ErlType::Number => Self::is_subtype_of_number(sub_ty),
      ErlType::Float => Self::is_subtype_of_float(sub_ty),
      ErlType::Integer => Self::is_subtype_of_integer(sub_ty),
      // ErlType::IntegerRange { from, to } => { todo: only singletons and nested ranges and must be from <= n <= to }

      // ErlType::AnyTuple => {}
      // ErlType::Tuple { .. } => {}
      // ErlType::Record { .. } => {}

      ErlType::AnyList => Self::is_subtype_of_anylist(sub_ty),
      ErlType::List { elements: supertype_elements, tail: supertype_tail } =>
        Self::is_subtype_of_list(supertype_elements, supertype_tail, sub_ty),
      ErlType::StronglyTypedList { elements: supertype_elements, tail: supertype_tail } =>
        Self::is_subtype_of_strongly_typed_list(supertype_elements, supertype_tail, sub_ty),
      ErlType::Nil => false, // can only include other nil

      ErlType::AnyMap => {
        match sub_ty {
          ErlType::Map { .. } => true,
          _ => false,
        }
      }
      // ErlType::Map { .. } => {}

      ErlType::AnyBinary => {
        if let ErlType::Binary { .. } = sub_ty { true } else { false }
      }
      // An equal binary type can be a subtype of binary, no other matches it (checked at the top)
      ErlType::Binary { .. } => false,

      ErlType::AnyFn => {
        match sub_ty {
          ErlType::Fn { .. } | ErlType::FnRef { .. } | ErlType::Lambda => true,
          _ => false,
        }
      }
      // only can be subtype of self (equality checked at the top)
      ErlType::Fn { .. } | ErlType::FnRef { .. } | ErlType::Lambda => false,

      // only can be subtype of self (equality checked at the top)
      ErlType::Pid | ErlType::Reference | ErlType::Port | ErlType::Singleton { .. } => false,

      _ => unimplemented!("Subtype check for <{}> in <{}>", sub_ty, super_ty),
    }
  }

  /// Checks whether sub_ty matches an atom() type.
  /// Atoms include other atoms, booleans, and singleton atom values
  fn is_subtype_of_atom(sub_ty: &ErlType) -> bool {
    match sub_ty {
      ErlType::Boolean => true,
      ErlType::Singleton { val } => {
        // Singleton atom values are a subtype of atom
        if let Literal::Atom(_) = val.deref() { true } else { false }
      }
      _other => false,
    }
  }

  /// Checks whether sub_ty matches a boolean() type.
  /// Boolean type includes all boolean, and singleton values 'true' or 'false'
  fn is_subtype_of_boolean(sub_ty: &ErlType) -> bool {
    match sub_ty {
      ErlType::Singleton { val } => {
        if let Literal::Atom(s) = val.deref() { s == "true" || s == "false" } else { false }
      }
      _ => false,
    }
  }

  /// Checks whether sub_ty matches a number() type.
  /// Numbers can include floats, integers and ranges, and singletons of integer
  fn is_subtype_of_number(sub_ty: &ErlType) -> bool {
    match sub_ty {
      ErlType::Float
      | ErlType::Integer
      | ErlType::IntegerRange { .. } => true,
      ErlType::Singleton { val } => {
        // Floats and integer singletons are subtype of number
        match val.deref() {
          Literal::Float(_) => true,
          Literal::Integer(_) => true,
          _ => false,
        }
      }
      _ => false,
    }
  }

  /// Checks whether sub_ty matches a float() type.
  /// Floats include integers, other floats, and singletons of integer and float
  fn is_subtype_of_float(sub_ty: &ErlType) -> bool {
    match sub_ty {
      ErlType::Integer
      | ErlType::IntegerRange { .. } => true,
      ErlType::Singleton { val } => {
        // Floats and integer singletons are subtype of float
        match val.deref() {
          Literal::Float(_) => true,
          Literal::Integer(_) => true,
          _ => false,
        }
      }
      _ => false,
    }
  }

  /// Checks whether sub_ty matches an integer() type.
  /// integer() includes integer(), ranges, and singletons of integer
  fn is_subtype_of_integer(sub_ty: &ErlType) -> bool {
    match sub_ty {
      ErlType::Integer
      | ErlType::IntegerRange { .. } => true,
      ErlType::Singleton { val } => {
        // Integer singletons are subtype of integer
        match val.deref() {
          Literal::Integer(_) => true,
          _ => false,
        }
      }
      _ => false,
    }
  }

  /// Checks whether sub_ty matches a list() type.
  /// A list() includes any other lists() and nil []
  fn is_subtype_of_anylist(sub_ty: &ErlType) -> bool {
    match sub_ty {
      ErlType::List { .. } | ErlType::StronglyTypedList { .. } | ErlType::Nil => true,
      _ => false,
    }
  }

  /// Checks whether sub_ty matches a list `[supertype_elements() | supertype_tail()]` type.
  fn is_subtype_of_list(supertype_elements: &ErlType, supertype_tail: &ErlType,
                        sub_ty: &ErlType) -> bool {
    match sub_ty {
      // For superlist to include a sublist
      ErlType::List { elements: subtype_elements, tail: subtype_tail } => {
        subtype_tail.is_subtype_of(supertype_tail)
            && subtype_elements.is_subtype_of(supertype_elements)
      }
      // For superlist to include typed sublist
      ErlType::StronglyTypedList { elements: subtype_elements, tail: subtype_tail } => {
        subtype_tail.is_subtype_of(supertype_tail)
            && subtype_elements.iter().all(|subt| subt.is_subtype_of(supertype_elements))
      }
      ErlType::Nil => true,
      _ => false,
    }
  }

  /// Checks whether sub_ty matches a list `[supertype_elements() | supertype_tail()]` type.
  fn is_subtype_of_strongly_typed_list(supertype_elements: &Vec<Arc<ErlType>>, supertype_tail: &ErlType,
                                       sub_ty: &ErlType) -> bool {
    match sub_ty {
      // For typed list to include a list
      ErlType::List { elements: subtype_elements, tail: subtype_tail } => {
        // Sublist type must be subtype of each superlist element
        subtype_tail.is_subtype_of(supertype_tail)
            && supertype_elements.iter()
            .all(|supt| subtype_elements.is_subtype_of(supt))
      }
      // For typed superlist to include another typed sublist
      ErlType::StronglyTypedList { elements: subtype_elements, tail: subtype_tail } => {
        // Length must match
        // AND Each element of sublist must be subtype of corresponding element of superlist
        subtype_elements.len() == supertype_elements.len()
            && subtype_tail.is_subtype_of(supertype_tail)
            && subtype_elements.iter()
            .zip(supertype_elements.iter())
            .all(|(subt, supert)| subt.is_subtype_of(supert))
      }
      ErlType::Nil => true,
      _ => false,
    }
  }
}

