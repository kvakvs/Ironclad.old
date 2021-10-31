//! Checks whether types are subtypes of other types

use std::ops::Deref;
use crate::literal::Literal;
use crate::typing::erl_type::ErlType;

/// Hosts code to check the is-subtype-of relation
pub struct SubtypeChecker {}

impl SubtypeChecker {
  /// Checks whether sub_ty is a subtype of super_ty
  pub fn is_subtype(sub_ty: &ErlType, super_ty: &ErlType) -> bool {
    match super_ty.deref() {
      // any type is a subtype of the same type
      equal_supertype if equal_supertype.eq(sub_ty) => true,
      // any includes all subtypes
      ErlType::Any => true,
      // none includes nothing
      ErlType::None => false,
      // atoms include other atoms, booleans, and singleton atom values
      ErlType::Atom => {
        match sub_ty {
          ErlType::Boolean => true,
          ErlType::Singleton { val } => {
            // Singleton atom values are a subtype of atom
            if let Literal::Atom(_) = val.deref() { true } else { false }
          }
          _other => false,
        }
      }
      // Boolean type includes all boolean, and singleton values 'true' or 'false'
      ErlType::Boolean => {
        match sub_ty {
          ErlType::Singleton { val } => {
            if let Literal::Atom(s) = val.deref() { s == "true" || s == "false" }
            else { false }
          }
          _ => false,
        }
      }
      // Numbers can include floats, integers and ranges, and singletons
      ErlType::Number => {
        match sub_ty {
          ErlType::Float | ErlType::Integer | ErlType::IntegerRange { .. } => true,
          _ => false,
        }
      }
      // Integers are subtype of both integer type and float type
      ErlType::Float | ErlType::Integer => {
        match sub_ty {
          ErlType::Integer | ErlType::IntegerRange {..} => true,
          _ => false,
        }
      }
      // ErlType::IntegerRange { .. } => {}
      // ErlType::AnyTuple => {}
      // ErlType::Tuple { .. } => {}
      // ErlType::Record { .. } => {}
      // ErlType::AnyList => {}
      // ErlType::List { .. } => {}
      // ErlType::TypedList { .. } => {}
      // ErlType::Nil => {}
      // ErlType::AnyMap => {}
      // ErlType::Map { .. } => {}
      // ErlType::AnyBinary => {}
      // ErlType::Binary { .. } => {}
      // ErlType::AnyFn => {}
      // ErlType::Fn { .. } => {}
      // ErlType::FnRef { .. } => {}
      // ErlType::Lambda => {}
      // ErlType::Pid => {}
      // ErlType::Reference => {}
      // ErlType::Port => {}
      // ErlType::Singleton { .. } => {}
      _ => unimplemented!("Subtype check for <{}> in <{}>", sub_ty, super_ty),
    }
  }
}

