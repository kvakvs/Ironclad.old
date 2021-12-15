//! Premade simple erltypes as lazy statics, to save on memory allocations
use std::sync::Arc;
use lazy_static::lazy_static;
use crate::typing::erl_type::ErlType;

lazy_static! {
  static ref PF_TYPE_ANY: Arc<ErlType> = ErlType::Any.into();
  static ref PF_TYPE_NONE: Arc<ErlType> = ErlType::None.into();

  static ref PF_TYPE_NUMBER: Arc<ErlType> = ErlType::Number.into();
  static ref PF_TYPE_INTEGER: Arc<ErlType> = ErlType::Integer.into();
  static ref PF_TYPE_FLOAT: Arc<ErlType> = ErlType::Float.into();

  static ref PF_TYPE_ATOM: Arc<ErlType> = ErlType::Atom.into();
  static ref PF_TYPE_BOOLEAN: Arc<ErlType> = ErlType::Boolean.into();

  static ref PF_TYPE_ANYLIST: Arc<ErlType> = ErlType::AnyList.into();
  static ref PF_TYPE_NIL: Arc<ErlType> = ErlType::Nil.into();
}

impl ErlType {
  /// Return a cloned instance of None-type
  pub fn none() -> Arc<ErlType> { PF_TYPE_NONE.clone() }

  /// Return a cloned instance of Any-type
  pub fn any() -> Arc<ErlType> { PF_TYPE_ANY.clone() }

  /// Return a cloned instance of Number-type
  pub fn number() -> Arc<ErlType> { PF_TYPE_NUMBER.clone() }
  /// Return a cloned instance of Integer-type
  pub fn integer() -> Arc<ErlType> { PF_TYPE_INTEGER.clone() }
  /// Return a cloned instance of Float-type
  pub fn float() -> Arc<ErlType> { PF_TYPE_FLOAT.clone() }

  /// Return a cloned instance of Atom-type
  pub fn atom() -> Arc<ErlType> { PF_TYPE_ATOM.clone() }
  /// Return a cloned instance of Boolean-type
  pub fn boolean() -> Arc<ErlType> { PF_TYPE_BOOLEAN.clone() }

  /// Return a cloned instance of AnyList-type
  pub fn any_list() -> Arc<ErlType> { PF_TYPE_ANYLIST.clone() }
  /// Return a cloned instance of NIL-type
  pub fn nil() -> Arc<ErlType> { PF_TYPE_NIL.clone() }
}
