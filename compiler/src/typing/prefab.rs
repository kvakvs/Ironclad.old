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

  static ref PF_TYPE_ANY_FUN: Arc<ErlType> = ErlType::AnyFn.into();

  static ref PF_TYPE_ANY_TUPLE: Arc<ErlType> = ErlType::AnyTuple.into();

  static ref PF_TYPE_ANY_BINARY: Arc<ErlType> = ErlType::AnyBinary.into();

  static ref PF_TYPE_ANYLIST: Arc<ErlType> = ErlType::AnyList.into();
  static ref PF_TYPE_NIL: Arc<ErlType> = ErlType::Nil.into();

  static ref PF_TYPE_PID: Arc<ErlType> = ErlType::Pid.into();
  static ref PF_TYPE_PORT: Arc<ErlType> = ErlType::Port.into();
  static ref PF_TYPE_REFERENCE: Arc<ErlType> = ErlType::Reference.into();
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

  /// Return a cloned instance of fun()-type
  pub fn any_fun() -> Arc<ErlType> { PF_TYPE_ANY_FUN.clone() }
  /// Return a cloned instance of tuple()-type
  pub fn any_tuple() -> Arc<ErlType> { PF_TYPE_ANY_TUPLE.clone() }
  /// Return a cloned instance of binary()-type
  pub fn any_binary() -> Arc<ErlType> { PF_TYPE_ANY_BINARY.clone() }

  /// Return a cloned instance of AnyList-type
  pub fn any_list() -> Arc<ErlType> { PF_TYPE_ANYLIST.clone() }
  /// Return a cloned instance of NIL-type
  pub fn nil() -> Arc<ErlType> { PF_TYPE_NIL.clone() }

  /// Return a cloned instance of Pid-type
  pub fn pid() -> Arc<ErlType> { PF_TYPE_PID.clone() }
  /// Return a cloned instance of Port-type
  pub fn port() -> Arc<ErlType> { PF_TYPE_PORT.clone() }
  /// Return a cloned instance of Reference-type
  pub fn reference() -> Arc<ErlType> { PF_TYPE_REFERENCE.clone() }
}
