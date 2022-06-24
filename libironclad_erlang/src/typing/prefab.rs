//! Premade simple erltypes as lazy statics, to save on memory allocations
use crate::typing::erl_type::{ErlType, ErlTypeImpl};
use lazy_static::lazy_static;
use std::sync::Arc;

lazy_static! {
  static ref PF_TYPE_ANY: ErlType = ErlTypeImpl::Any.into();
  static ref PF_TYPE_NONE: ErlType = ErlTypeImpl::None.into();
  static ref PF_TYPE_NUMBER: ErlType = ErlTypeImpl::Number.into();
  static ref PF_TYPE_INTEGER: ErlType = ErlTypeImpl::Integer.into();
  static ref PF_TYPE_FLOAT: ErlType = ErlTypeImpl::Float.into();
  static ref PF_TYPE_ATOM: ErlType = ErlTypeImpl::Atom.into();
  static ref PF_TYPE_BOOLEAN: ErlType = ErlTypeImpl::Boolean.into();
  static ref PF_TYPE_ANY_FUN: ErlType = ErlTypeImpl::AnyFn.into();
  static ref PF_TYPE_ANY_TUPLE: ErlType = ErlTypeImpl::AnyTuple.into();
  static ref PF_TYPE_ANY_BINARY: ErlType = ErlTypeImpl::AnyBinary.into();
  static ref PF_TYPE_ANYLIST: ErlType = ErlTypeImpl::AnyList.into();
  static ref PF_TYPE_NIL: ErlType = ErlTypeImpl::Nil.into();
  static ref PF_TYPE_PID: ErlType = ErlTypeImpl::Pid.into();
  static ref PF_TYPE_PORT: ErlType = ErlTypeImpl::Port.into();
  static ref PF_TYPE_REFERENCE: ErlType = ErlTypeImpl::Reference.into();
}

impl ErlTypeImpl {
  /// Return a cloned instance of None-type
  pub(crate) fn none() -> ErlType {
    PF_TYPE_NONE.clone()
  }

  /// Return a cloned instance of Any-type
  pub fn any() -> ErlType {
    PF_TYPE_ANY.clone()
  }

  /// Return a cloned instance of Number-type
  pub fn number() -> ErlType {
    PF_TYPE_NUMBER.clone()
  }
  /// Return a cloned instance of Integer-type
  pub fn integer() -> ErlType {
    PF_TYPE_INTEGER.clone()
  }
  /// Return a cloned instance of Float-type
  pub fn float() -> ErlType {
    PF_TYPE_FLOAT.clone()
  }

  /// Return a cloned instance of Atom-type
  pub(crate) fn atom() -> ErlType {
    PF_TYPE_ATOM.clone()
  }
  /// Return a cloned instance of Boolean-type
  pub(crate) fn boolean() -> ErlType {
    PF_TYPE_BOOLEAN.clone()
  }

  /// Return a cloned instance of fun()-type
  #[allow(dead_code)]
  pub(crate) fn any_fun() -> ErlType {
    PF_TYPE_ANY_FUN.clone()
  }
  /// Return a cloned instance of tuple()-type
  pub fn any_tuple() -> ErlType {
    PF_TYPE_ANY_TUPLE.clone()
  }
  /// Return a cloned instance of binary()-type
  #[allow(dead_code)]
  pub(crate) fn any_binary() -> ErlType {
    PF_TYPE_ANY_BINARY.clone()
  }

  /// Return a cloned instance of AnyList-type
  pub(crate) fn any_list() -> ErlType {
    PF_TYPE_ANYLIST.clone()
  }
  /// Return a cloned instance of NIL-type
  pub fn nil() -> ErlType {
    PF_TYPE_NIL.clone()
  }

  /// Return a cloned instance of Pid-type
  pub(crate) fn pid() -> ErlType {
    PF_TYPE_PID.clone()
  }
  /// Return a cloned instance of Port-type
  pub(crate) fn port() -> ErlType {
    PF_TYPE_PORT.clone()
  }
  /// Return a cloned instance of Reference-type
  pub(crate) fn reference() -> ErlType {
    PF_TYPE_REFERENCE.clone()
  }
}
