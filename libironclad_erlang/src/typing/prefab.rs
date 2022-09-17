//! Premade simple erltypes as lazy statics, to save on memory allocations
use crate::typing::erl_type::typekind::TypeKind;
use crate::typing::erl_type::{ErlType, TypeImpl};
use lazy_static::lazy_static;

lazy_static! {
  static ref PF_TYPE_ANY: ErlType = TypeImpl::new_unnamed(TypeKind::Any.into());
  static ref PF_TYPE_NONE: ErlType = TypeImpl::new_unnamed(TypeKind::None.into());
  static ref PF_TYPE_NUMBER: ErlType = TypeImpl::new_unnamed(TypeKind::Number.into());
  static ref PF_TYPE_INTEGER: ErlType = TypeImpl::new_unnamed(TypeKind::Integer.into());
  static ref PF_TYPE_FLOAT: ErlType = TypeImpl::new_unnamed(TypeKind::Float.into());
  static ref PF_TYPE_ATOM: ErlType = TypeImpl::new_unnamed(TypeKind::Atom.into());
  static ref PF_TYPE_BOOLEAN: ErlType = TypeImpl::new_unnamed(TypeKind::Boolean.into());
  static ref PF_TYPE_ANY_FUN: ErlType = TypeImpl::new_unnamed(TypeKind::AnyFn.into());
  static ref PF_TYPE_ANY_TUPLE: ErlType = TypeImpl::new_unnamed(TypeKind::AnyTuple.into());
  static ref PF_TYPE_ANY_BINARY: ErlType = TypeImpl::new_unnamed(TypeKind::AnyBinary.into());
  static ref PF_TYPE_ANYLIST: ErlType = TypeImpl::new_unnamed(TypeKind::AnyList.into());
  static ref PF_TYPE_NIL: ErlType = TypeImpl::new_unnamed(TypeKind::Nil.into());
  static ref PF_TYPE_PID: ErlType = TypeImpl::new_unnamed(TypeKind::Pid.into());
  static ref PF_TYPE_PORT: ErlType = TypeImpl::new_unnamed(TypeKind::Port.into());
  static ref PF_TYPE_REFERENCE: ErlType = TypeImpl::new_unnamed(TypeKind::Reference.into());
}

impl TypeImpl {
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
