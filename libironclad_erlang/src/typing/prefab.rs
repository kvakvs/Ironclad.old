//! Premade simple erltypes as lazy statics, to save on memory allocations
use crate::typing::erl_type::typekind::TypeKind;
use crate::typing::erl_type::{ErlType, TypeImpl};
use lazy_static::lazy_static;

lazy_static! {
  static ref PF_TYPE_ANY: ErlType = TypeImpl::new_unnamed(TypeKind::Any);
  static ref PF_TYPE_NONE: ErlType = TypeImpl::new_unnamed(TypeKind::None);
  static ref PF_TYPE_NUMBER: ErlType = TypeImpl::new_unnamed(TypeKind::Number);
  static ref PF_TYPE_INTEGER: ErlType = TypeImpl::new_unnamed(TypeKind::Integer);
  static ref PF_TYPE_FLOAT: ErlType = TypeImpl::new_unnamed(TypeKind::Float);
  static ref PF_TYPE_ATOM: ErlType = TypeImpl::new_unnamed(TypeKind::Atom);
  static ref PF_TYPE_ATOM_TRUE: ErlType = TypeImpl::new_unnamed(TypeKind::new_atom("true"));
  static ref PF_TYPE_ATOM_FALSE: ErlType = TypeImpl::new_unnamed(TypeKind::new_atom("false"));
  static ref PF_TYPE_BOOLEAN: ErlType = TypeImpl::new_unnamed(TypeKind::Boolean);
  static ref PF_TYPE_ANY_FUN: ErlType = TypeImpl::new_unnamed(TypeKind::AnyFn);
  static ref PF_TYPE_ANY_TUPLE: ErlType = TypeImpl::new_unnamed(TypeKind::AnyTuple);
  static ref PF_TYPE_ANY_BINARY: ErlType = TypeImpl::new_unnamed(TypeKind::AnyBinary);
  static ref PF_TYPE_ANY_LIST: ErlType = TypeImpl::new_unnamed(TypeKind::AnyList);
  static ref PF_TYPE_NIL: ErlType = TypeImpl::new_unnamed(TypeKind::Nil);
  static ref PF_TYPE_PID: ErlType = TypeImpl::new_unnamed(TypeKind::Pid);
  static ref PF_TYPE_PORT: ErlType = TypeImpl::new_unnamed(TypeKind::Port);
  static ref PF_TYPE_REFERENCE: ErlType = TypeImpl::new_unnamed(TypeKind::Reference);
}

impl TypeImpl {
  /// Return a cloned instance of None-type
  pub fn none() -> ErlType {
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
  pub fn atom() -> ErlType {
    PF_TYPE_ATOM.clone()
  }

  /// Return a cloned instance of Atom 'true'
  pub fn atom_true() -> ErlType {
    PF_TYPE_ATOM_TRUE.clone()
  }

  /// Return a cloned instance of Atom 'false'
  pub fn atom_false() -> ErlType {
    PF_TYPE_ATOM_FALSE.clone()
  }

  /// Return a cloned instance of Boolean-type
  pub fn boolean() -> ErlType {
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
  pub fn any_list() -> ErlType {
    PF_TYPE_ANY_LIST.clone()
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
