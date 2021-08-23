//! Contains pre-fabricated types ready to use
use lazy_static::lazy_static;
use crate::typing::erl_type::ErlType;
use std::sync::Arc;

/// Contains constructors for prefabricated Erlang types, saving on multiple memory allocations
pub struct TypePrefab {}

lazy_static! {
    /// Prefabricated type
    pub static ref NONE: Arc<ErlType> = ErlType::None.into();
    /// Prefabricated type
    pub static ref ANY: Arc<ErlType> = ErlType::Any.into();
    /// Prefabricated type
    pub static ref ANY_INTEGER: Arc<ErlType> = ErlType::AnyInteger.into();
    /// Prefabricated type
    pub static ref FLOAT: Arc<ErlType> = ErlType::Float.into();
    /// Prefabricated type
    pub static ref NUMBER: Arc<ErlType> = ErlType::Number.into();
    /// Prefabricated type
    pub static ref ANY_BOOL: Arc<ErlType> = ErlType::AnyBool.into();
    /// Prefabricated type
    pub static ref ANY_LIST: Arc<ErlType> = ErlType::AnyList.into();
}

impl TypePrefab {
  /// Returns a prefabricated none-type
  pub fn none() -> Arc<ErlType> { NONE.clone() }
  /// Returns a prefabricated any-type
  pub fn any() -> Arc<ErlType> { ANY.clone() }
  /// Returns a prefabricated any-integer-type
  pub fn any_integer() -> Arc<ErlType> { ANY_INTEGER.clone() }
  /// Returns a prefabricated float-type
  pub fn float() -> Arc<ErlType> { FLOAT.clone() }
  /// Returns a prefabricated number-type
  pub fn number() -> Arc<ErlType> { NUMBER.clone() }
  /// Returns a prefabricated any-bool-type
  pub fn any_bool() -> Arc<ErlType> { ANY_BOOL.clone() }
  /// Returns a prefabricated any-list type
  pub fn any_list() -> Arc<ErlType> { ANY_LIST.clone() }
}
