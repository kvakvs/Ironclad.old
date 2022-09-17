//! Defines an Erlang-type
use crate::literal::Literal;
use crate::typing::erl_integer::ErlInteger;
use crate::typing::erl_type::binary_type::{BinaryTypeHeadElement, BinaryTypeTailElement};
use crate::typing::erl_type::map_type::MapMemberType;
use crate::typing::fn_type::FnType;
use crate::typing::record_field_type::RecordFieldType;
use crate::typing::type_union::TypeUnion;
use libironclad_util::mfarity::MFArity;
use std::sync::Arc;
use typekind::TypeKind;

pub mod binary_type;
pub mod map_type;
pub mod type_as;
pub mod type_is;
pub mod type_new;
pub mod type_print;
pub mod type_var_subst;
pub mod typekind;
pub mod typekind_new;

/// Internal type representation, contains an optional name for when this type had a variable name
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TypeImpl {
  /// Optional type-variable name
  pub typevar: Option<String>,
  /// The type
  pub kind: TypeKind,
}

/// Wraps `ErlType` with `Arc<>`
pub type ErlType = Arc<TypeImpl>;

//
// Type classification
//

impl TypeImpl {
  /// Return a number placing the type somewhere in the type ordering hierarchy
  /// number < atom < reference < fun < port < pid < tuple < map < nil < list < bit string
  /// This ordering is used for BTree construction, not for comparisons
  pub(crate) fn get_order(&self) -> usize {
    match &self.kind {
      TypeKind::None => 0,
      // ErlType::Union(_) => 1,
      // ErlType::TVar(_) => 2,
      TypeKind::Any => 1000,

      TypeKind::Number => 10,
      TypeKind::Float => 11,
      TypeKind::Integer => 12,
      TypeKind::IntegerRange { .. } => 13,

      TypeKind::Boolean => 20,
      TypeKind::Atom => 21,

      TypeKind::Reference => 30,

      TypeKind::AnyFn => 40,
      TypeKind::Fn { .. } => 41,
      TypeKind::Lambda { .. } => 42,

      TypeKind::Port => 50,

      TypeKind::Pid => 60,

      TypeKind::AnyTuple => 70,
      TypeKind::Tuple { .. } => 71,
      TypeKind::Record { .. } => 72,

      TypeKind::Map { .. } => 80,

      TypeKind::Nil => 90,

      TypeKind::AnyList => 100,
      TypeKind::List { .. } => 101,
      TypeKind::StronglyTypedList { .. } => 102,

      TypeKind::AnyBinary => 110,
      TypeKind::Binary { .. } => 111,

      TypeKind::Singleton { val } => val.synthesize_type().get_order(),

      other => unimplemented!("Don't know how to get numeric order for Erlang-type {}", &self),
    }
  }
}
