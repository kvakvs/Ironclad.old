//! Defines an Erlang-type
use crate::literal::Literal;
use crate::typing::erl_integer::ErlInteger;
use crate::typing::erl_type::binary_type::{BinaryTypeHeadElement, BinaryTypeTailElement};
use crate::typing::erl_type::map_type::MapMemberType;
use crate::typing::fn_type::FnType;
use crate::typing::record_field_type::RecordFieldType;
use crate::typing::type_union::TypeUnion;
use crate::typing::typevar::Typevar;
use libironclad_util::mfarity::MFArity;
use std::sync::Arc;

pub mod binary_type;
pub mod map_type;
pub mod type_as;
pub mod type_is;
pub mod type_new;
pub mod type_print;

/// Describes an Erlang type, usually stored as ErlType
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ErlTypeImpl {
  /// Any type
  Any,
  /// Empty set of types, or a void return, or a crash
  None,

  /// Any atom
  Atom,
  /// Atom 'true' or atom 'false'
  Boolean,

  /// Float or integer, any of those
  Number,
  /// IEEE 64-bit float
  Float,
  /// Any integer or big integer
  Integer,
  /// Defines integer range A..B, cannot be single element or empty
  IntegerRange {
    /// First value of the range
    from: ErlInteger,
    /// Last value of the range
    to: ErlInteger,
  },

  /// Any tuple of any size
  AnyTuple,
  /// Tuple of multiple types, elements count is the size
  Tuple {
    /// Collection of types for tuple elements, same size as tuple arity
    elements: Vec<ErlType>,
  },
  /// A tuple with an atom tag and typed fields
  Record {
    /// Literal atom for the record tag
    tag: String,
    /// Record fields - name :: type()
    fields: Vec<Arc<RecordFieldType>>,
  },

  /// List of any()
  AnyList,
  /// List of elements belonging to an union type
  List {
    /// Union type for all elements
    elements: ErlType,
    /// Tail element if not NIL, otherwise None
    tail: Option<ErlType>,
    /// Requirement that the list is non-empty
    is_non_empty: bool,
  },
  /// Tuple-style strongly typed list of fixed size, with each element having own type
  StronglyTypedList {
    /// Type for each list element also
    elements: Vec<ErlType>,
    /// Tail element if not NIL
    tail: Option<ErlType>,
  },
  /// Empty list []
  Nil,

  /// Map with any keys
  AnyMap,
  /// Type for a dictionary of key=>value style
  Map {
    /// Defines map key/value type pairs
    members: Vec<MapMemberType>,
  },

  /// Any binary of any size
  AnyBinary,
  /// Binary with head element, and repeated tail element, both optional
  Binary {
    /// Start of the binary
    head: Option<BinaryTypeHeadElement>,
    /// Rest of the binary, repeated
    tail: Option<BinaryTypeTailElement>,
  },

  /// Matches function references and lambdas
  AnyFn,
  /// Describes a function type with multiple clauses and return types
  Fn(Arc<FnType>),
  /// fun name/2 style references, also remote references
  FnRef {
    /// Function's location (module/function or just function)
    fun: MFArity,
  },
  /// A function value, created using `fun(Args) -> code.` expression
  Lambda,

  /// A local or remote process id
  Pid,
  /// A local or remote reference value
  Reference,
  /// An open file, socket or some other port resource
  Port,

  /// A single literal value of any type
  Singleton {
    /// Singleton's value, a literal
    val: Arc<Literal>,
  },

  /// Contains multiple types + operations on these types
  Union(TypeUnion),

  /// A user-defined type referred by a string, with 0 or more type parameters.
  UserDefinedType {
    /// User-defined module and name, arity must match args length
    name: MFArity,
    /// Type variable arguments for `typename(arg1, arg2, ...)`
    args: Vec<Typevar>,
  },

  /// A type variable, possibly with a name, and possibly with a pinned type
  Typevar(Typevar),

  /// Refers to a record by its tag name
  RecordRef {
    /// Record tag
    tag: String,
    /// Pin field type to a subtype of parent field
    pins: Vec<RecordFieldType>,
  },
}

/// Wraps `ErlType` with `Arc<>`
pub type ErlType = Arc<ErlTypeImpl>;

//
// Type classification
//

impl ErlTypeImpl {
  /// Return a number placing the type somewhere in the type ordering hierarchy
  /// number < atom < reference < fun < port < pid < tuple < map < nil < list < bit string
  /// This ordering is used for BTree construction, not for comparisons
  pub(crate) fn get_order(&self) -> usize {
    match self {
      ErlTypeImpl::None => 0,
      // ErlType::Union(_) => 1,
      // ErlType::TVar(_) => 2,
      ErlTypeImpl::Any => 1000,

      ErlTypeImpl::Number => 10,
      ErlTypeImpl::Float => 11,
      ErlTypeImpl::Integer => 12,
      ErlTypeImpl::IntegerRange { .. } => 13,

      ErlTypeImpl::Boolean => 20,
      ErlTypeImpl::Atom => 21,

      ErlTypeImpl::Reference => 30,

      ErlTypeImpl::AnyFn => 40,
      ErlTypeImpl::Fn { .. } => 41,
      ErlTypeImpl::Lambda { .. } => 42,

      ErlTypeImpl::Port => 50,

      ErlTypeImpl::Pid => 60,

      ErlTypeImpl::AnyTuple => 70,
      ErlTypeImpl::Tuple { .. } => 71,
      ErlTypeImpl::Record { .. } => 72,

      ErlTypeImpl::Map { .. } => 80,

      ErlTypeImpl::Nil => 90,

      ErlTypeImpl::AnyList => 100,
      ErlTypeImpl::List { .. } => 101,
      ErlTypeImpl::StronglyTypedList { .. } => 102,

      ErlTypeImpl::AnyBinary => 110,
      ErlTypeImpl::Binary { .. } => 111,

      ErlTypeImpl::Singleton { val } => val.synthesize_type().get_order(),

      other => unimplemented!("Don't know how to get numeric order for Erlang-type {}", other),
    }
  }
}
