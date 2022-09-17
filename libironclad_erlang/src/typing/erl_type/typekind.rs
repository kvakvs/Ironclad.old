//! Inner `ErlType` content

use crate::literal::Literal;
use crate::typing::erl_integer::ErlInteger;
use crate::typing::erl_type::binary_type::{BinaryTypeHeadElement, BinaryTypeTailElement};
use crate::typing::erl_type::map_type::MapMemberType;
use crate::typing::erl_type::ErlType;
use crate::typing::fn_type::FnType;
use crate::typing::record_field_type::RecordFieldType;
use crate::typing::type_union::TypeUnion;
use libironclad_util::mfarity::MFArity;
use std::sync::Arc;

/// Describes an Erlang type, usually stored as ErlType
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TypeKind {
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
    args: Vec<ErlType>,
  },

  /// Refers to a record by its tag name
  RecordRef {
    /// Record tag
    tag: String,
    /// Pin field type to a subtype of parent field
    pins: Vec<RecordFieldType>,
  },
}
