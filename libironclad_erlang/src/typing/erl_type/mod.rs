//! Defines an Erlang-type
use crate::literal::Literal;
use crate::typing::erl_type::map_type::MapMemberType;
use crate::typing::fn_type::FnType;
use crate::typing::record_field_type::RecordFieldType;
use crate::typing::type_union::TypeUnion;
use crate::typing::typevar::Typevar;
use libironclad_util::mfarity::MFArity;
use std::sync::Arc;

pub mod map_type;
pub mod type_as;
pub mod type_is;
pub mod type_new;
pub mod type_print;

/// Describes an Erlang type, usually stored as Arc<ErlType>
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ErlType {
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
    from: Literal,
    /// Last value of the range
    to: Literal,
  },

  /// Any tuple of any size
  AnyTuple,
  /// Tuple of multiple types, elements count is the size
  Tuple {
    /// Collection of types for tuple elements, same size as tuple arity
    elements: Vec<Arc<ErlType>>,
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
    elements: Arc<ErlType>,
    /// Tail element if not NIL, otherwise None
    tail: Option<Arc<ErlType>>,
  },
  /// Tuple-style strongly typed list of fixed size, with each element having own type
  StronglyTypedList {
    /// Type for each list element also
    elements: Vec<Arc<ErlType>>,
    /// Tail element if not NIL
    tail: Option<Arc<ErlType>>,
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

  /// Any ironclad_exe of any size
  AnyBinary,
  /// Binary of size and possibly with last byte incomplete
  Binary {
    /// Byte size
    size: usize,
    /// If non-zero, this represents a bit string
    last_byte_bits: usize,
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
}

//
// Type classification
//

impl ErlType {
  /// Return a number placing the type somewhere in the type ordering hierarchy
  /// number < atom < reference < fun < port < pid < tuple < map < nil < list < bit string
  /// This ordering is used for BTree construction, not for comparisons
  pub fn get_order(&self) -> usize {
    match self {
      ErlType::None => 0,
      // ErlType::Union(_) => 1,
      // ErlType::TVar(_) => 2,
      ErlType::Any => 1000,

      ErlType::Number => 10,
      ErlType::Float => 11,
      ErlType::Integer => 12,
      ErlType::IntegerRange { .. } => 13,

      ErlType::Boolean => 20,
      ErlType::Atom => 21,

      ErlType::Reference => 30,

      ErlType::AnyFn => 40,
      ErlType::Fn { .. } => 41,
      ErlType::Lambda { .. } => 42,

      ErlType::Port => 50,

      ErlType::Pid => 60,

      ErlType::AnyTuple => 70,
      ErlType::Tuple { .. } => 71,
      ErlType::Record { .. } => 72,

      ErlType::Map { .. } => 80,

      ErlType::Nil => 90,

      ErlType::AnyList => 100,
      ErlType::List { .. } => 101,
      ErlType::StronglyTypedList { .. } => 102,

      ErlType::AnyBinary => 110,
      ErlType::Binary { .. } => 111,

      ErlType::Singleton { val } => val.synthesize_type().get_order(),

      other => unimplemented!("Don't know how to get numeric order for Erlang-type {}", other),
    }
  }
}
