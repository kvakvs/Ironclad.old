//! Defines an Erlang-type
use std::ops::Deref;
use std::sync::Arc;
use crate::literal::Literal;
use crate::mfarity::MFArity;
use crate::typing::fn_clause_type::FnClauseType;
use crate::typing::record_field_type::RecordFieldType;
use crate::typing::subtyping::SubtypeChecker;
use crate::typing::type_union::TypeUnion;

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
    elements: Vec<Arc<ErlType>>
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
    items: Vec<(Arc<ErlType>, Arc<ErlType>)>
  },

  /// Any binary of any size
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
  Fn {
    /// For convenience arity is stored here, but each clause has same arity too
    arity: usize,
    /// Function clauses
    clauses: Vec<FnClauseType>,
  },
  /// fun name/2 style references, also remote references
  FnRef {
    /// Function's location (module/function or just function)
    fun: MFArity
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
    val: Arc<Literal>
  },

  /// Contains multiple types + operations on these types
  Union(TypeUnion),
}

//
// Constructors and Generators
//
impl ErlType {
  /// Clones literal's refcounted pointer and returns a singleton type
  pub fn new_singleton(lit: &Arc<Literal>) -> Arc<ErlType> {
    ErlType::Singleton { val: lit.clone() }.into()
  }

  /// Creates a new singleton atom of name `s`
  pub fn new_atom(s: &str) -> ErlType {
    ErlType::Singleton {
      val: Literal::Atom(s.to_string()).into()
    }
  }

  /// Creates a type for a proper list with a NIL tail.
  pub fn list_of(t: ErlType) -> ErlType {
    ErlType::List {
      elements: t.into(),
      tail: None,
    }
  }

  /// Creates new function type with clauses
  pub fn new_fn_type(clauses: Vec<FnClauseType>) -> ErlType {
    assert!(!clauses.is_empty(), "Attempt to build a fn type with zero clauses");

    let arity = clauses[0].arity();
    assert!(clauses.iter().all(|c| c.arity() == arity),
            "Attempt to build a fn type with clauses of different arity (first clause had arity {})",
            arity);

    ErlType::Fn {
      arity,
      clauses,
    }
  }

  /// Wrapper to access type union construction
  pub fn new_union(types: Vec<Arc<ErlType>>) -> ErlType {
    match types.len() {
      0 => ErlType::None,
      1 => (*types[0].as_ref()).clone(),
      _ => {
        let mut u = TypeUnion::new(types);
        u.normalize();
        ErlType::Union(u)
      }
    }
  }
}

//
// Type classification
//
impl ErlType {
  /// Shortcut to the subtype checker
  pub fn is_subtype_of(&self, other: &ErlType) -> bool {
    SubtypeChecker::is_subtype(self, other)
  }

  /// Checks whether type is an atom
  pub fn is_atom(&self) -> bool {
    return match self {
      ErlType::Atom
      | ErlType::Boolean => true,
      ErlType::Singleton { val } => {
        match val.deref() {
          Literal::Atom(_) => true,
          _ => false,
        }
      }
      _ => false,
    };
  }

  /// Checks whether type is a literal atom of value
  pub fn is_lit_atom(&self, s: &str) -> bool {
    return match self {
      ErlType::Singleton { val } => {
        match val.deref() {
          Literal::Atom(actual) => actual == s,
          _ => false,
        }
      }
      _ => false,
    };
  }

  /// Checks whether type is a number
  pub fn is_number(&self) -> bool {
    return match self {
      ErlType::Number
      | ErlType::Float
      | ErlType::Integer
      | ErlType::IntegerRange { .. } => true,
      ErlType::Singleton { val } => {
        match val.deref() {
          Literal::Integer(_)
          | Literal::BigInteger
          | Literal::Float(_) => true,
          _ => false
        }
      }
      _ => false,
    };
  }

  /// Checks whether type is an integer number
  pub fn is_integer(&self) -> bool {
    return match self {
      ErlType::Integer
      | ErlType::IntegerRange { .. } => true,
      ErlType::Singleton { val } => {
        match val.deref() {
          Literal::Integer(_)
          | Literal::BigInteger => true,
          _ => false
        }
      }
      _ => false,
    };
  }

  /// Checks whether type is a floating point number (or an integer, because compatible why not)
  pub fn is_float(&self) -> bool {
    return match self {
      ErlType::Float => true,
      ErlType::Singleton { val } => {
        match val.deref() {
          Literal::Float(_)
          | Literal::Integer(_)
          | Literal::BigInteger => true,
          _ => false
        }
      }
      _ => false,
    };
  }

  /// Checks whether type is a tuple type
  pub fn is_tuple(&self) -> bool {
    return match self {
      ErlType::AnyTuple
      | ErlType::Tuple { .. }
      | ErlType::IntegerRange { .. } => true,
      _ => false,
    };
  }

  /// Checks whether type is a list
  pub fn is_list(&self) -> bool {
    return match self {
      ErlType::AnyList
      | ErlType::List { .. }
      | ErlType::StronglyTypedList { .. }
      | ErlType::Nil => true,
      ErlType::Singleton { val: singleton } => {
        match singleton.deref() {
          Literal::List { .. } | Literal::String { .. } => true,
          _ => false
        }
      }
      _ => false,
    };
  }

  /// Checks whether type is an empty list (NIL)
  pub fn is_nil(&self) -> bool {
    return match self {
      ErlType::Nil => true,
      _ => false,
    };
  }

  /// Checks whether type is a binary
  pub fn is_binary(&self) -> bool {
    return match self {
      ErlType::AnyBinary
      | ErlType::Binary { .. } => true,
      _ => false,
    };
  }

  /// Checks whether type is a map
  pub fn is_map(&self) -> bool {
    return match self {
      ErlType::AnyMap
      | ErlType::Map { .. } => true,
      _ => false,
    };
  }

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
