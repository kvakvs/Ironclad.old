//! Defines a type enum for any Erlang value or function
use std::collections::HashSet;
use std::fmt::Formatter;
use std::hash::{Hash, Hasher};

use crate::syntaxtree::erl::node::literal_node::LiteralNode;
use crate::typing::function_type::FunctionType;
use crate::typing::typevar::TypeVar;

// use enum_as_inner::EnumAsInner;

/// A record field definition
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct RecordField {
  /// Field name, an atom stored as string
  pub name: String,
  /// Field value type TODO: by default record field types include 'undefined' atom
  pub ty: ErlType,
}

impl std::fmt::Display for RecordField {
  /// String representation of a record field type
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}: {}", self.name, self.ty)
  }
}

/// A map field constraint
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct MapField {
  /// Key can be any literal
  pub key: LiteralNode,
  /// Value type
  pub ty: ErlType,
}

impl std::fmt::Display for MapField {
  /// String representation of a map field
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{} => {}", self.key, self.ty)
  }
}

/// Defines a type of any Erlang value or expression or function
#[derive(Clone, Eq)]
pub enum ErlType {
  //-------------------------------------
  // Special types, groups of types, etc
  //-------------------------------------
  /// Multiple types together
  Union(HashSet<ErlType>),
  /// No type, usually signifies a type error
  None,
  /// All types, usually signifies an unchecked or untyped type
  Any,
  /// A type variable, unique generated integer id
  TVar(TypeVar),

  //-------------------
  // Erlang data types
  //-------------------

  // TODO: Integer ranges, maybe float ranges possible too?
  /// Integers or floats
  Number,

  /// A integer number
  AnyInteger,

  /// Specific integer value
  Integer(isize),

  /// 64 bit floating point, is-a(Number)
  Float,

  /// A list of any type
  AnyList,

  /// A list with element type, type can be union
  List(Box<ErlType>),

  /// A list of unicode codepoints list(char())
  String,

  /// A tuple of any size and any content
  AnyTuple,

  /// A tuple with each element type defined
  Tuple(Vec<ErlType>),

  /// Special case of a tagged tuple where we know a record definition exists. is-a(Tuple)
  Record {
    /// Atom tag for record. Stored as string here.
    tag: String,
    /// List of record fields
    fields: Vec<RecordField>,
  },

  /// A map with a vec of field constraints
  Map(Vec<MapField>),

  /// Any atom. For specific atom values see Literal
  AnyAtom,

  /// Specific atom value
  Atom(String),

  /// Atom 'true' or atom 'false', is-a(Atom)
  AnyBool,

  /// A process id value, generated at runtime
  Pid,

  /// A reference value, generated at runtime
  Reference,

  /// A binary type containing bytes and some trailing bits (incomplete last byte)
  BinaryBits,

  /// A binary type containing bytes, is-a(BinaryBits)
  Binary,

  /// Type for an Erlang Literal value, a data value fully known at compile time, having no type
  /// variables or references to other types or other data
  Literal(LiteralNode),

  /// Any callable
  AnyFunction,

  /// Named function or unnamed
  Function(FunctionType),

  /// Refers to a function in local module
  LocalFunction {
    /// Atom of the function name
    name: String,
    /// How many args
    arity: usize,
  },
}

impl PartialEq for ErlType {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      (ErlType::Number, ErlType::Number) | (ErlType::None, ErlType::None)
      | (ErlType::Any, ErlType::Any) | (ErlType::Float, ErlType::Float)
      | (ErlType::String, ErlType::String) | (ErlType::AnyList, ErlType::AnyList)
      | (ErlType::AnyTuple, ErlType::AnyTuple) | (ErlType::AnyAtom, ErlType::AnyAtom)
      | (ErlType::AnyBool, ErlType::AnyBool) | (ErlType::Pid, ErlType::Pid)
      | (ErlType::Reference, ErlType::Reference) | (ErlType::BinaryBits, ErlType::BinaryBits)
      | (ErlType::AnyInteger, ErlType::AnyInteger) | (ErlType::Binary, ErlType::Binary)
      | (ErlType::AnyFunction, ErlType::AnyFunction) => true,

      (ErlType::LocalFunction { name: n1, arity: a1 },
        ErlType::LocalFunction { name: n2, arity: a2 }) => n1 == n2 && a1 == a2,

      (ErlType::Record { fields: f1, tag: t1, .. },
        ErlType::Record { fields: f2, tag: t2, .. }) => t1 == t2 && f1 == f2,
      (ErlType::TVar(u), ErlType::TVar(v)) => u == v,
      (ErlType::Integer(u), ErlType::Integer(v)) => u == v,
      (ErlType::List(u), ErlType::List(v)) => u == v,
      (ErlType::Tuple(u), ErlType::Tuple(v)) => u == v,
      (ErlType::Map(u), ErlType::Map(v)) => u == v,
      (ErlType::Atom(a), ErlType::Atom(b)) => a == b,
      (ErlType::Literal(u), ErlType::Literal(v)) => u == v,
      (ErlType::Function(fa), ErlType::Function(fb)) => fa == fb,
      (ErlType::Union(u), ErlType::Union(v)) => u == v,
      _ => false,
    }
  }
}

impl Hash for ErlType {
  fn hash<H: Hasher>(&self, state: &mut H) {
    match self {
      ErlType::Union(members) => {
        'U'.hash(state);
        members.iter().for_each(|m| m.hash(state));
      }
      ErlType::None => 'z'.hash(state),
      ErlType::Any => '*'.hash(state),
      ErlType::TVar(n) => {
        't'.hash(state);
        n.hash(state);
      }
      ErlType::Number => 'N'.hash(state),
      ErlType::AnyInteger => 'I'.hash(state),
      ErlType::Integer(i) => {
        'i'.hash(state);
        i.hash(state);
      }
      ErlType::Float => 'f'.hash(state),
      ErlType::AnyList => 'L'.hash(state),
      ErlType::List(ltype) => {
        'l'.hash(state);
        (*ltype).hash(state);
      }
      ErlType::String => 'S'.hash(state),
      ErlType::AnyTuple => 'T'.hash(state),
      ErlType::Tuple(items) => {
        't'.hash(state);
        items.iter().for_each(|i| i.hash(state));
      }
      ErlType::Record { tag, fields } => {
        'r'.hash(state);
        tag.hash(state);
        fields.iter().for_each(|f| f.hash(state));
      }
      ErlType::Map(mf) => {
        '#'.hash(state);
        mf.iter().for_each(|f| f.hash(state));
      }
      ErlType::AnyAtom => 'A'.hash(state),
      ErlType::Atom(s) => {
        'a'.hash(state);
        s.hash(state);
      }
      ErlType::AnyBool => 'B'.hash(state),
      ErlType::Pid => 'p'.hash(state),
      ErlType::Reference => 'r'.hash(state),
      ErlType::BinaryBits => '.'.hash(state),
      ErlType::Binary => 'b'.hash(state),
      ErlType::Literal(lit) => lit.hash(state),
      ErlType::AnyFunction => '>'.hash(state),
      ErlType::Function(ft) => {
        '<'.hash(state);
        ft.hash(state);
      }
      ErlType::LocalFunction { name, arity } => {
        '='.hash(state);
        name.hash(state);
        arity.hash(state);
      }
    }
  }

  fn hash_slice<H: Hasher>(data: &[Self], state: &mut H) where Self: Sized {
    data.iter().for_each(|d| d.hash(state))
  }
}

impl ErlType {
  /// If self is a TypeVar, return Any, otherwise do not change.
  /// Used after type inference was done to replace all typevars with any()
  pub fn into_final_type(self) -> Self {
    match self {
      ErlType::TVar(_) => ErlType::Any,
      other => other,
    }
  }

  /// Given vector of literals, make a union type
  pub fn union_of_literal_types(items: &[LiteralNode]) -> ErlType {
    Self::union_of(
      items.iter()
          .map(|it| it.get_type())
          .collect())
  }

  /// Creates an empty union type, equivalent to None
  pub fn union_empty() -> Self {
    ErlType::Union(Default::default())
  }

  /// Creates a new union of types from a vec of types. Tries to unfold nested union types and
  /// flatten them while also trying to maintain uniqueness (see to do below)
  pub fn union_of(types: Vec<ErlType>) -> Self {
    assert!(!types.is_empty(), "Can't create union of 0 types");
    if types.len() == 1 {
      return types[0].clone();
    }

    let mut merged: HashSet<ErlType> = Default::default();

    // For every type in types, if its a Union, unfold it into member types.
    // HashSet will ensure that the types are unique
    types.into_iter().for_each(|t| {
      match t {
        ErlType::Union(members) => merged.extend(members.iter().cloned()),
        _ => { merged.insert(t); }
      }
    });
    Self::union_promote(merged)
  }

  /// Given a hashset of erlang types try and promote combinations of simpler types to a compound
  /// type if such type exists. This is lossless operation, types are not generalized or shrunk.
  /// Example: integer()|float() shrink into number()
  fn union_promote(elements: HashSet<ErlType>) -> ErlType {
    if elements.len() == 2 {
      // integer() | float() => number()
      if elements.contains(&ErlType::Float) && elements.contains(&ErlType::AnyInteger) {
        return ErlType::Number;
      }
    }
    ErlType::Union(elements)
  }

  /// Create a new function type provided args types and return type, possibly with a name
  pub fn new_localref(name: &str, arity: usize) -> Self {
    ErlType::LocalFunction {
      name: String::from(name),
      arity,
    }
  }

  /// Create a new function type provided args types and return type, possibly with a name
  pub fn new_fun_type(name: Option<String>, args: Vec<ErlType>, ret: ErlType) -> Self {
    ErlType::Function(FunctionType {
      name,
      arg_types: args,
      ret_type: Box::from(ret),
    })
  }

  /// Create a new type, containing a new type variable with unique integer id
  pub fn new_typevar() -> Self {
    ErlType::TVar(TypeVar::new())
  }

  /// Check whether a type denotes a simple non-nested value or a union of simple values, i.e. when
  /// the deeper type inspection is not required.
  pub fn is_simple_value_type(&self) -> bool {
    match self {
      ErlType::Union(members) => {
        members.iter().all(|m| m.is_simple_value_type())
      }
      ErlType::Number | ErlType::AnyInteger | ErlType::Integer(_) | ErlType::Float
      | ErlType::AnyAtom | ErlType::Atom(_) | ErlType::AnyBool
      | ErlType::List(_) | ErlType::String | ErlType::Tuple(_) | ErlType::Binary
      | ErlType::Map(_) | ErlType::Record { .. }
      | ErlType::Pid | ErlType::Reference
      | ErlType::Literal(_) => true,
      _ => false,
    }
  }

  /// Given a union type, add t to the union, with uniqueness check
  /// Returns Some() if the union was modified, None if no update has happened
  pub fn union_add(&self, t: &ErlType) -> Option<Self> {
    match self {
      ErlType::Union(members) => {
        let mut new_members = members.clone();
        new_members.insert(t.clone());
        Some(ErlType::Union(new_members))
      }
      _ => None,
    }
  }

  /// For Union type, if it contains empty set, collapses into None, if contains one type - will
  /// collapse into that type.
  pub fn union_collapse(&self) -> Self {
    match self {
      ErlType::Union(members) => {
        match members.len() {
          1 => members.iter().next().unwrap().clone(),
          0 => ErlType::None,
          _ => self.clone(),
        }
      }
      _ => unreachable!("ErlType::union_collapse called on not-a-Union type")
    }
  }

  /// Retrieve inner FunctionType value or fail
  pub fn as_function(&self) -> &FunctionType {
    match self {
      Self::Function(ft) => &ft,
      _ => panic!("Node {} is expected to be a Function type", self)
    }
  }
}

impl From<TypeVar> for ErlType {
  fn from(tv: TypeVar) -> Self {
    ErlType::TVar(tv)
  }
}
