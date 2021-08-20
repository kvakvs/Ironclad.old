//! Defines a type enum for any Erlang value or function
use std::fmt::Formatter;
use std::hash::{Hash, Hasher};

use ::function_name::named;

use crate::erlang::syntax_tree::node::literal::Literal;
use crate::typing::fn_type::FunctionType;
use crate::typing::typevar::TypeVar;
use std::collections::BTreeSet;
use std::cmp::Ordering;
use crate::typing::fn_clause_type::FnClauseType;

// use enum_as_inner::EnumAsInner;

/// A record field definition
#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
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
#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct MapField {
  /// Key can be any literal
  pub key: Literal,
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
  Union(BTreeSet<ErlType>),
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

  /// A list of zero size
  Nil,

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
  /// A port (socket, open file, etc.)
  Port,

  /// A binary type containing bytes and some trailing bits (incomplete last byte)
  BinaryBits,

  /// A binary type containing bytes, is-a(BinaryBits)
  Binary,

  /// Type for an Erlang Literal value, a data value fully known at compile time, having no type
  /// variables or references to other types or other data
  Literal(Literal),

  /// Any callable
  AnyFn,

  /// Named function or unnamed
  Fn(FunctionType),
  /// Unnamed function clause, just argument types and return, this should also class as a function
  FnClause(FnClauseType),
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
      | (ErlType::AnyFn, ErlType::AnyFn) => true,

      // (ErlType::Callable(fa1), ErlType::Callable(fa2)) => fa1 == fa2,

      (ErlType::Record { fields: f1, tag: t1, .. },
        ErlType::Record { fields: f2, tag: t2, .. }) => t1 == t2 && f1 == f2,
      (ErlType::TVar(u), ErlType::TVar(v)) => u == v,
      (ErlType::Integer(u), ErlType::Integer(v)) => u == v,
      (ErlType::List(u), ErlType::List(v)) => u == v,
      (ErlType::Tuple(u), ErlType::Tuple(v)) => u == v,
      (ErlType::Map(u), ErlType::Map(v)) => u == v,
      (ErlType::Atom(a), ErlType::Atom(b)) => a == b,
      (ErlType::Literal(u), ErlType::Literal(v)) => u == v,
      (ErlType::Fn(fa), ErlType::Fn(fb)) => fa == fb,
      (ErlType::Union(u), ErlType::Union(v)) => {
        u.iter().all(|u_member| v.contains(u_member))
      }
      _ => false,
    }
  }
}

impl PartialOrd<Self> for ErlType {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl Ord for ErlType {
  fn cmp(&self, other: &Self) -> Ordering {
    let order = self.get_order().cmp(&other.get_order());
    match order {
      Ordering::Less | Ordering::Greater => order,
      Ordering::Equal => self.cmp_same_type(other),
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
      ErlType::AnyFn => "f*".hash(state),
      ErlType::Fn(ft) => {
        "fn".hash(state);
        ft.hash(state);
      }
      ErlType::FnClause(fc) => {
        "fc".hash(state);
        fc.hash(state);
      }
      // ErlType::Callable(fa) => {
      //   '='.hash(state);
      //   fa.hash(state);
      // }
      ErlType::Nil => "[]".hash(state),
      ErlType::Port => 'P'.hash(state),
    }
  }

  fn hash_slice<H: Hasher>(data: &[Self], state: &mut H) where Self: Sized {
    data.iter().for_each(|d| d.hash(state))
  }
}

impl ErlType {
  /// Compares two erlang types of same kind, otherwise general ordering applies
  pub fn cmp_same_type(&self, other: &ErlType) -> Ordering {
    match (self, other) {
      (ErlType::None, ErlType::None) | (ErlType::Any, ErlType::Any) | (ErlType::Number, ErlType::Number)
      | (ErlType::AnyInteger, ErlType::AnyInteger) | (ErlType::Float, ErlType::Float)
      | (ErlType::AnyList, ErlType::AnyList) | (ErlType::Nil, ErlType::Nil)
      | (ErlType::String, ErlType::String) | (ErlType::AnyTuple, ErlType::AnyTuple)
      | (ErlType::AnyAtom, ErlType::AnyAtom) | (ErlType::AnyBool, ErlType::AnyBool)
      | (ErlType::Pid, ErlType::Pid) | (ErlType::Reference, ErlType::Reference)
      | (ErlType::Port, ErlType::Port) | (ErlType::BinaryBits, ErlType::BinaryBits)
      | (ErlType::Binary, ErlType::Binary) | (ErlType::AnyFn, ErlType::AnyFn) => {
        Ordering::Equal
      }

      (ErlType::Union(a), ErlType::Union(b)) => a.cmp(b),
      (ErlType::TVar(TypeVar(a)), ErlType::TVar(TypeVar(b))) => a.cmp(b),
      (ErlType::Integer(a), ErlType::Integer(b)) => a.cmp(b),
      (ErlType::List(l1), ErlType::List(l2)) => l1.cmp(l2),
      (ErlType::Tuple(t1), ErlType::Tuple(t2)) => t1.cmp(t2),
      (ErlType::Record { tag: t1, fields: fields1 },
        ErlType::Record { tag: t2, fields: fields2 }) => {
        let mut result = t1.cmp(t2);
        if result == Ordering::Equal { // if record tags match, fields may be different!
          result = fields1.cmp(&fields2);
        }
        result
      }
      (ErlType::Map(m1), ErlType::Map(m2)) => m1.cmp(&m2),
      (ErlType::Atom(a), ErlType::Atom(b)) => a.cmp(b),
      (ErlType::Literal(lit1), ErlType::Literal(lit2)) => lit1.cmp(&lit2),
      (ErlType::Fn(f1), ErlType::Fn(f2)) => f1.cmp(f2),
      // (ErlType::Callable(fa1), ErlType::Callable(fa2)) => fa1.cmp(fa2),

      _ => unreachable!("Don't know how to compare {} vs {}, only same type allowed in this function",
                        self, other)
    }
  }

  /// Return a number placing the type somewhere in the type ordering hierarchy
  /// number < atom < reference < fun < port < pid < tuple < map < nil < list < bit string
  /// This ordering is used for BTree construction, not
  pub fn get_order(&self) -> usize {
    match self {
      ErlType::None => 0,
      ErlType::Union(_) => 1,
      ErlType::TVar(_) => 2,
      ErlType::Any => 1000,

      ErlType::Number => 10,
      ErlType::Float => 11,
      ErlType::AnyInteger => 12,
      ErlType::Integer(_) => 13,

      ErlType::AnyBool => 20,
      ErlType::AnyAtom => 21,
      ErlType::Atom(_) => 22,

      ErlType::Reference => 30,

      ErlType::AnyFn => 40,
      ErlType::Fn(_) => 41,
      ErlType::FnClause(_) => 42,
      // ErlType::Callable(_) => 43,

      ErlType::Port => 50,

      ErlType::Pid => 60,

      ErlType::AnyTuple => 70,
      ErlType::Tuple(_) => 71,
      ErlType::Record { .. } => 72,

      ErlType::Map(_) => 80,

      ErlType::Nil => 90,

      ErlType::AnyList => 100,
      ErlType::List(_) => 101,
      ErlType::String => 102,

      ErlType::Binary => 110,
      ErlType::BinaryBits => 111,

      ErlType::Literal(lit) => lit.get_type().get_order(),
    }
  }

  /// If self is a TypeVar, return Any, otherwise do not change.
  /// Used after type inference was done to replace all typevars with any()
  pub fn into_final_type(self) -> Self {
    match self {
      ErlType::TVar(_) => ErlType::Any,
      other => other,
    }
  }

  /// Given vector of literals, make a union type
  pub fn union_of_literal_types(items: &[Literal]) -> ErlType {
    Self::union_of(
      items.iter()
          .map(|it| it.get_type())
          .collect(),
      true)
  }

  /// Creates an empty union type, equivalent to None
  pub fn union_empty() -> Self {
    ErlType::Union(Default::default())
  }

  /// Creates a new union of types from a vec of types. Tries to unfold nested union types and
  /// flatten them while also trying to maintain uniqueness (see to do below).
  /// Param `promote` will also call promote on the resulting union, trying to merge some type
  /// combinations into supertypes.
  pub fn union_of(types: Vec<ErlType>, promote: bool) -> Self {
    if types.len() == 1 {
      return types[0].clone();
    }

    let mut merged: BTreeSet<ErlType> = Default::default();

    // For every type in types, if its a Union, unfold it into member types.
    // HashSet will ensure that the types are unique
    types.into_iter().for_each(|t| {
      match t {
        ErlType::Union(members) => merged.extend(members.iter().cloned()),
        _ => { merged.insert(t); }
      }
    });
    if promote { ErlType::union_promote(merged) } else { ErlType::Union(merged) }
  }

  /// Given a hashset of erlang types try and promote combinations of simpler types to a compound
  /// type if such type exists. This is lossless operation, types are not generalized or shrunk.
  /// Example: integer()|float() shrink into number()
  #[named]
  fn union_promote(elements: BTreeSet<ErlType>) -> ErlType {
    assert!(!elements.is_empty(), "Union of 0 types is not valid, fill it with some types then call {}", function_name!());
    if elements.len() == 2 {
      // integer() | float() => number()
      if elements.contains(&ErlType::Float) && elements.contains(&ErlType::AnyInteger) {
        return ErlType::Number;
      }
    }
    ErlType::Union(elements)
  }

  // /// Create a new function type provided args types and return type, possibly with a name
  // pub fn new_fun_type(name: Option<String>, clauses: Vec<FunctionClauseType>) -> Self {
  //   assert!(!clauses.is_empty(), "Function type with 0 function clause types is not allowed");
  //   ErlType::Function(FunctionType::new(name, clauses))
  // }

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
      Self::Fn(ft) => &ft,
      _ => panic!("Node {} is expected to be a Function type", self)
    }
  }

  /// Create an atom from a string slice
  pub fn atom_str(s: &str) -> Self {
    ErlType::Atom(String::from(s))
  }
}

impl From<TypeVar> for ErlType { fn from(tv: TypeVar) -> Self { ErlType::TVar(tv) } }
impl From<&TypeVar> for ErlType { fn from(tv: &TypeVar) -> Self { ErlType::TVar(tv.clone()) } }
