//! Defines a type enum for any Erlang value or function
use crate::syntaxtree::erl::node::literal_node::LiteralNode;
use crate::typing::function_type::FunctionType;
use crate::typing::typevar::TypeVar;
// use enum_as_inner::EnumAsInner;

/// A record field definition
#[derive(Clone, PartialEq)]
pub struct RecordField {
  /// Field name, an atom stored as string
  pub name: String,
  /// Field value type TODO: by default record field types include 'undefined' atom
  pub ty: ErlType,
}

impl RecordField {
  /// String representation of a record field type
  pub fn to_string(&self) -> String {
    format!("{}: {}", self.name, self.ty.to_string())
  }
}

/// A map field constraint
#[derive(Clone, PartialEq)]
pub struct MapField {
  /// Key can be any literal
  pub key: LiteralNode,
  /// Value type
  pub ty: ErlType,
}

impl MapField {
  /// String representation of a map constraint
  pub fn to_string(&self) -> String {
    format!("{} => {}", self.key.to_string(), self.ty.to_string())
  }
}

/// Defines a type of any Erlang value or expression or function
#[derive(Clone, PartialEq)]
pub enum ErlType {
  //-------------------------------------
  // Special types, groups of types, etc
  //-------------------------------------
  /// Multiple types together
  // TODO: Vec replace with HashSet and impl hash for all ErlType's
  Union(Vec<ErlType>),
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
  IntegerConst(isize),

  /// 64 bit floating point, is-a(Number)
  Float,

  /// A list with element type, type can be union
  List(Box<ErlType>),

  /// A list of unicode codepoints list(char())
  String,

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
  pub fn union_of_literal_types(items: &Vec<LiteralNode>) -> ErlType {
    Self::union_of(
      items.iter()
          .map(|it| it.get_type())
          .collect())
  }

  /// Creates a new union of types from a vec of types. Tries to unfold nested union types and
  /// flatten them while also trying to maintain uniqueness (see to do below)
  pub fn union_of(types: Vec<ErlType>) -> Self {
    assert!(types.len() > 0, "Can't create union of 0 types");
    if types.len() == 1 {
      return types[0].clone();
    }

    let mut merged: Vec<ErlType> = Vec::new(); // TODO: Use BTreeSet or HashSet for uniqueness

    // For every type in types, if its a Union, unfold it into member types.
    // HashSet will ensure that the types are unique
    types.into_iter().for_each(|t| {
      match t {
        ErlType::Union(members) => {
          members.iter().for_each(
            |m| merged.push(m.clone())
          )
        }
        _ => merged.push(t),
      }
    });
    ErlType::Union(merged)
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

  /// Return type expressed as a printable string
  pub fn to_string(&self) -> String {
    match self {
      ErlType::Union(types) => {
        types.iter().map(|t| t.to_string())
            .collect::<Vec<String>>()
            .join(" | ")
      }
      ErlType::None => String::from("none()"),
      ErlType::Any => String::from("any()"),
      ErlType::Number => String::from("number()"),
      ErlType::AnyInteger => String::from("integer()"),
      ErlType::IntegerConst(i) => format!("{}", i),
      ErlType::Float => String::from("float()"),
      ErlType::List(ty) => format!("list({})", ty.to_string()),
      ErlType::Tuple(items) => {
        let items_s = items.iter().map(|t| t.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        format!("{{{}}}", items_s)
      }
      ErlType::Record { tag, fields: types } => {
        let fields_s = types.iter().map(|t| t.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        format!("#{}{{{}}}", tag, fields_s)
      }
      ErlType::Map(fields) => {
        let fields_s = fields.iter().map(|t| t.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        format!("#{{{}}}", fields_s)
      }
      ErlType::AnyAtom => String::from("atom()"),
      ErlType::Atom(s) => format!("'{}'", s),

      ErlType::AnyBool => String::from("bool()"),
      ErlType::Pid => String::from("pid()"),
      ErlType::Reference => String::from("reference()"),
      ErlType::Binary => String::from("binary()"),
      ErlType::BinaryBits => String::from("bits()"),
      ErlType::Literal(lit) => lit.to_string(),
      ErlType::LocalFunction { name, arity } => format!("fun {}/{}", name, arity),
      ErlType::Function(fun_type) => {
        let args_s = fun_type.arg_types.iter()
            .map(|t| t.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        match &fun_type.name {
          None => format!("fun(({}) -> {})", args_s, fun_type.ret_type.to_string()),
          Some(n) => format!("{}({}) -> {}", n, args_s, fun_type.ret_type.to_string()),
        }
      }
      ErlType::TVar(tv) => tv.to_string(),
      ErlType::String => format!("string()"), // a list of unicode codepoint: list(char())
    }
  }

  /// Check whether a type denotes a simple non-nested value or a union of simple values, i.e. when
  /// the deeper type inspection is not required.
  pub fn is_simple_value_type(&self) -> bool {
    match self {
      ErlType::Union(members) => {
        members.iter().all(|m| m.is_simple_value_type())
      }
      ErlType::Number | ErlType::AnyInteger | ErlType::IntegerConst(_) | ErlType::Float
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
        // TODO: uniqueness check
        new_members.push(t.clone());
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
          1 => members[0].clone(),
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
      _ => panic!("Node {:?} is expected to be a Function type", self)
    }
  }
}
