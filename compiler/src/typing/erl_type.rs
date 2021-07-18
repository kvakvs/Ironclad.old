//! Defines a type enum for any Erlang value or function
use crate::syntaxtree::erl::literal::ErlLit;
use crate::typing::typevar::TypeVar;

/// A record field definition
#[derive(Debug, Clone, PartialEq)]
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
#[derive(Debug, Clone, PartialEq)]
pub struct MapField {
  /// Key can be any literal
  pub key: ErlLit,
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
#[derive(Debug, Clone, PartialEq)]
pub enum ErlType {
  //-------------------------------------
  // Special types, groups of types, etc
  //-------------------------------------
  /// Multiple types together
  Union(Vec<ErlType>),
  /// No type, usually signifies a type error
  None,
  /// All types, usually signifies an unchecked or untyped type
  Any,
  /// A type variable, unique generated integer id
  TypeVar(TypeVar),

  //-------------------
  // Erlang data types
  //-------------------

  /// Integers or floats
  Number,
  /// A integer number
  Integer,
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
  Atom,

  /// Atom 'true' or atom 'false', is-a(Atom)
  Bool,

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
  Literal(ErlLit),

  /// Named function or unnamed
  Function {
    /// Name if known, for module level functions, or unnamed for anonymous funs
    name: Option<String>,
    /// Types of input args
    arg_ty: Vec<ErlType>,
    /// Return type
    ret: Box<ErlType>,
  },
}

impl ErlType {
  /// Given vector of literals, make a union type
  pub fn new_union_from_lit(items: &Vec<ErlLit>) -> ErlType {
    Self::new_union(
      items.iter()
          .map(|it| it.get_type())
          .collect())
  }

  /// Creates a new union of types from a vec of types. Tries to unfold nested union types and
  /// flatten them while also trying to maintain uniqueness (see to do below)
  pub fn new_union(types: Vec<ErlType>) -> Self {
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
  pub fn new_fun(name: Option<String>, args: Vec<ErlType>, ret: ErlType) -> Self {
    ErlType::Function {
      name,
      arg_ty: args,
      ret: Box::from(ret),
    }
  }

  /// Create a new type, containing a new type variable with unique integer id
  pub fn new_typevar() -> Self {
    ErlType::TypeVar(TypeVar::new())
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
      ErlType::Integer => String::from("integer()"),
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
      ErlType::Atom => String::from("atom()"),
      ErlType::Bool => String::from("bool()"),
      ErlType::Pid => String::from("pid()"),
      ErlType::Reference => String::from("reference()"),
      ErlType::Binary => String::from("binary()"),
      ErlType::BinaryBits => String::from("bits()"),
      ErlType::Literal(lit) => lit.to_string(),
      ErlType::Function { name, arg_ty: args, ret } => {
        let args_s = args.iter().map(|t| t.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        match name {
          None => format!("fun(({}) -> {})", args_s, ret.to_string()),
          Some(n) => format!("{}({}) -> {}", n, args_s, ret.to_string()),
        }
      }
      ErlType::TypeVar(tv) => tv.to_string(),
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
      ErlType::Number | ErlType::Integer | ErlType::Float | ErlType::Atom | ErlType::Bool
      | ErlType::List(_) | ErlType::String | ErlType::Tuple(_) | ErlType::Binary
      | ErlType::Map(_) | ErlType::Record { .. }
      | ErlType::Pid | ErlType::Reference
      | ErlType::Literal(_) => true,
      _ => false,
    }
  }
}
