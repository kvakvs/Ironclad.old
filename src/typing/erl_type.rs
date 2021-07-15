use crate::syntaxtree::erl::literal::ErlLiteral;
use crate::typing::typevar::TypeVar;

#[derive(Debug, Clone, PartialEq)]
pub struct RecordField {
  pub name: String,
  // Atom
  pub ty: ErlType,
}

impl RecordField {
  pub fn to_string(&self) -> String {
    format!("{}: {}", self.name, self.ty.to_string())
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MapField {
  pub key: ErlLiteral,
  // any key
  pub ty: ErlType,
}

impl MapField {
  pub fn to_string(&self) -> String {
    format!("{} => {}", self.key.to_string(), self.ty.to_string())
  }
}

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
  TypeVar(TypeVar),

  //-------------------
  // Erlang data types
  //-------------------

  /// Integers or floats
  Number,
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
  Record { tag: String, fields: Vec<RecordField> },

  Map(Vec<MapField>),

  /// Any atom. For specific atom values see Literal
  Atom,

  /// Atom 'true' or atom 'false', is-a(Atom)
  Bool,

  Pid,
  Reference,

  // TODO: Bits, binary with incomplete last byte
  Binary,

  Literal(ErlLiteral),

  /// Named function or unnamed, lambda
  Function { name: Option<String>, arg_ty: Vec<ErlType>, ret: Box<ErlType> },
}

impl ErlType {
  /// Given vector of literals, make a union type
  pub fn new_union_from_lit(items: &Vec<ErlLiteral>) -> ErlType {
    Self::new_union(
      items.iter()
          .map(|it| it.get_type())
          .collect())
  }

  pub fn new_union(types: Vec<ErlType>)-> Self {
    // TODO: Deduplicate and join overtypes with subtypes in some good way
    ErlType::Union(types)
  }

  pub fn new_fun(name: Option<String>, args: Vec<ErlType>, ret: ErlType) -> Self {
    ErlType::Function {
      name,
      arg_ty: args,
      ret: Box::from(ret),
    }
  }

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
}
