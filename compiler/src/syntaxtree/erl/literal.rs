//! Erlang literals, values fully known at compile time
use crate::typing::erl_type::ErlType;

/// An Erlang literal, a value fully known at compile time
#[derive(Debug, Clone, PartialEq)]
pub enum ErlLit {
  // TODO: Big integer
  /// Small enough to fit into a machine word
  Integer(isize),

  /// A 8-byte wide float
  Float(f64),

  /// Atom literal, also includes atoms 'true' and 'false'
  Atom(String),
  // TODO: String/list lit, tuple lit, map lit, binary lit, etc

  /// A boolean value true or false atom, is-a(Atom)
  Bool(bool),

  // Cannot have runtime values as literals
  // Pid,
  // Reference,

  /// A list of literals
  List(Vec<ErlLit>),

  /// A list containing only unicode codepoints is-a(List)
  String(String),

  /// A tuple of literals
  Tuple(Vec<ErlLit>),
}

// lazy_static! {
//   static ref ERLTYPE_Integer: ErlType = ErlType::Integer;
//   static ref ERLTYPE_Float: ErlType = ErlType::Float;
//   static ref ERLTYPE_Atom: ErlType = ErlType::Atom;
//   static ref ERLTYPE_Bool: ErlType = ErlType::Bool;
//   static ref ERLTYPE_Pid: ErlType = ErlType::Pid;
//   static ref ERLTYPE_Reference: ErlType = ErlType::Reference;
//   static ref ERLTYPE_List: ErlType = ErlType::List;
//   static ref ERLTYPE_String: ErlType = ErlType::String;
//   static ref ERLTYPE_Tuple: ErlType = ErlType::Tuple;
// }

impl ErlLit {
  /// Print a literal nicely
  pub fn to_string(&self) -> String {
    match self {
      ErlLit::Integer(n) => format!("{}", n),
      ErlLit::Float(f) => format!("{}", f),
      ErlLit::Atom(a) => format!("'{}'", a),
      ErlLit::Bool(b) => format!("{}", if *b { "'true'" } else { "'false'" }),
      // Cannot have runtime values as literals
      // ErlLit::Pid => format!("<pid>"),
      // ErlLit::Reference => format!("<ref>"),
      ErlLit::List(items) => {
        let items_s = items.iter()
            .map(|i| i.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        format!("[{}]", items_s)
      }
      ErlLit::String(s) => format!("\"{}\"", s), // TODO: Quote special characters
      ErlLit::Tuple(items) => {
        let items_s = items.iter()
            .map(|i| i.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        format!("{{{}}}", items_s)
      }
    }
  }

  /// Retrieves a type of a literal
  pub fn get_type(&self) -> ErlType {
    match self {
      ErlLit::Integer(_) => ErlType::Integer,
      ErlLit::Float(_) => ErlType::Float,
      ErlLit::Atom(_) => ErlType::Atom,
      ErlLit::Bool(_) => ErlType::Bool,
      // Cannot have runtime values as literals
      // ErlLit::Pid => ErlType::Pid,
      // ErlLit::Reference => ErlType::Reference,
      ErlLit::List(items) => {
        // List type is union of all element types
        ErlType::List(Box::new(ErlType::new_union_from_lit(items)))
      }
      ErlLit::String(_) => ErlType::String, // is-a(list(char))
      ErlLit::Tuple(items) => {
        ErlType::Tuple(items.iter()
            .map(|it| it.get_type())
            .collect())
      }
    }
  }
}
