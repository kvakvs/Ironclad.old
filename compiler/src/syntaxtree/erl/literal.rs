use crate::typing::erl_type::ErlType;

#[derive(Debug, Clone, PartialEq)]
pub enum ErlLiteral {
  // TODO: Big integer
  /// Small enough to fit into a machine word
  Integer(isize),

  /// A 8-byte wide float
  Float(f64),

  /// Atom literal, also includes atoms 'true' and 'false'
  Atom(String),
  // TODO: String/list lit, tuple lit, map lit, binary lit, etc

  Bool(bool),
  Pid,
  Reference,

  List(Vec<ErlLiteral>),

  /// A list containing only unicode codepoints is-a(List)
  String(String),

  Tuple(Vec<ErlLiteral>),
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

impl ErlLiteral {
  pub fn to_string(&self) -> String {
    match self {
      ErlLiteral::Integer(n) => format!("{}", n),
      ErlLiteral::Float(f) => format!("{}", f),
      ErlLiteral::Atom(a) => format!("'{}'", a),
      ErlLiteral::Bool(b) => format!("{}", if *b { "'true'" } else { "'false'" }),
      ErlLiteral::Pid => format!("<pid>"),
      ErlLiteral::Reference => format!("<ref>"),
      ErlLiteral::List(items) => {
        let items_s = items.iter()
            .map(|i| i.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        format!("[{}]", items_s)
      }
      ErlLiteral::String(s) => format!("\"{}\"", s), // TODO: Quote special characters
      ErlLiteral::Tuple(items) => {
        let items_s = items.iter()
            .map(|i| i.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        format!("{{{}}}", items_s)
      }
    }
  }

  pub fn get_type(&self) -> ErlType {
    match self {
      ErlLiteral::Integer(_) => ErlType::Integer,
      ErlLiteral::Float(_) => ErlType::Float,
      ErlLiteral::Atom(_) => ErlType::Atom,
      ErlLiteral::Bool(_) => ErlType::Bool,
      ErlLiteral::Pid => ErlType::Pid,
      ErlLiteral::Reference => ErlType::Reference,
      ErlLiteral::List(items) => {
        // List type is union of all element types
        ErlType::List(Box::new(ErlType::new_union_from_lit(items)))
      }
      ErlLiteral::String(_) => ErlType::String,
      ErlLiteral::Tuple(items) => {
        ErlType::Tuple(items.iter()
            .map(|it| it.get_type())
            .collect())
      }
    }
  }
}
