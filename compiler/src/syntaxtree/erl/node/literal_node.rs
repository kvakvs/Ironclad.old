//! Erlang literals, values fully known at compile time
use crate::typing::erl_type::ErlType;

/// An Erlang literal, a value fully known at compile time
#[derive(Clone, PartialEq)]
pub enum LiteralNode {
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
  List(Vec<LiteralNode>),

  /// A list containing only unicode codepoints is-a(List)
  String(String),

  /// A tuple of literals
  Tuple(Vec<LiteralNode>),
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

impl LiteralNode {
  /// Print a literal nicely
  pub fn to_string(&self) -> String {
    match self {
      LiteralNode::Integer(n) => format!("{}", n),
      LiteralNode::Float(f) => format!("{}", f),
      LiteralNode::Atom(a) => format!("'{}'", a),
      LiteralNode::Bool(b) => format!("{}", if *b { "'true'" } else { "'false'" }),
      // Cannot have runtime values as literals
      // ErlLit::Pid => format!("<pid>"),
      // ErlLit::Reference => format!("<ref>"),
      LiteralNode::List(items) => {
        let items_s = items.iter()
            .map(|i| i.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        format!("[{}]", items_s)
      }
      LiteralNode::String(s) => format!("\"{}\"", s), // TODO: Quote special characters
      LiteralNode::Tuple(items) => {
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
      LiteralNode::Integer(i) => ErlType::Integer(*i),
      LiteralNode::Float(_) => ErlType::Float,
      LiteralNode::Atom(s) => ErlType::Atom(s.clone()),
      LiteralNode::Bool(_) => ErlType::AnyBool,
      // Cannot have runtime values as literals
      // ErlLit::Pid => ErlType::Pid,
      // ErlLit::Reference => ErlType::Reference,
      LiteralNode::List(items) => {
        // List type is union of all element types
        ErlType::List(Box::new(ErlType::union_of_literal_types(items)))
      }
      LiteralNode::String(_) => ErlType::String, // is-a(list(char))
      LiteralNode::Tuple(items) => {
        ErlType::Tuple(items.iter()
            .map(|it| it.get_type())
            .collect())
      }
    }
  }
}
