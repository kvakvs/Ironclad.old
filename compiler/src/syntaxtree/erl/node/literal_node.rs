//! Erlang literals, values fully known at compile time
use crate::typing::erl_type::ErlType;
use std::hash::{Hash, Hasher};

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

impl Hash for LiteralNode {
  fn hash<H: Hasher>(&self, state: &mut H) {
    match self {
      LiteralNode::Integer(n) => {
        'i'.hash(state);
        n.hash(state);
      }
      LiteralNode::Float(f) => {
        'f'.hash(state);
        format!("{}", f).hash(state);
      }
      LiteralNode::Atom(a) => {
        'a'.hash(state);
        a.hash(state);
      }
      LiteralNode::Bool(b) => {
        'b'.hash(state);
        b.hash(state);
      }
      LiteralNode::List(elements) => {
        'L'.hash(state);
        elements.hash(state);
      }
      LiteralNode::String(s) => {
        's'.hash(state);
        s.hash(state);
      }
      LiteralNode::Tuple(elements) => {
        'T'.hash(state);
        elements.hash(state);
      }
    }
  }

  fn hash_slice<H: Hasher>(data: &[Self], state: &mut H) where Self: Sized {
    data.iter().for_each(|d| d.hash(state))
  }
}

impl std::cmp::Eq for LiteralNode {}

impl LiteralNode {
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
