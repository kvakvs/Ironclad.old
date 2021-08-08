//! Erlang literals, values fully known at compile time
use crate::typing::erl_type::ErlType;
use std::hash::{Hash, Hasher};

/// An Erlang literal, a value fully known at compile time
#[derive(Clone)]
pub enum Literal {
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
  List(Vec<Literal>),

  /// A list containing only unicode codepoints is-a(List)
  String(String),

  /// A tuple of literals
  Tuple(Vec<Literal>),
}

impl Hash for Literal {
  fn hash<H: Hasher>(&self, state: &mut H) {
    match self {
      Literal::Integer(n) => {
        'i'.hash(state);
        n.hash(state);
      }
      Literal::Float(f) => {
        'f'.hash(state);
        format!("{}", f).hash(state);
      }
      Literal::Atom(a) => {
        'a'.hash(state);
        a.hash(state);
      }
      Literal::Bool(b) => {
        'b'.hash(state);
        b.hash(state);
      }
      Literal::List(elements) => {
        'L'.hash(state);
        elements.hash(state);
      }
      Literal::String(s) => {
        's'.hash(state);
        s.hash(state);
      }
      Literal::Tuple(elements) => {
        'T'.hash(state);
        elements.hash(state);
      }
    }
  }

  fn hash_slice<H: Hasher>(data: &[Self], state: &mut H) where Self: Sized {
    data.iter().for_each(|d| d.hash(state))
  }
}

impl std::cmp::Eq for Literal {}

impl Literal {
  /// Retrieves a type of a literal
  pub fn get_type(&self) -> ErlType {
    match self {
      Literal::Integer(i) => ErlType::Integer(*i),
      Literal::Float(_) => ErlType::Float,
      Literal::Atom(s) => ErlType::Atom(s.clone()),
      Literal::Bool(_) => ErlType::AnyBool,
      // Cannot have runtime values as literals
      // ErlLit::Pid => ErlType::Pid,
      // ErlLit::Reference => ErlType::Reference,
      Literal::List(items) => {
        // List type is union of all element types
        ErlType::List(Box::new(ErlType::union_of_literal_types(items)))
      }
      Literal::String(_) => ErlType::String, // is-a(list(char))
      Literal::Tuple(items) => {
        ErlType::Tuple(items.iter()
            .map(|it| it.get_type())
            .collect())
      }
    }
  }
}

impl PartialEq for Literal {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      (Literal::Integer(a), Literal::Integer(b)) => a == b,
      (Literal::Float(a), Literal::Float(b)) => (a - b).abs() <= f64::EPSILON,
      (Literal::Atom(a), Literal::Atom(b)) => a == b,
      (Literal::Bool(a), Literal::Bool(b)) => a == b,
      (Literal::List(a), Literal::List(b)) => a == b,
      (Literal::String(a), Literal::String(b)) => a == b,
      (Literal::Tuple(a), Literal::Tuple(b)) => a == b,
      _ => false,
    }
  }
}