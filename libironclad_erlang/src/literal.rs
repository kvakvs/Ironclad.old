//! Erlang literals, values fully known at compile time
use std::cmp::Ordering;
use std::hash::{Hash, Hasher};

use crate::typing::erl_integer::ErlInteger;
use crate::typing::erl_type::{ErlType, ErlTypeImpl};
use std::sync::Arc;

/// An Erlang literal, a value fully known at compile time
#[derive(Clone, Debug)]
pub enum Literal {
  /// Small enough to fit into a machine word
  Integer(ErlInteger),

  /// A 8-byte wide float
  Float(f64),

  /// Atom literal, also includes atoms 'true' and 'false'
  Atom(String),
  /// Character literal
  Character(char),
  /// Character literal, escaped with `\`
  EscapedCharacter { value: char, in_source: char },

  // TODO: String/list lit, tuple lit, map lit, binary lit, etc
  /// A boolean value true or false atom, is-a(Atom)
  Bool(bool),

  // Cannot have runtime values as literals
  // Pid,
  // Reference,
  /// A list of literals
  List {
    /// List elements
    elements: Vec<Literal>,
    /// Optional tail element or None if NIL
    tail: Option<Box<Literal>>,
  },

  /// An empty list `[]`
  Nil,
  /// An empty binary `<< >>`
  EmptyBinary,

  /// A list containing only unicode codepoints is-a(List)
  String(Arc<String>),

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
      Literal::List { elements, .. } => {
        'L'.hash(state);
        elements.hash(state);
      }
      Literal::Nil => {
        "[]".hash(state);
      }
      Literal::EmptyBinary => {
        "<<>>".hash(state);
      }
      Literal::String(s) => {
        's'.hash(state);
        s.hash(state);
      }
      Literal::Tuple(elements) => {
        'T'.hash(state);
        elements.hash(state);
      }
      Literal::Character(c) => {
        '$'.hash(state);
        c.hash(state);
      }
      Literal::EscapedCharacter { value, .. } => {
        '$'.hash(state);
        value.hash(state);
      }
    }
  }

  fn hash_slice<H: Hasher>(data: &[Self], state: &mut H)
  where
    Self: Sized,
  {
    data.iter().for_each(|d| d.hash(state))
  }
}

impl Literal {
  /// Clones input and wraps with `Arc<>`
  pub(crate) fn new_string(s: &str) -> Literal {
    Literal::String(s.to_string().into())
  }

  /// Synthesizes a type of this literal
  pub(crate) fn synthesize_type(&self) -> ErlType {
    match self {
      Literal::Integer(_) => ErlTypeImpl::integer(),
      Literal::Float(_) => ErlTypeImpl::float(),
      Literal::Atom(_) => ErlTypeImpl::atom(),
      Literal::Bool(_) => ErlTypeImpl::boolean(),
      Literal::EmptyBinary => ErlTypeImpl::new_binary(None, None),
      // Cannot have runtime values as literals
      // ErlLit::Pid => ErlType::Pid,
      // ErlLit::Reference => ErlType::Reference,
      Literal::String(_) | Literal::Nil | Literal::List { .. } => {
        // List type is union of all element types
        // ErlType::List(ErlType::union_of_literal_types(elements)).into()
        ErlTypeImpl::any_list()
      }
      Literal::Tuple(items) => {
        let element_types = items.iter().map(|it| it.synthesize_type()).collect();
        ErlTypeImpl::Tuple { elements: element_types }.into()
      } // other => unimplemented!("Don't know how to synthesize type for {}", other),
      Literal::Character(_) => ErlTypeImpl::integer(),
      Literal::EscapedCharacter { .. } => ErlTypeImpl::integer(),
    }
  }

  // /// Subtype check for literal against builtin types
  // pub(crate) fn is_subtype_of(&self, super_ty: &ErlType) -> bool {
  //   match self {
  //     Literal::BigInteger
  //     | Literal::Integer(_) => super_ty.is_integer(),
  //     Literal::Float(_) => super_ty.is_float(),
  //     Literal::Atom(_)
  //     | Literal::Bool(_) => super_ty.is_atom(),
  //     Literal::Nil
  //     | Literal::String(_)
  //     | Literal::List { .. } => super_ty.is_list(),
  //     Literal::Tuple(_) => super_ty.is_tuple(),
  //   }
  // }

  /// Check for empty binary literal
  pub fn is_binary_lit(&self) -> bool {
    matches!(self, Literal::EmptyBinary)
  }
}

impl Eq for Literal {}

impl PartialEq for Literal {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      (Literal::Integer(a), Literal::Integer(b)) => a.eq(b),
      (Literal::Float(a), Literal::Float(b)) => (a - b).abs() <= f64::EPSILON,
      (Literal::Atom(a), Literal::Atom(b)) => a == b,
      (Literal::Bool(a), Literal::Bool(b)) => a == b,
      (Literal::List { elements: a, .. }, Literal::List { elements: b, .. }) => a == b,
      (Literal::String(a), Literal::String(b)) => a == b,
      (Literal::Tuple(a), Literal::Tuple(b)) => a == b,
      _ => false,
    }
  }
}

impl PartialOrd<Self> for Literal {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl Ord for Literal {
  fn cmp(&self, other: &Self) -> Ordering {
    let self_order = self.synthesize_type().get_order();
    let other_order = other.synthesize_type().get_order();
    let order = self_order.cmp(&other_order);
    match order {
      Ordering::Less | Ordering::Greater => order,
      Ordering::Equal => self.cmp_same_type(other),
    }
  }
}

impl Literal {
  /// Compares two literals of same kind, otherwise general ordering applies
  pub(crate) fn cmp_same_type(&self, other: &Literal) -> Ordering {
    match (self, other) {
      (Literal::Integer(a), Literal::Integer(b)) => a.partial_cmp(b).unwrap_or(Ordering::Equal),
      (Literal::Float(a), Literal::Float(b)) => {
        if (a - b).abs() <= f64::EPSILON {
          Ordering::Equal
        } else {
          a.partial_cmp(b).unwrap()
        }
      }
      (Literal::Atom(a), Literal::Atom(b)) => a.cmp(b),
      (Literal::Bool(a), Literal::Bool(b)) => a.cmp(b),
      (Literal::List { elements: a, .. }, Literal::List { elements: b, .. }) => a.cmp(b),
      (Literal::String(a), Literal::String(b)) => a.cmp(b),
      (Literal::Tuple(a), Literal::Tuple(b)) => a.cmp(b),
      _ => {
        unreachable!("Can't compare {} vs {}, only same type allowed in this function", self, other)
      }
    }
  }
}
