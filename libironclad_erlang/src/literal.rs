//! Erlang literals, values fully known at compile time
use std::cmp::Ordering;
use std::hash::{Hash, Hasher};

use crate::typing::erl_type::ErlType;
use std::sync::Arc;

/// An Erlang literal, a value fully known at compile time
#[derive(Clone, Debug)]
pub enum Literal {
  /// Small enough to fit into a machine word
  Integer(isize),
  /// TODO: Contains big integer
  BigInteger,

  /// A 8-byte wide float
  Float(f64),

  /// Atom literal, also includes atoms 'true' and 'false'
  Atom(String),
  // TODO: String/list lit, tuple lit, map lit, ironclad_exe lit, etc
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

  /// An empty list
  Nil,

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
      Literal::BigInteger => unimplemented!("Big integer hash"),
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

  fn hash_slice<H: Hasher>(data: &[Self], state: &mut H)
  where
    Self: Sized,
  {
    data.iter().for_each(|d| d.hash(state))
  }
}

impl Literal {
  /// Synthesizes a type of this literal
  pub fn synthesize_type(&self) -> Arc<ErlType> {
    match self {
      Literal::Integer(_) | Literal::BigInteger => ErlType::integer(),
      Literal::Float(_) => ErlType::float(),
      Literal::Atom(_) => ErlType::atom(),
      Literal::Bool(_) => ErlType::boolean(),
      // Cannot have runtime values as literals
      // ErlLit::Pid => ErlType::Pid,
      // ErlLit::Reference => ErlType::Reference,
      Literal::String(_) | Literal::Nil | Literal::List { .. } => {
        // List type is union of all element types
        // ErlType::List(ErlType::union_of_literal_types(elements)).into()
        ErlType::any_list()
      }
      Literal::Tuple(items) => {
        let element_types = items.iter().map(|it| it.synthesize_type()).collect();
        ErlType::Tuple { elements: element_types }.into()
      } // other => unimplemented!("Don't know how to synthesize type for {}", other),
    }
  }

  // /// Subtype check for literal against builtin types
  // pub fn is_subtype_of(&self, super_ty: &ErlType) -> bool {
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
}

impl Eq for Literal {}

impl PartialEq for Literal {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      (Literal::Integer(a), Literal::Integer(b)) => a == b,
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
  pub fn cmp_same_type(&self, other: &Literal) -> Ordering {
    match (self, other) {
      (Literal::Integer(a), Literal::Integer(b)) => a.cmp(b),
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
