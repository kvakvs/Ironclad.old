//! Enumeration of language structures for error reporting

use libironclad_util::pretty::Pretty;
use std::fmt::{Display, Formatter};

/// Format language structures for error reporting
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum LangConstruct {
  Variable,
  Literal,
  ParenthesizedExpression,
  AtomOf(&'static str),
  //------------------
  // Type constructs
  //------------------
  UserType,
  IntegerRangeType,
  ListType,
  TupleType,
  MapType,
  RecordRefType,
  IntegerLiteralType,
  AtomLiteralType,
  BinaryType,
}

impl Display for LangConstruct {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      LangConstruct::AtomOf(a) => write!(f, "'{}'", a),
      LangConstruct::Variable => write!(f, "variable"),
      LangConstruct::Literal => write!(f, "literal value"),
      LangConstruct::ParenthesizedExpression => write!(f, "parenthesized expression"),
      //------------------
      // Type constructs
      //------------------
      LangConstruct::UserType => write!(f, "user type with or without module name"),
      LangConstruct::IntegerRangeType => write!(f, "integer range type"),
      LangConstruct::ListType => write!(f, "list type"),
      LangConstruct::TupleType => write!(f, "tuple type"),
      LangConstruct::MapType => write!(f, "map type"),
      LangConstruct::RecordRefType => write!(f, "record reference type"),
      LangConstruct::IntegerLiteralType => write!(f, "integer literal type"),
      LangConstruct::AtomLiteralType => write!(f, "atom literal type"),
      LangConstruct::BinaryType => {
        write!(f, "binary type spec (head element, repeat element or both)")
      }
    }
  }
}

pub struct LangConstructs<'a>(pub &'a [LangConstruct]);

impl<'a> Display for LangConstructs<'a> {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    Pretty::display_comma_separated(self.0.iter(), f)
  }
}
