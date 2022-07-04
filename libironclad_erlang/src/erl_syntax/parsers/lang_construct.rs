//! Enumeration of language structures for error reporting

use libironclad_util::pretty::Pretty;
use std::fmt::{Display, Formatter};

/// Format language structures for error reporting
#[allow(missing_docs)]
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum LangConstruct {
  Variable,
  Literal,
  ParenthesizedExpression,
  AtomOf(&'static str),
  Lambda,
  BeginEnd,
  TryCatch,
  IfExpression,
  CaseExpression,
  List,
  Tuple,
  FunctionReference,
  Map,
  Record,
  ListComprehension,
  BinaryComprehension,
  Binary,
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
      LangConstruct::Lambda => write!(f, "a lambda expression"),
      LangConstruct::BeginEnd => write!(f, "begin-end expression"),
      LangConstruct::TryCatch => write!(f, "try-catch expression"),
      LangConstruct::IfExpression => write!(f, "if expression"),
      LangConstruct::CaseExpression => write!(f, "case expression"),
      LangConstruct::List => write!(f, "list"),
      LangConstruct::Tuple => write!(f, "tuple"),
      LangConstruct::FunctionReference => write!(f, "function reference"),
      LangConstruct::Map => write!(f, "map"),
      LangConstruct::Record => write!(f, "record"),
      LangConstruct::ListComprehension => write!(f, "list comprehension"),
      LangConstruct::BinaryComprehension => write!(f, "binary comprehension"),
      LangConstruct::Binary => write!(f, "binary"),
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
