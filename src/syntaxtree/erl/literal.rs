use crate::typing::erl_type::ErlType;
use lazy_static::lazy_static;

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
}

lazy_static! {
static ref ERLTYPE_Integer: ErlType = ErlType::Integer;
static ref ERLTYPE_Float: ErlType = ErlType::Float;
static ref ERLTYPE_Atom: ErlType = ErlType::Atom;
static ref ERLTYPE_Bool: ErlType = ErlType::Bool;
static ref ERLTYPE_Pid: ErlType = ErlType::Pid;
static ref ERLTYPE_Reference: ErlType = ErlType::Reference;
}

impl ErlLiteral {
  pub fn to_string(&self) -> String {
    match self {
      ErlLiteral::Integer(n) => format!("{}", n),
      ErlLiteral::Float(f) => format!("{}", f),
      ErlLiteral::Atom(a) => format!("'{}'", a),
      ErlLiteral::Bool(b) => format!("{}", if *b { "'true'" } else { "'false'" }),
      ErlLiteral::Pid => format!("<pid>"),
      ErlLiteral::Reference => format!("<ref>"),
    }
  }

  pub fn get_type(&self) -> &ErlType {
    match self {
      ErlLiteral::Integer(_) => &ERLTYPE_Integer,
      ErlLiteral::Float(_) => &ERLTYPE_Float,
      ErlLiteral::Atom(_) => &ERLTYPE_Atom,
      ErlLiteral::Bool(_) => &ERLTYPE_Bool,
      ErlLiteral::Pid => &ERLTYPE_Pid,
      ErlLiteral::Reference => &ERLTYPE_Reference,
    }
  }
}
