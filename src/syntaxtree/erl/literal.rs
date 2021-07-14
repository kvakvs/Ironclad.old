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
}
