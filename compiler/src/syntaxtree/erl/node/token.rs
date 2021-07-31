//! Declares temporary tokens for the AST

/// Temporary token marking tokens of interest while parsing the AST tree. Must not be present in
/// the final AST produced by the parser.
#[allow(missing_docs)]
#[derive(Eq, PartialEq)]
pub enum ErlToken {
  Comma,
  Plus,
  Minus,
  Div,
  Mul,
  IntegerDiv,
  Remainder,
  Not,
  Or,
  Xor,
  And,
  BinaryNot,
  BinaryAnd,
  BinaryOr,
  BinaryXor,
  BinaryShiftLeft,
  BinaryShiftRight,
  ListAppend,
  ListSubtract,
  Eq,
  NotEq,
  LessThanEq,
  LessThan,
  GreaterEq,
  GreaterThan,
  HardEq,
  HardNotEq,
  AndAlso,
  OrElse,
  Assign,
  Send,
  Catch,
}
