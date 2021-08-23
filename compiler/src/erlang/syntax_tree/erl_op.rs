//! Binary and unary operations used in type checking.

/// Binary operation taking two arguments
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ErlBinaryOp {
  /// Joins two expressions together, right becomes the result
  Comma,
  /// Sum of two any numbers
  Add,
  /// Subtraction resut of two any numbers
  Sub,
  /// Product of two any numbers
  Mul,
  /// Float division result of two any numbers
  Div,
  /// Integer division result of two integer numbers
  IntegerDiv,
  /// Division remainder of two integer numbers
  Modulo,
  /// Left value is less than the right value
  Less,
  /// Left value is greater than the right value
  Greater,
  /// Left value is less or equal to the right value
  LessEq,
  /// Left value is greater or equal to the right value
  GreaterEq,
  /// Equality of two any values
  Eq,
  /// Inequality of two any values
  NotEq,
  /// Hard equality (identity) of two any values
  HardEq,
  /// Hard inequality (identity mismatch) of two any values
  HardNotEq,
  /// Concatenate two lists ++
  ListAppend,
  /// Difference of two lists --
  ListSubtract,
}

/// Unary operation takes 1 argument of bool or number, and returns same type
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ErlUnaryOp {
  /// Prefixed by 'catch' keyword
  Catch,
  /// Logical negation
  Not,
  /// Numerical sign change, -X
  Negative,
  /// Numerical sign positive, no sign change: +X
  Positive,
}
