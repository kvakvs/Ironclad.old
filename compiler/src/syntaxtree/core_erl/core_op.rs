//! Binary and unary operations used in type checking.

/// Binary operation taking two arguments
#[derive(Copy, Clone, Eq, PartialEq)]
pub enum CoreBinaryOp {
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
pub enum CoreUnaryOp {
  /// Prefixed by 'catch' keyword
  Catch,
  /// Logical negation
  Not,
  /// Numerical sign change, -X
  Negative,
  /// Numerical sign positive, no sign change: +X
  Positive,
}


impl std::fmt::Display for CoreBinaryOp {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      CoreBinaryOp::Add => write!(f, "+"),
      CoreBinaryOp::Sub => write!(f, "-"),
      CoreBinaryOp::Mul => write!(f, "*"),
      CoreBinaryOp::Div => write!(f, "/"),
      CoreBinaryOp::IntegerDiv => write!(f, "div"),
      CoreBinaryOp::Modulo => write!(f, "mod"),
      CoreBinaryOp::Less => write!(f, "<"),
      CoreBinaryOp::Greater => write!(f, ">"),
      CoreBinaryOp::LessEq => write!(f, "⩽"),
      CoreBinaryOp::GreaterEq => write!(f, "⩾"),
      CoreBinaryOp::Eq => write!(f, "≃"),
      CoreBinaryOp::NotEq => write!(f, "≄"),
      CoreBinaryOp::HardEq => write!(f, "≡"),
      CoreBinaryOp::HardNotEq => write!(f, "≢"),
      CoreBinaryOp::ListAppend => write!(f, "++"),
      CoreBinaryOp::ListSubtract => write!(f, "--"),
      CoreBinaryOp::Comma => write!(f, ","),
    }
  }
}

impl std::fmt::Display for CoreUnaryOp {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      CoreUnaryOp::Not => write!(f, "not"),
      CoreUnaryOp::Negative => write!(f, "-"),
      CoreUnaryOp::Positive => write!(f, "+"),
      CoreUnaryOp::Catch => write!(f, "catch"),
    }
  }
}
