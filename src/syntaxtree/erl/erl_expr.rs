#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ErlExprOp {
  Lt,
  Gt,
  Leq,
  Geq,
  Eq,
  Neq,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ErlUnaryExprOp {
  Not,
  Negate,
}

/// An Erlang expression
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ErlExpr {
  Variable(String),
  Application,
  BinaryOp { left: Box<ErlExpr>, right: Box<ErlExpr>, op: ErlExprOp },
  UnaryOp { expr: Box<ErlExpr>, op: ErlUnaryExprOp },
}
