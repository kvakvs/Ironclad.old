#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PpExprOp {
  Lt,
  Gt,
  Leq,
  Geq,
  Eq,
  Neq,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PpUnaryExprOp {
  Not,
  Negate,
}

/// Boolean expression for IF and ELIF, used inside PpAst::If/Elif
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PpExpr {
  Variable(String),
  BinaryOp { left: Box<PpExpr>, right: Box<PpExpr>, op: PpExprOp },
  UnaryOp { expr: Box<PpExpr>, op: PpUnaryExprOp },
}
