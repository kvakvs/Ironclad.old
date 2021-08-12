//! Defines AST tree for Core Erlang-like intermediate language. A more generalized and simplified
//! intermediate language, allowing easier optimisations and easier code generation.

use crate::mfarity::MFArity;
use crate::syntaxtree::core_erl::node::case::Case;
use crate::syntaxtree::core_erl::node::fn_def::FnDef;
use crate::syntaxtree::core_erl::node::let_expr::LetExpr;
use crate::syntaxtree::erl::node::literal::Literal;
use crate::typing::typevar::TypeVar;
use crate::syntaxtree::core_erl::node::prim_op::PrimOp;

/// Each module attribute is just a name and list of literals
pub struct ModuleAttr {
  name: String,
  args: Vec<Literal>,
}

/// AST node in Core Erlang (parsed or generated)
pub enum CoreAst {
  ModuleName(String),
  Exports(Vec<MFArity>),
  Attributes(Vec<ModuleAttr>),
  Var(String, TypeVar),
  FnDef(FnDef),
  Case(Case),
  Let(LetExpr),
  Apply(Apply),
  Call(Call),
  /// Primitive operation, such as `raise`
  PrimOp(PrimOp),
}
