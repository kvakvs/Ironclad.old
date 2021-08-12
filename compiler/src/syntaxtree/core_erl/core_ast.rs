//! Defines AST tree for Core Erlang-like intermediate language. A more generalized and simplified
//! intermediate language, allowing easier optimisations and easier code generation.

use crate::mfarity::MFArity;
use crate::syntaxtree::core_erl::node::case::Case;
use crate::syntaxtree::core_erl::node::fn_def::FnDef;
use crate::syntaxtree::core_erl::node::let_expr::LetExpr;
use crate::syntaxtree::core_erl::node::apply::Apply;
use crate::syntaxtree::core_erl::node::call::Call;
use crate::syntaxtree::core_erl::node::var::Var;
use crate::syntaxtree::erl::node::literal::Literal;
use crate::syntaxtree::core_erl::node::prim_op::PrimOp;

/// Each module attribute is just a name and list of literals
pub struct ModuleAttr {
  name: String,
  args: Vec<Literal>,
}

/// AST node in Core Erlang (parsed or generated)
pub enum CoreAst {
  /// Module header with the name and a collection of exported functions
  Module {
    /// Module name atom
    name: String,
    /// List of fun/arity (module always `None`)
    exports: Vec<MFArity>,
  },
  /// Module attributes collection, grouped all together
  Attributes(Vec<ModuleAttr>),

  //
  // Execution and branching AST nodes
  //

  /// Define a function with 1 clause (multiple clauses handled by a `case Args of`)
  FnDef(FnDef),
  /// Branch based on whether an expression matches some conditions
  Case(Case),
  /// Create a scope by assigning 1 or more variables
  Let(LetExpr),
  /// Call an expression which has a function type
  Apply(Apply),
  /// Call a function export mod:fun/arity, or a local fun/arity
  Call(Call),
  /// Primitive operation, such as `raise`
  PrimOp(PrimOp),

  //
  // AST nodes carrying values and operations
  //

  /// A variable with optional name and assigned unique numbered typevar
  Var(Var),
  /// A literal value (fully known at compile time)
  Lit(Literal),
}
