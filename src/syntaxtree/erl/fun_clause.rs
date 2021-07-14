use crate::syntaxtree::erl::erl_ast::ErlAst;
use crate::typing::typevar::TypeVar;

#[derive(Debug, PartialEq)]
pub struct FunctionClause {
  args: Vec<ErlAst>,
  arg_types: Vec<TypeVar>,
  body: ErlAst,
}

impl FunctionClause {
  pub fn new(args: Vec<ErlAst>, expr: ErlAst) -> Self {
    let arg_types = args.iter().map(|_a| TypeVar::new()).collect();
    Self {
      args,
      arg_types,
      body: expr
    }
  }
}
