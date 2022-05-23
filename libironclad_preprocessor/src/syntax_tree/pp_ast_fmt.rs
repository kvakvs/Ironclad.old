//! Printing support for Preprocessor AST

use ::function_name::named;
use libironclad_util::pretty::Pretty;
use crate::syntax_tree::pp_ast::PpAst;

impl std::fmt::Display for PpAst {
  /// Format AST as a string
  #[named]
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      PpAst::File(items) => {
        for i in items.iter() {
          writeln!(f, "{}", i)?;
        }
        Ok(())
      }

      PpAst::Text(s) => write!(f, "text ⊏{}⊐", s),
      PpAst::EmptyText => write!(f, "text ∅"),

      PpAst::IncludedFile { ast: include_rc, .. } => write!(f, "{}", include_rc),
      PpAst::Define { name, args, body } => {
        write!(f, "-define({}", name)?;
        if let Some(args1) = args {
          Pretty::display_paren_list(args1, f)?;
          if body.is_some() {
            write!(f, ", ")?;
          }
        }
        if let Some(body1) = body {
          write!(f, "{}", body1)?;
        }
        writeln!(f, ").")
      }
      PpAst::DefineFun { name, args, body } => {
        write!(f, "-define({}({:?}), {})", name, args, body)
      }
      PpAst::IfdefBlock { macro_name, .. } => write!(f, "-ifdef({}).", macro_name),
      // PpAst::Ifndef { macro_name, .. } => write!(f, "-ifndef({}).", macro_name),
      PpAst::IfBlock { cond, cond_true, cond_false } => {
        write!(f, "-if({}).\n", cond)?;
        if let Some(branch_true) = cond_true {
          for c in branch_true { writeln!(f, "{}", c) ?; }
        }
        if let Some(branch_false) = cond_false {
          for c in branch_false { writeln!(f, "{}", c) ?; }
        }
        write!(f, "-endif.\n")
      }
      PpAst::Undef(name) => write!(f, "-undef({}).", name),
      PpAst::Error(t) => write!(f, "-error({}).", t),
      PpAst::Warning(t) => write!(f, "-warning({}).", t),

      _ => unreachable!("{}(): can't process {:?}", function_name!(), self),
    }
  }
}
