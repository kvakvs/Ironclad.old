//! Printing support for Preprocessor AST

use crate::preprocessor_syntax::pp_ast::PpAst;
use crate::preprocessor_syntax::pp_ast::PpAstType::{
  Define, EmptyText, Error, File, IfBlock, IfdefBlock, IncludedFile, Text, Undef, Warning,
};
use ::function_name::named;
use libironclad_util::pretty::Pretty;

impl std::fmt::Display for PpAst {
  /// Format AST as a string
  #[named]
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match &self.node_type {
      File(items) => {
        for i in items.iter() {
          writeln!(f, "{}", i)?;
        }
        Ok(())
      }

      Text(s) => write!(f, "{}", s),
      EmptyText => write!(f, "% empty text"),

      IncludedFile { ast: include_rc, .. } => write!(f, "{}", include_rc),
      Define { name, args, body } => {
        write!(f, "-define({}", name)?;
        if !args.is_empty() {
          Pretty::display_paren_list(args, f)?;
          if !body.is_empty() {
            write!(f, ", ")?;
          }
        }
        writeln!(f, "{}).", body)
      }
      IfdefBlock { macro_name, .. } => write!(f, "-ifdef({}).", macro_name),
      // PpAst::Ifndef { macro_name, .. } => write!(f, "-ifndef({}).", macro_name),
      IfBlock { cond, cond_true, cond_false } => {
        writeln!(f, "-if({}).", cond)?;
        for c in cond_true {
          writeln!(f, "{}", c)?;
        }
        for c in cond_false {
          writeln!(f, "{}", c)?;
        }
        writeln!(f, "-endif.")
      }
      Undef(name) => write!(f, "-undef({}).", name),
      Error(t) => write!(f, "-error({}).", t),
      Warning(t) => write!(f, "-warning({}).", t),

      _ => unreachable!("{}(): can't process {:?}", function_name!(), self),
    }
  }
}
