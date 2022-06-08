//! Printing preprocessor AST nodes

use crate::erl_syntax::preprocessor::ast::PreprocessorNodeType;
use crate::erl_syntax::preprocessor::ast::PreprocessorNodeType::{
  Define, IfBlock, IfdefBlock, Include, IncludeLib, IncludedFile,
};
use ::function_name::named;
use libironclad_util::pretty::Pretty;

impl std::fmt::Display for PreprocessorNodeType {
  /// Format Preprocessor AST as a string
  #[named]
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match &self {
      Include(p) => writeln!(f, "-include(\"{}\").", p),
      IncludeLib(p) => write!(f, "-include_lib(\"{}\").", p),
      IncludedFile { ast: include_rc, .. } => write!(f, "{}", include_rc),
      Define { name, args, body } => {
        write!(f, "-define({}", name)?;

        if !args.is_empty() {
          Pretty::display_paren_list(args, f)?;
        }
        if !body.is_empty() {
          write!(f, ", {}).", body)?;
        }
        Ok(())
      }
      IfdefBlock { macro_name, .. } => write!(f, "-ifdef({}).", macro_name),
      // PpAst::Ifndef { macro_name, .. } => write!(f, "-ifndef({}).", macro_name),
      IfBlock { cond, cond_true, cond_false } => {
        writeln!(f, "-if({}).", cond)?;
        for c in cond_true {
          writeln!(f, "{}", c)?;
        }
        if !cond_false.is_empty() {
          writeln!(f, "-else.")?;
        }
        for c in cond_false {
          writeln!(f, "{}", c)?;
        }
        writeln!(f, "-endif.")
      }
      PreprocessorNodeType::Undef(name) => write!(f, "-undef({}).", name),
      PreprocessorNodeType::Error(t) => {
        write!(f, "-error(")?;
        Pretty::doublequot_string(f, t)?;
        write!(f, ").")
      }
      PreprocessorNodeType::Warning(t) => {
        write!(f, "-warning(")?;
        Pretty::doublequot_string(f, t)?;
        write!(f, ").")
      }

      _ => unreachable!("{}(): can't process {:?}", function_name!(), self),
    }
  }
}
