//! Printing support for Preprocessor AST

use ::function_name::named;

use crate::display::Pretty;
use crate::preprocessor::syntax_tree::pp_ast::PpAst;

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

      PpAst::IncludedFile { nested: include_rc, .. } => write!(f, "{}", include_rc),
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
      PpAst::Ifdef(name) => write!(f, "-ifdef({}).", name),
      PpAst::Ifndef(name) => write!(f, "-ifndef({}).", name),
      PpAst::Else => write!(f, "-else."),
      PpAst::Endif => write!(f, "-endif."),
      PpAst::If(expr) => write!(f, "-if({}).", expr),
      PpAst::Elif(expr) => write!(f, "-elif({}).", expr),
      PpAst::Undef(name) => write!(f, "-undef({}).", name),
      PpAst::Error(t) => write!(f, "-error({}).", t),
      PpAst::Warning(t) => write!(f, "-warning({}).", t),
      PpAst::Comment(t) => write!(f, "% {}", t),

      _ => unreachable!("{}(): can't process {:?}", function_name!(), self),
    }
  }
}
