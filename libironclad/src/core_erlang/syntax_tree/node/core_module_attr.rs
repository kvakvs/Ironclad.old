//! Module attribute is a generic container for erlang syntax `-name(param, param, ...)`.
#![cfg(coreast)]

use libironclad_erlsyntax::literal::Literal;
use libironclad_util::pretty::Pretty;
use libironclad_util::source_loc::SourceLoc;
use std::fmt::Formatter;

/// Each module attribute is just a name and list of literals
#[derive(Debug)]
#[cfg(coreast)]
pub struct ModuleAttr {
  /// Source file pointer
  location: SourceLoc,
  /// Attribute name atom
  name: String,
  /// Attribute args, as parsed
  args: Vec<Literal>,
}

#[cfg(coreast)]
impl std::fmt::Display for ModuleAttr {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "'{}' = ", self.name)?;
    Pretty::display_square_list(&self.args, f)?;
    write!(f, ",")
  }
}
