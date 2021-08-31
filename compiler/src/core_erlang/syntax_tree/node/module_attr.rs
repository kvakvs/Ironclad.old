//! Module attribute is a generic container for erlang syntax `-name(param, param, ...)`.
use crate::source_loc::SourceLoc;
use crate::literal::Literal;
use std::fmt::Formatter;
use crate::display;

/// Each module attribute is just a name and list of literals
#[derive(Debug)]
pub struct ModuleAttr {
  /// Source file pointer
  location: SourceLoc,
  /// Attribute name atom
  name: String,
  /// Attribute args, as parsed
  args: Vec<Literal>,
}

impl std::fmt::Display for ModuleAttr {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "'{}' = ", self.name)?;
    display::display_square_list(&self.args, f)?;
    write!(f, ",")
  }
}