use libironclad_error::ic_error::{IcResult, IroncladError};
use libironclad_error::ic_error_category::IcErrorCategory;
use libironclad_error::source_loc::SourceLoc;
use crate::syntax_tree::nom_parse::ErlParserError;

pub enum ErlErrorCategory {
  /// Error while parsing Erlang syntax
  Parser,
  /// Error raised when unsupported AST node occured where it shouldn't
  Unacceptable,
}

/// Erlang libironclad errors all gathered together
pub struct ErlError {
  /// Error kind, an enum which might contain extra values
  category: ErlErrorCategory,
  /// Location where error was found
  loc: SourceLoc,
  /// Message from the libironclad
  msg: String,
}

impl ErlError {
  /// Builds ErlError with nice error details from input string and Nom's verbose error
  pub fn from_nom_error(input: &str, value: ErlParserError) -> Self {
    Self {
      category: ErlErrorCategory::Parser,
      loc: SourceLoc::None,
      msg: nom::error::convert_error(input, value),
    }
  }

  /// Creates an "Unacceptable" error
  pub fn unacceptable<T>(loc: SourceLoc, message: String) -> IcResult<T> {
    let new_err = ErlError::new(ErlErrorCategory::Unacceptable, loc, message);
    Err(Box::new(new_err))
  }
}
