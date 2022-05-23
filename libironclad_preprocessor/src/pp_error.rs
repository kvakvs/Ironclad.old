use crate::nom_parser::PpParserError;
use ::function_name::named;
use libironclad_error::ic_error::IcResult;
use libironclad_error::ic_error_category::IcErrorCategory;
use libironclad_error::ic_error_trait::{IcError, IcErrorT};
use libironclad_error::source_loc::SourceLoc;
use std::fmt::{Debug, Display, Formatter};

#[derive(Debug)]
pub enum PpErrorCategory {
  /// Error while parsing Erlang syntax
  Parser,
  /// `#error` directive found
  ErrorDirective,
  /// `#warning` directive found
  WarningDirective,
}

/// Erlang libironclad errors all gathered together
#[derive(Debug)]
pub struct PpError {
  ic_category: IcErrorCategory,

  /// Error kind, an enum which might contain extra values
  #[allow(dead_code)]
  category: PpErrorCategory,

  /// Location where error was found
  loc: SourceLoc,

  /// Message from the libironclad
  msg: String,
}

impl Display for PpError {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{:?}", self)
  }
}

impl IcErrorT for PpError {
  fn get_category(&self) -> &IcErrorCategory {
    &self.ic_category
  }

  fn get_location(&self) -> &SourceLoc {
    &self.loc
  }

  fn get_process_exit_code(&self) -> i32 {
    1
  }

  fn get_message(&self) -> &str {
    &self.msg
  }
}

impl PpError {
  /// Create PpError from 3 components
  pub fn new(ic_cat: IcErrorCategory, cat: PpErrorCategory, loc: SourceLoc, msg: String) -> Self {
    Self {
      ic_category: ic_cat,
      category: cat,
      loc,
      msg,
    }
  }

  /// Builds PpError with nice error details from input string and Nom's verbose error
  #[named]
  pub fn from_nom_error<T>(input: &str, value: PpParserError) -> IcResult<T> {
    let new_err = Self {
      ic_category: IcErrorCategory::PreprocessorParse,
      category: PpErrorCategory::Parser,
      loc: SourceLoc::unimplemented(file!(), function_name!()),
      msg: nom::error::convert_error(input, value),
    };
    Err(Box::new(new_err))
  }

  /// Builds PpError for #error directive
  #[named]
  pub fn new_error_directive(msg: String) -> IcError {
    Box::new(PpError::new(
      IcErrorCategory::Preprocessor,
      PpErrorCategory::ErrorDirective,
      SourceLoc::unimplemented(file!(), function_name!()),
      msg,
    ))
  }

  // /// Creates an "Unacceptable" error
  // pub fn unacceptable<T>(loc: SourceLoc, message: String) -> IcResult<T> {
  //   let new_err = PpError::new(
  //     IcErrorCategory::ErlangParse, PpErrorCategory::Unacceptable, loc, message);
  //   Err(Box::new(new_err))
  // }
}
