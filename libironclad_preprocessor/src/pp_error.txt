//! Errors which can occur during preprocess stage
use crate::parsers::pp_parse_types::PpParserError;
use std::fmt::{Debug, Display, Formatter};

/// Category for preprocessor errors
#[derive(Debug)]
pub enum PpErrorCategory {
  /// Error while parsing Erlang syntax
  Parser,
  /// `#error` directive found
  ErrorDirective,
  /// `#warning` directive found
  WarningDirective,
  /// Errors produced by -if/-ifdef directives
  IfDirective,
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

  fn get_location(&self) -> SourceLoc {
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
    Self { ic_category: ic_cat, category: cat, loc, msg }
  }

  /// Builds PpError with nice error details from input string and Nom's verbose error
  pub fn from_nom_error<T>(input: ParserInput, value: PpParserError) -> IcResult<T> {
    let new_err = Self {
      ic_category: IcErrorCategory::PreprocessorParse,
      category: PpErrorCategory::Parser,
      loc: SourceLoc::from_input(input),
      msg: nom::error::convert_error(input, value),
    };
    Err(Box::new(new_err))
  }

  /// Builds PpError for #error directive
  pub fn new_error_directive(location: SourceLoc, msg: String) -> IcError {
    Box::new(PpError::new(
      IcErrorCategory::Preprocessor,
      PpErrorCategory::ErrorDirective,
      location.clone(),
      msg,
    ))
  }

  /// Builds PpError for an if-ifdef error situation
  pub fn new_if_directive_error<T>(loc: SourceLoc, msg: String) -> IcResult<T> {
    Err(Box::new(PpError::new(
      IcErrorCategory::Preprocessor,
      PpErrorCategory::IfDirective,
      loc.clone(),
      msg,
    )))
  }

  // /// Creates an "Unacceptable" error
  // pub fn unacceptable<T>(loc: SourceLoc, message: String) -> IcResult<T> {
  //   let new_err = PpError::new(
  //     IcErrorCategory::ErlangParse, PpErrorCategory::Unacceptable, loc, message);
  //   Err(Box::new(new_err))
  // }
}
