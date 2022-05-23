use std::fmt::{Debug};
use libironclad_error::ic_error::{IcResult};
use libironclad_error::ic_error_category::IcErrorCategory;
use libironclad_error::ic_error_trait::IcErrorT;
use libironclad_error::source_loc::SourceLoc;
use libironclad_util::mfarity::MFArity;
use crate::syntax_tree::nom_parse::ErlParserError;
use crate::typing::type_error::TypeError;

#[derive(Debug)]
pub enum ErlErrorCategory {
  /// Error while parsing Erlang syntax
  Parser,
  /// Error raised when unsupported AST node occured where it shouldn't
  Unacceptable,
  /// Type discrepancy found
  TypeError,
  /// Local function not found
  LocalFnNotFound { mfa: MFArity },
  /// Variable not found
  VariableNotFound { var: String },
}

/// Erlang libironclad errors all gathered together
#[derive(Debug)]
pub struct ErlError {
  pub ic_category: IcErrorCategory,
  /// Error kind, an enum which might contain extra values
  pub category: ErlErrorCategory,
  /// Location where error was found
  pub loc: SourceLoc,
  /// Message from the libironclad
  pub msg: String,
}

impl IcErrorT for ErlError {
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

impl ErlError {
  /// Create ErlError from 3 components
  pub fn new(ic_cat: IcErrorCategory,
             cat: ErlErrorCategory,
             loc: SourceLoc,
             msg: String) -> Self {
    Self {
      ic_category: ic_cat,
      category: cat,
      loc,
      msg,
    }
  }

  /// Builds ErlError with nice error details from input string and Nom's verbose error
  pub fn from_nom_error<T>(input: &str, value: ErlParserError) -> IcResult<T> {
    let new_err = Self {
      ic_category: IcErrorCategory::ErlangParse,
      category: ErlErrorCategory::Parser,
      loc: SourceLoc::None,
      msg: nom::error::convert_error(input, value),
    };
    Err(Box::new(new_err))
  }

  /// Creates an "Unacceptable" error
  pub fn unacceptable<T>(loc: SourceLoc, message: String) -> IcResult<T> {
    let new_err = ErlError::new(
      IcErrorCategory::ErlangParse, ErlErrorCategory::Unacceptable, loc, message);
    Err(Box::new(new_err))
  }

  /// Creates an "TypeError" error
  pub fn type_error<T>(loc: SourceLoc, type_err: TypeError) -> IcResult<T> {
    let new_err = ErlError::new(
      IcErrorCategory::TypeError, ErlErrorCategory::TypeError, loc,
      format!("{}", type_err));
    Err(Box::new(new_err))
  }

  /// Creates an "Local Function Not Found" error
  pub fn local_function_not_found<T>(loc: SourceLoc, mfa: MFArity, msg: String) -> IcResult<T> {
    let new_err = ErlError::new(
      IcErrorCategory::Erlang, ErlErrorCategory::LocalFnNotFound { mfa }, loc, msg);
    Err(Box::new(new_err))
  }

  /// Creates a "Variable Not Found" error
  pub fn variable_not_found<T>(loc: SourceLoc, var: String) -> IcResult<T> {
    let new_err = ErlError::new(
      IcErrorCategory::Erlang, ErlErrorCategory::VariableNotFound { var }, loc,
      String::default());
    Err(Box::new(new_err))
  }
}
