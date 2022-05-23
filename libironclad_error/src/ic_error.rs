//! Contains all possible Erlang libironclad errors
use crate::ic_error_category::IcErrorCategory;
use crate::ic_error_trait::{IcError, IcErrorT};
use crate::source_loc::SourceLoc;

/// Ironclad errors all gathered together, and categorised
pub struct IroncladError {
  /// Error kind, an enum which might contain extra values
  category: IcErrorCategory,
  /// Location where error was found
  loc: SourceLoc,
  /// Message for the user
  msg: String,
}

impl IcErrorT for IroncladError {
  fn get_category(&self) -> &IcErrorCategory {
    &self.category
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

impl IroncladError {
  /// Create `IroncladError` from 3 components
  pub fn new(err_type: IcErrorCategory, loc: SourceLoc, msg: String) -> Self {
    IroncladError {
      category: err_type,
      loc,
      msg,
    }
  }

  /// Create ErlError from type only
  pub fn new_type_only(err_type: IcErrorCategory) -> Self {
    Self {
      category: err_type,
      loc: SourceLoc::None,
      msg: String::new(),
    }
  }

  /// Create an internal error
  pub fn internal<T>(message: String) -> IcResult<T> {
    let new_err = IroncladError::new(IcErrorCategory::Internal, SourceLoc::None, message);
    Err(Box::new(new_err))
  }

  /// Wraps a `VariableNotFound`
  pub fn variable_not_found<T>(var_name: &str, loc: SourceLoc) -> IcResult<T> {
    let cat = IcErrorCategory::VariableNotFound(String::from(var_name));
    let new_err = IroncladError::new(cat, loc, "Variable not found".to_string());
    Err(Box::new(new_err))
  }

  // TODO: move to preprocessor crate
  /// Creates a preprocessor parse error from a filename and a message
  pub fn pp_parse<T>(loc: SourceLoc, message: &str) -> IcResult<T> {
    let new_err = IroncladError::new(
      IcErrorCategory::PreprocessorParse,
      loc,
      String::from(message),
    );
    Err(Box::new(new_err))
  }

  // TODO: move to preprocessor crate
  /// Creates a preprocessor error from a filename and a message
  pub fn pp_error<T>(loc: SourceLoc, message: &str) -> IcResult<T> {
    let new_err = IroncladError::new(IcErrorCategory::Preprocessor, loc, String::from(message));
    Err(Box::new(new_err))
  }

  /// Create a parser internal error. Should not happen for the user, only during the development
  /// and testing.
  pub fn parser_internal(location: SourceLoc, msg: String) -> Self {
    IroncladError::new(IcErrorCategory::ParserInternal, location, msg)
  }

  /// Given a vector of ErlErrors, return one, multiple error, or panic if no errors were given
  pub fn multiple(mut errors: Vec<IcError>) -> IcError {
    match errors.len() {
      0 => panic!("IcError::multiple() called with an empty error vector"),
      1 => errors.pop().unwrap(),
      _ => {
        let new_err = IroncladError::new_type_only(IcErrorCategory::Multiple(errors));
        Box::new(new_err)
      }
    }
  }
}

/// Used as generic `Result<T>` which can hold any error
pub type IcResult<T> = Result<T, IcError>;

/// Used as Result<T> for non-compiler related operations (loading config, e.g.)
pub type IroncladResult<T> = Result<T, IroncladError>;
