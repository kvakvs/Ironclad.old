//! Contains all possible Erlang libironclad errors
use crate::error::ic_error_category::IcErrorCategory;
use crate::error::ic_error_trait::{GenericIroncladError, IcErrorTrait};
use crate::source_loc::SourceLoc;
use std::path::{Path, PathBuf};

/// Ironclad errors all gathered together, and categorised
pub struct IroncladError {
  /// Error kind, an enum which might contain extra values
  category: IcErrorCategory,
  /// Location where error was found
  location: SourceLoc,
  /// Message for the user
  msg: String,
}

impl IcErrorTrait for IroncladError {
  fn get_category(&self) -> &IcErrorCategory {
    &self.category
  }

  fn get_location(&self) -> SourceLoc {
    self.location.clone()
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
  pub(crate) fn new(err_type: IcErrorCategory, loc: SourceLoc, msg: String) -> Self {
    IroncladError { category: err_type, location: loc, msg }
  }

  /// Create ErlError from type only
  pub(crate) fn new_type_only(err_type: IcErrorCategory) -> Self {
    Self {
      category: err_type,
      location: SourceLoc::None,
      msg: String::new(),
    }
  }

  /// Create an internal error
  #[allow(dead_code)]
  pub(crate) fn internal<T>(message: String) -> IroncladResult<T> {
    let new_err = IroncladError::new(IcErrorCategory::Internal, SourceLoc::None, message);
    Err(Box::new(new_err))
  }

  /// Wraps a `VariableNotFound`
  #[allow(dead_code)]
  pub(crate) fn variable_not_found<T>(var_name: &str, loc: SourceLoc) -> IroncladResult<T> {
    let cat = IcErrorCategory::VariableNotFound(String::from(var_name));
    let new_err = IroncladError::new(cat, loc, "Variable not found".to_string());
    Err(Box::new(new_err))
  }

  /// Wraps a `FileNotFound`
  #[allow(dead_code)]
  pub(crate) fn file_not_found<T>(
    location: SourceLoc,
    path: &Path,
    while_verb: &str,
  ) -> IroncladResult<T> {
    let cat = IcErrorCategory::FileNotFound {
      file: PathBuf::from(path),
      while_verb: while_verb.to_string(),
    };
    let new_err = IroncladError::new(cat, location, format!("While {}", while_verb));
    Err(Box::new(new_err))
  }

  // TODO: move to preprocessor crate
  /// Creates a preprocessor parse error from a filename and a message
  #[allow(dead_code)]
  pub(crate) fn pp_parse<T>(loc: SourceLoc, message: &str) -> IroncladResult<T> {
    let new_err =
      IroncladError::new(IcErrorCategory::PreprocessorParse, loc, String::from(message));
    Err(Box::new(new_err))
  }

  // TODO: move to preprocessor crate
  /// Creates a preprocessor error from a filename and a message
  #[allow(dead_code)]
  pub(crate) fn pp_error<T>(loc: SourceLoc, message: &str) -> IroncladResult<T> {
    let new_err = IroncladError::new(IcErrorCategory::Preprocessor, loc, String::from(message));
    Err(Box::new(new_err))
  }

  /// Create a parser internal error. Should not happen for the user, only during the development
  /// and testing.
  #[allow(dead_code)]
  pub(crate) fn parser_internal(location: SourceLoc, msg: String) -> Self {
    IroncladError::new(IcErrorCategory::ParserInternal, location, msg)
  }

  /// Given a vector of ErlErrors, return one, multiple error, or panic if no errors were given
  #[allow(dead_code)]
  pub(crate) fn multiple(mut errors: Vec<GenericIroncladError>) -> GenericIroncladError {
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
pub type IroncladResult<T> = Result<T, GenericIroncladError>;

// /// Used as Result<T> for non-compiler related operations (loading config, e.g.)
// pub type IroncladResult<T> = Result<T, IroncladError>;
