//! Contains all possible Erlang libironclad errors
use crate::error::ic_error_kind::IcErrorKind;
use crate::error::ic_error_trait::{GenericIroncladError, IcErrorTrait};
use crate::source_loc::SourceLoc;
use std::path::{Path, PathBuf};

/// Ironclad errors all gathered together, and categorised
pub struct IroncladError {
  /// The error severity
  pub severity: IcSeverity,
  /// Error kind, an enum which might contain extra values
  pub kind: IcErrorKind,
  /// Location where error was found
  pub location: SourceLoc,
  /// Message for the user
  pub msg: String,
}

/// How bad is the error
#[derive(Debug, Copy, Clone)]
pub enum IcSeverity {
  /// Nothing, just a notice for the user to read
  Notice,
  /// Warn after the processing
  Warning,
  /// Fail the processing but do as much as possible
  Error,
  /// Stop Ironclad immediately
  Fatal,
}

impl std::fmt::Display for IcSeverity {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      IcSeverity::Notice => write!(f, "Notice"),
      IcSeverity::Warning => write!(f, "Warning"),
      IcSeverity::Error => write!(f, "Error"),
      IcSeverity::Fatal => write!(f, "Fatal"),
    }
  }
}

impl IcErrorTrait for IroncladError {
  fn get_severity(&self) -> IcSeverity {
    self.severity
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
  pub(crate) fn new(
    severity: IcSeverity,
    err_type: IcErrorKind,
    loc: SourceLoc,
    msg: String,
  ) -> Self {
    IroncladError { severity, kind: err_type, location: loc, msg }
  }

  /// Create ErlError from type only
  pub(crate) fn new_type_only(severity: IcSeverity, kind: IcErrorKind) -> Self {
    Self {
      severity,
      kind,
      location: SourceLoc::None,
      msg: String::new(),
    }
  }

  /// Create an internal error
  #[allow(dead_code)]
  pub(crate) fn internal<T>(message: String) -> IroncladResult<T> {
    let new_err =
      IroncladError::new(IcSeverity::Error, IcErrorKind::Internal, SourceLoc::None, message);
    Err(Box::new(new_err))
  }

  /// Wraps a `VariableNotFound`
  #[allow(dead_code)]
  pub(crate) fn variable_not_found<T>(var_name: &str, loc: SourceLoc) -> IroncladResult<T> {
    let cat = IcErrorKind::VariableNotFound(String::from(var_name));
    let new_err = IroncladError::new(IcSeverity::Error, cat, loc, "Variable not found".to_string());
    Err(Box::new(new_err))
  }

  /// Wraps a `FileNotFound`
  #[allow(dead_code)]
  pub(crate) fn file_not_found<T>(
    location: SourceLoc,
    path: &Path,
    while_verb: &str,
  ) -> IroncladResult<T> {
    let cat = IcErrorKind::FileNotFound {
      file: PathBuf::from(path),
      while_verb: while_verb.to_string(),
    };
    let new_err =
      IroncladError::new(IcSeverity::Error, cat, location, format!("While {}", while_verb));
    Err(Box::new(new_err))
  }

  // // TODO: move to preprocessor crate
  // /// Creates a preprocessor parse error from a filename and a message
  // #[allow(dead_code)]
  // pub(crate) fn pp_parse<T>(loc: SourceLoc, message: &str) -> IroncladResult<T> {
  //   let new_err = IroncladError::new(
  //     IcSeverity::Error,
  //     IcErrorKind::PreprocessorParse,
  //     loc,
  //     String::from(message),
  //   );
  //   Err(Box::new(new_err))
  // }

  // // TODO: move to preprocessor crate
  // /// Creates a preprocessor error from a filename and a message
  // #[allow(dead_code)]
  // pub(crate) fn pp_error<T>(loc: SourceLoc, message: &str) -> IroncladResult<T> {
  //   let new_err =
  //     IroncladError::new(IcSeverity::Error, IcErrorKind::Preprocessor, loc, String::from(message));
  //   Err(Box::new(new_err))
  // }

  // /// Create a parser internal error. Should not happen for the user, only during the development
  // /// and testing.
  // #[allow(dead_code)]
  // pub(crate) fn parser_internal(location: SourceLoc, msg: String) -> Self {
  //   IroncladError::new(IcErrorKind::ParserInternal, location, msg)
  // }

  /// Given a vector of ErlErrors, return one, multiple error, or panic if no errors were given
  #[allow(dead_code)]
  pub(crate) fn multiple(mut errors: Vec<GenericIroncladError>) -> GenericIroncladError {
    match errors.len() {
      0 => panic!("IcError::multiple() called with an empty error vector"),
      1 => errors.pop().unwrap(),
      _ => {
        let new_err =
          IroncladError::new_type_only(IcSeverity::Error, IcErrorKind::Multiple(errors));
        Box::new(new_err)
      }
    }
  }
}

/// Used as generic `Result<T>` which can hold any error
pub type IroncladResult<T> = Result<T, GenericIroncladError>;

// /// Used as Result<T> for non-compiler related operations (loading config, e.g.)
// pub type IroncladResult<T> = Result<T, IroncladError>;
