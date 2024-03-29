//! Erlang errors
use crate::error::ic_error::{IcSeverity, IroncladResult};
use crate::error::ic_error_trait::IcErrorTrait;
use crate::source_loc::SourceLoc;
use libironclad_util::mfarity::MFArity;
use std::fmt::Debug;

/// Category of Erlang Errors
#[derive(Debug)]
pub enum ErlErrorKind {
  /// Error is related to preprocessor directives or created by their interpretation
  PreprocessorError,
  /// Type discrepancy found
  TypeError,
  /// Local function not found
  LocalFnNotFound {
    /// Local function reference
    mfa: MFArity,
  },
  /// Variable not found
  VariableNotFound {
    /// Variable which is not found
    var: String,
  },
}

/// Erlang libironclad errors all gathered together
#[derive(Debug)]
pub struct ErlError {
  /// The error severity
  pub severity: IcSeverity,
  /// Error kind, also contains details about error, useful for later analysis or printing
  pub kind: ErlErrorKind,
  /// Location where error was found
  pub loc: SourceLoc,
  /// Message from the libironclad
  pub msg: String,
}

impl IcErrorTrait for ErlError {
  fn get_severity(&self) -> IcSeverity {
    self.severity
  }

  fn get_location(&self) -> SourceLoc {
    self.loc.clone()
  }

  fn get_process_exit_code(&self) -> i32 {
    crate::exit_codes::EXIT_ERRORS_FOUND
  }

  fn get_message(&self) -> &str {
    &self.msg
  }
}

impl ErlError {
  /// Create ErlError from 3 components
  pub(crate) fn new(severity: IcSeverity, kind: ErlErrorKind, loc: SourceLoc, msg: String) -> Self {
    Self { severity, kind, loc, msg }
  }

  // /// Builds ErlError with nice error details from input string and Nom's verbose error
  // #[allow(dead_code)]
  // pub(crate) fn from_nom_error<T>(input: ParserInput, value: ErlParserError) -> IcResult<T> {
  //   let new_err = Self {
  //     ic_category: IcErrorCategory::ErlangParse,
  //     category: ErlErrorCategory::Parser,
  //     loc: SourceLoc::None,
  //     msg: nom::error::convert_error(input, value),
  //   };
  //   Err(Box::new(new_err))
  // }

  // /// Creates an "Unacceptable" error
  // pub(crate) fn unacceptable<T>(loc: SourceLoc, message: String) -> IroncladResult<T> {
  //   Err(IcParserError::new(loc, message))
  // }

  // /// Creates an "TypeError" error
  // pub(crate) fn type_error<T>(loc: SourceLoc, type_err: TypeError) -> IcResult<T> {
  //   let new_err = ErlError::new(
  //     IcErrorCategory::TypeErr,
  //     ErlErrorCategory::TypeError,
  //     loc,
  //     format!("{}", type_err),
  //   );
  //   Err(Box::new(new_err))
  // }

  /// Creates an "Local Function Not Found" error
  pub(crate) fn local_function_not_found<T>(
    loc: SourceLoc,
    mfa: MFArity,
    msg: String,
  ) -> IroncladResult<T> {
    let new_err = ErlError::new(IcSeverity::Error, ErlErrorKind::LocalFnNotFound { mfa }, loc, msg);
    Err(Box::new(new_err))
  }

  // /// Creates an "preprocessor" error, even though there isn't preprocessor and we do preprocessor
  // /// directives inline with the other bits of Erlang source.
  // #[inline]
  // pub(crate) fn preprocessor_error(loc: SourceLoc, msg: String) -> Self {
  //   ErlError::new(IcErrorKind::Erlang, ErlErrorKind::PreprocessorError, loc, msg)
  // }

  // #[allow(dead_code)]
  // #[inline]
  // pub(crate) fn return_preprocessor_error<T>(loc: SourceLoc, msg: String) -> IroncladResult<T> {
  //   Err(IcPreprocessorError::new(loc, msg))
  // }

  /// Creates a "Variable Not Found" error
  pub(crate) fn variable_not_found<T>(loc: SourceLoc, var: String) -> IroncladResult<T> {
    let new_err = ErlError::new(
      IcSeverity::Error,
      ErlErrorKind::VariableNotFound { var },
      loc,
      String::default(),
    );
    Err(Box::new(new_err))
  }
}
