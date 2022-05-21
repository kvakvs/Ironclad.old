//! Contains all possible Erlang libironclad errors
use crate::source_loc::{SourceLoc};
use crate::mfarity::MFArity;
use crate::typing::type_error::TypeError;

pub mod err_fmt;
pub mod err_from;

/// Type of errors
pub enum ErlErrorType {
  /// Returned when multiple errors were found, report each error
  Interrupted(String),
  /// Returned when multiple errors were found, report each error
  Multiple(Vec<ErlError>),

  // Same as Multiple errors, but warnings
  // MultipleWarnings(Vec<ErlError>),
  /// Returned when file or directory read/write failed
  Io(std::io::Error),
  /// Project errors produced when glob() scanning input files and directories
  Glob(glob::GlobError),
  /// Returned when directory scan glob pattern contained an error
  GlobPattern(glob::PatternError),
  /// Project loading error produced when loading TOML
  Config(toml::de::Error),
  /// Returned when preprocessor parser failed
  PreprocessorParse,
  /// Returned when preprocessor syntax is not correct
  Preprocessor,
  /// Returned when Erlang parser failed: internal error must not occur with the user
  ParserInternal,
  /// Something unexpected like a TO-DO or assertion
  Internal,
  /// Returned when Erlang parser failed
  ErlangParse,
  /// A variable was referenced that's not in the scope
  VariableNotFound(String),
  /// A local function referenced by MFA (module ignored) is not found
  LocalFunctionNotFound(MFArity),
  /// Returned when a type error or mismatching types were found
  TypeErr(TypeError),
  /// Error raised when unsupported AST node occured where it shouldn't
  Unacceptable {
    /// Printable representation of the bad AST node
    ast_repr: String,
    /// Context to suggest the user where this was found
    context: String,
  },
}

/// Erlang libironclad errors all gathered together
pub struct ErlError {
  /// Error kind, an enum which might contain extra values
  err_type: ErlErrorType,
  /// Location where error was found
  loc: SourceLoc,
  /// Message from the libironclad
  msg: String,
}

impl ErlError {
  /// Create ErlError from 3 components
  pub fn new(err_type: ErlErrorType, loc: SourceLoc, msg: String) -> Self {
    ErlError {
      err_type,
      loc,
      msg,
    }
  }

  /// Create ErlError from type only
  pub fn new_type_only(err_type: ErlErrorType) -> Self {
    ErlError {
      err_type,
      loc: SourceLoc::None,
      msg: String::new(),
    }
  }

  /// Some errors might result in a non-0 exit code, list them here
  pub fn process_exit_code(&self) -> i32 { 1 }

  /// Create an internal error
  pub fn internal<T>(message: String) -> ErlResult<T> {
    Err(ErlError::new(ErlErrorType::Internal, SourceLoc::None, message))
  }

  /// Wraps a `VariableNotFound`
  pub fn variable_not_found<T>(var_name: &str, loc: SourceLoc) -> ErlResult<T> {
    let err_type = ErlErrorType::VariableNotFound(String::from(var_name));
    Err(ErlError::new(err_type, loc, "Variable not found".to_string()))
  }

  /// Wraps a `FunctionNotFound`
  pub fn local_function_not_found<T>(mfa: &MFArity) -> ErlResult<T> {
    let err_type = ErlErrorType::LocalFunctionNotFound(mfa.clone());
    Err(ErlError::new_type_only(err_type))
  }

  /// Creates an unacceptable AST error, this type of AST is not allowed here.
  pub fn unacceptable_ast<T>(ast_repr: String, context: String) -> ErlResult<T> {
    let err_type = ErlErrorType::Unacceptable { ast_repr, context };
    Err(ErlError::new_type_only(err_type))
  }

  /// Creates a preprocessor parse error from a filename and a message
  pub fn pp_parse<T>(loc: SourceLoc, message: &str) -> ErlResult<T> {
    Err(ErlError::new(ErlErrorType::PreprocessorParse, loc, String::from(message)))
  }

  /// Creates a preprocessor error from a filename and a message
  pub fn pp_error<T>(loc: SourceLoc, message: &str) -> ErlResult<T> {
    Err(ErlError::new(ErlErrorType::Preprocessor, loc, String::from(message)))
  }

  /// Create a parser internal error. Should not happen for the user, only during the development
  /// and testing.
  pub fn parser_internal(location: SourceLoc, msg: String) -> Self {
    ErlError::new(ErlErrorType::ParserInternal, location, msg)
  }

  /// Given a vector of ErlErrors, return one, multiple error, or panic if no errors were given
  pub fn multiple(mut errors: Vec<ErlError>) -> ErlError {
    match errors.len() {
      0 => panic!("ErlError::multiple() called with an empty error vector"),
      1 => errors.pop().unwrap(),
      _ => ErlError::new_type_only(ErlErrorType::Multiple(errors.into())),
    }
  }

  /// Given a vector of ErlErrors, return a warning wrap
  pub fn multiple_warnings(mut warnings: Vec<ErlError>) -> ErlError {
    match warnings.len() {
      0 => panic!("ErlError::multiple_warnings() called with an empty error vector"),
      1 => warnings.pop().unwrap(),
      _ => ErlError::new_type_only(ErlErrorType::Multiple(warnings.into())),
    }
  }
}

/// Used as Result<T> for all parse and compile operations
pub type ErlResult<T> = Result<T, ErlError>;
