//! Type errors returned by the typing engine

use crate::error::ic_error::IcSeverity;
use crate::error::ic_error_trait::{GenericIroncladError, IcErrorTrait};
use crate::source_loc::SourceLoc;
use libironclad_util::mfarity::MFArity;
use std::fmt::{Display, Formatter};

/// Indicates various type problems
#[derive(Debug)]
pub enum TypeErrorKind {
  /// Synthesized type for an expression isn't a subtype of the given type
  ExpectedType {
    /// Type which is expected
    expected_type: String,
    /// Type to check: type synthesized from an expression
    actual_type: String,
  },
  /// List operation received something that's not a list
  ListExpected,
  /// Returned when a call is attempted to something that's not a function
  NotAFunction {
    /// If MFA is known, then its stored here.
    mfa: Option<MFArity>,
  },
  /// Returned when function is not in function scope
  FunctionNotFound {
    /// The MFA that's not found
    mfa: MFArity,
  },
  /// A function call was attempted with wrong argument count
  BadArity,
  /// A function call was attempted with incompatible arguments
  BadArguments,
  /// Something wrong with type specs
  TypeSpecError,
}

/// Wraps a message with error kind together
#[derive(Debug)]
pub struct TypeError {
  /// The error severity
  pub severity: IcSeverity,
  /// The error category with extra details
  kind: TypeErrorKind,
  /// Source location where the error has occurred
  location: Option<SourceLoc>,
  /// The error message
  message: String,
}

impl IcErrorTrait for TypeError {
  fn get_severity(&self) -> IcSeverity {
    self.severity
  }

  fn get_location(&self) -> SourceLoc {
    self.location.clone().unwrap_or(SourceLoc::None)
  }

  fn get_process_exit_code(&self) -> i32 {
    todo!()
  }

  fn get_message(&self) -> &str {
    todo!()
  }
}

impl TypeError {
  /// Create a typespec error with a message
  pub fn new_spec_error(location: Option<SourceLoc>, msg: String) -> GenericIroncladError {
    Box::new(Self {
      severity: IcSeverity::Error,
      kind: TypeErrorKind::TypeSpecError,
      location,
      message: msg,
    })
  }

  /// Create a new `function not found` error.
  pub fn new_fn_not_found(location: Option<SourceLoc>, mfa: MFArity) -> GenericIroncladError {
    Box::new(Self {
      severity: IcSeverity::Error,
      kind: TypeErrorKind::FunctionNotFound { mfa: mfa.clone() },
      location,
      message: format!("Function not found: {}", mfa),
    })
  }

  /// Create a new `not a function` error.
  pub fn new_not_a_fn(
    location: Option<SourceLoc>,
    mfa: Option<MFArity>,
    message: String,
  ) -> GenericIroncladError {
    Box::new(Self {
      severity: IcSeverity::Error,
      kind: TypeErrorKind::NotAFunction { mfa },
      location,
      message,
    })
  }

  /// Create a new `bad arity` error.
  pub fn new_bad_arity(location: Option<SourceLoc>, message: String) -> GenericIroncladError {
    Box::new(Self {
      severity: IcSeverity::Error,
      kind: TypeErrorKind::BadArity,
      location,
      message,
    })
  }

  /// Create a new `list expected` error.
  pub fn new_list_expected(location: Option<SourceLoc>, message: String) -> GenericIroncladError {
    Box::new(Self {
      severity: IcSeverity::Error,
      kind: TypeErrorKind::ListExpected,
      location,
      message,
    })
  }

  /// Create a new `bad arguments` error.
  pub fn new_bad_arguments(location: Option<SourceLoc>, message: String) -> GenericIroncladError {
    Box::new(Self {
      severity: IcSeverity::Error,
      kind: TypeErrorKind::BadArguments,
      location,
      message,
    })
  }

  /// Create a new `different type expected` error.
  pub fn new_type_error(
    location: Option<SourceLoc>,
    expected: String,
    received: String,
    message: String,
  ) -> GenericIroncladError {
    Box::new(Self {
      severity: IcSeverity::Error,
      kind: TypeErrorKind::ExpectedType { expected_type: expected, actual_type: received },
      location,
      message,
    })
  }
}

impl Display for TypeError {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match &self.kind {
      TypeErrorKind::ExpectedType { expected_type, actual_type } => {
        write!(f, "Expression's type: {} but expected: {}", actual_type, expected_type)
      }
      TypeErrorKind::ListExpected => write!(f, "Bad list: {}", self.message),
      TypeErrorKind::NotAFunction { mfa } => {
        if let Some(some_mfa) = mfa {
          write!(f, "Bad fun: {}", some_mfa)
        } else {
          write!(f, "Bad fun")
        }
      }
      TypeErrorKind::BadArity => write!(f, "Bad arity: {}", self.message),
      TypeErrorKind::BadArguments => write!(f, "Bad arguments: {}", self.message),
      TypeErrorKind::FunctionNotFound { mfa } => write!(f, "Function not found: {}", mfa),
      TypeErrorKind::TypeSpecError => write!(f, "Type spec error: {}", self.message),
    }
  }
}
