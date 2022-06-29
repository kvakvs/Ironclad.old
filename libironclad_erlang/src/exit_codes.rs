//! Exit codes for `std::process::exit`

use crate::erl_syntax::erl_error::ErlError;
use crate::error::ic_error_trait::IcError;

/// Returned on success
pub const EXIT_SUCCESS: i32 = 0;
/// Returned if an error occured, making the run impossible.
pub const EXIT_FATAL: i32 = 1;
/// Returned if `module.errors` was not empty on exit.
pub const EXIT_ERRORS_FOUND: i32 = 2;

/// End program with the error message
pub fn erl_fatal_error(err: ErlError) {
  println!("Fatal error ({:?}/{:?}): {}", err.ic_category, err.category, err.msg);
  std::process::exit(EXIT_FATAL);
}

/// End program with the error message
pub fn erl_fatal_icerror(err: IcError) {
  println!("Fatal error ({:?}): {}", err.get_category(), err.get_message());
  std::process::exit(EXIT_FATAL);
}
