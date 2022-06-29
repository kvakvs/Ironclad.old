//! Exit codes for `std::process::exit`

/// Returned on success
pub const EXIT_SUCCESS: i32 = 0;
/// Returned if an error occured, making the run impossible.
pub const EXIT_FATAL: i32 = 1;
/// Returned if `module.errors` was not empty on exit.
pub const EXIT_ERRORS_FOUND: i32 = 2;
