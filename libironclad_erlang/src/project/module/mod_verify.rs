//! Checks after parsing, to see that fn specs, exports, etc are not orphaned

use crate::error::ic_error::IcResult;
use crate::project::module::mod_impl::{ErlModule, ErlModuleImpl};

impl ErlModuleImpl {
  /// Check that specs and exports match the fn defs
  pub fn verify_integrity(_module: &ErlModule) -> IcResult<()> {
    // TODO: Check that func specs match func defs
    // TODO: Check that exports all exist as funs
    Ok(())
  }
}
