//! Printing for ErlError's

use crate::ic_error::{IroncladError};
use crate::ic_error_category::IcErrorCategory;
use crate::ic_error_trait::IcErrorT;

impl std::fmt::Debug for IroncladError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { write!(f, "{}", self) }
}

impl std::fmt::Display for IroncladError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self.get_category() {
      // IcErrorType::Interrupted(s) => write!(f, "Processing interrupted: {}", s),
      IcErrorCategory::Multiple(errs) => {
        writeln!(f, "Multiple errors:")?;
        for err in errs.iter() {
          writeln!(f, "{}", err)?;
        }
        Ok(())
      }
      IcErrorCategory::Io(ioerr) => write!(f, "File IO error: {}", ioerr),
      IcErrorCategory::Glob(gerr) => write!(f, "Directory scan error: {}", gerr),
      IcErrorCategory::GlobPattern(gperr) => write!(f, "Glob pattern error: {}", gperr),
      IcErrorCategory::Config(cfgerr) => write!(f, "Configuration file syntax error: {}", cfgerr),
      IcErrorCategory::Preprocessor => {
        write!(f, "Preprocessor error: {} (at {})", self.get_message(), self.get_location())
      }
      IcErrorCategory::PreprocessorParse => {
        write!(f, "Preprocessor parse error: {} (at {})", self.get_message(), self.get_location())
      }
      IcErrorCategory::ParserInternal => {
        write!(f, "Parser internal error: {} (at {})", self.get_message(), self.get_location())
      }
      IcErrorCategory::Internal => {
        write!(f, "Internal error: {} (at {})", self.get_message(), self.get_location())
      }
      IcErrorCategory::ErlangParse => {
        write!(f, "Erlang parse error: {} (at {})", self.get_message(), self.get_location())
      }
      IcErrorCategory::VariableNotFound(vname) => write!(f, "Variable not found: {}", vname),
      // IcErrorCategory::LocalFunctionNotFound(mfa) => write!(f, "Local function not found: {}", mfa),
      // IcErrorCategory::TypeErr(terr) => write!(f, "Type error: {}", terr),
    }
  }
}
