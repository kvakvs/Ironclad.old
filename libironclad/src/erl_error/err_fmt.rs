//! Printing for ErlError's

use crate::erl_error::{ErlError, ErlErrorType};

impl std::fmt::Debug for ErlError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { write!(f, "{}", self) }
}

impl std::fmt::Display for ErlError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match &self.err_type {
      ErlErrorType::Interrupted(s) => write!(f, "Processing interrupted: {}", s),
      ErlErrorType::Multiple(errs) => {
        writeln!(f, "Multiple errors:")?;
        for err in errs.iter() {
          writeln!(f, "{}", err)?;
        }
        Ok(())
      }
      ErlErrorType::Io(ioerr) => write!(f, "File IO error: {}", ioerr),
      ErlErrorType::Glob(gerr) => write!(f, "Directory scan error: {}", gerr),
      ErlErrorType::GlobPattern(gperr) => write!(f, "Glob pattern error: {}", gperr),
      ErlErrorType::Config(cfgerr) => write!(f, "Configuration file syntax error: {}", cfgerr),
      ErlErrorType::Preprocessor => {
        write!(f, "Preprocessor error: {} (at {})", self.msg, self.loc)
      }
      ErlErrorType::PreprocessorParse => {
        write!(f, "Preprocessor parse error: {} (at {})", self.msg, self.loc)
      }
      ErlErrorType::ParserInternal => {
        write!(f, "Parser internal error: {} (at {})", self.msg, self.loc)
      }
      ErlErrorType::Internal => {
        write!(f, "Internal error: {} (at {})", self.msg, self.loc)
      }
      ErlErrorType::ErlangParse => {
        write!(f, "Erlang parse error: {} (at {})", self.msg, self.loc)
      }
      ErlErrorType::VariableNotFound(vname) => write!(f, "Variable not found: {}", vname),
      ErlErrorType::LocalFunctionNotFound(mfa) => write!(f, "Local function not found: {}", mfa),
      ErlErrorType::TypeErr(terr) => write!(f, "Type error: {}", terr),
      ErlErrorType::Unacceptable { ast_repr, context } => {
        write!(f, "{} is not acceptable in {}", ast_repr, context)
      }
    }
  }
}
