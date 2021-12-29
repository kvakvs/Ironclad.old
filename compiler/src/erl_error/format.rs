//! Printing for ErlError's

use crate::erl_error::ErlError;

impl std::fmt::Debug for ErlError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { write!(f, "{}", self) }
}

impl std::fmt::Display for ErlError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      ErlError::Interrupted(s) => write!(f, "Processing interrupted: {}", s),
      ErlError::Multiple(errs) => {
        writeln!(f, "Multiple errors:")?;
        for err in errs.iter() {
          writeln!(f, "{}", err)?;
        }
        Ok(())
      }
      ErlError::Io(ioerr) => write!(f, "File IO error: {}", ioerr),
      ErlError::Glob(gerr) => write!(f, "Directory scan error: {}", gerr),
      ErlError::GlobPattern(gperr) => write!(f, "Glob pattern error: {}", gperr),
      ErlError::Config(cfgerr) => write!(f, "Configuration file syntax error: {}", cfgerr),
      ErlError::PreprocessorParse { loc, msg } => {
        write!(f, "Preprocessor parse error: {} (at {})", msg, loc)
      }
      ErlError::PreprocessorSyntax { parse_err } => {
        write!(f, "Preprocessor syntax parse error: {}", parse_err)
      }
      ErlError::ParserInternal { loc, msg } => {
        write!(f, "Parser internal error: {} (at {})", msg, loc)
      }
      ErlError::Internal(msg) => {
        write!(f, "Internal error: {}", msg)
      }
      ErlError::ErlangParse { loc, msg } => {
        write!(f, "{} (at {})", msg, loc)
      }
      // ErlError::ErlangSyntax { parse_err, msg } => {
      //   write!(f, "Erlang syntax parse error: {} - {}", parse_err, msg)
      // }
      ErlError::VariableNotFound(vname) => write!(f, "Variable not found: {}", vname),
      ErlError::LocalFunctionNotFound(mfa) => write!(f, "Local function not found: {}", mfa),
      ErlError::TypeErr(terr) => write!(f, "Type error: {}", terr),
    }
  }
}
