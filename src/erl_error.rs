use std::path::{Path, PathBuf};

use thiserror::Error;

use crate::syntaxtree::pp::pp_parser;
use crate::syntaxtree::erl::erl_parser;
use crate::typing::error::TypeError;

#[derive(Debug)]
pub enum ErrorLocation {
  None,
  SourceFile(PathBuf),
}

impl ErrorLocation {
  pub fn from_source_file(p: &Path) -> ErrorLocation {
    Self::SourceFile(p.to_path_buf())
  }
}

#[derive(Error, Debug)]
pub enum ErlError {
  #[error("Multiple errors: {0:?}")]
  Multiple(Vec<ErlError>),

  #[error("Not implemented: {0}")]
  NotImpl(String),

  #[error("File IO error: {0:?}")]
  Io(std::io::Error),

  // Project errors produced when glob() scanning input files and directories
  #[error("Glob directory scan error: {0:?}")]
  Glob(glob::GlobError),

  #[error("Glob pattern error: {0:?}")]
  GlobPattern(glob::PatternError),

  // Project loading error produced when loading TOML
  #[error("Configuration file syntax error: {0:?}")]
  Config(toml::de::Error),

  // Lock poisoning happens when a Write-lock caught a panic
  // #[error("Compiler internal data locking error")]
  // LockingPoisonError,

  #[error("Preprocessor parse error: {1} (at {0:?})")]
  PpParse(ErrorLocation, String),

  #[error("Preprocessor syntax parse error: {0:?}")]
  PreprocessorSyntax(pest::error::Error<pp_parser::Rule>),

  #[error("Erlang syntax parse error: {0:?}")]
  ErlangSyntax(pest::error::Error<erl_parser::Rule>),

  #[error("Type error: {0:?}")]
  TypeError(TypeError),
}

impl ErlError {
  pub(crate) fn not_impl<T>(what: &str) -> ErlResult<T> {
    Err(ErlError::NotImpl(what.to_string()))
  }

  pub fn pp_parse<T>(file_name: &Path, message: &str) -> ErlResult<T> {
    Err(ErlError::PpParse(ErrorLocation::from_source_file(file_name),
                          String::from(message)))
  }
}

pub type ErlResult<T> = Result<T, ErlError>;

impl From<std::io::Error> for ErlError {
  fn from(value: std::io::Error) -> Self {
    ErlError::Io(value)
  }
}

impl From<toml::de::Error> for ErlError {
  fn from(value: toml::de::Error) -> Self {
    ErlError::Config(value)
  }
}

impl From<glob::GlobError> for ErlError {
  fn from(value: glob::GlobError) -> Self {
    ErlError::Glob(value)
  }
}

impl From<glob::PatternError> for ErlError {
  fn from(value: glob::PatternError) -> Self {
    ErlError::GlobPattern(value)
  }
}

impl From<pest::error::Error<pp_parser::Rule>> for ErlError {
  fn from(value: pest::error::Error<pp_parser::Rule>) -> Self {
    ErlError::PreprocessorSyntax(value)
  }
}

impl From<pest::error::Error<erl_parser::Rule>> for ErlError {
  fn from(value: pest::error::Error<erl_parser::Rule>) -> Self {
    ErlError::ErlangSyntax(value)
  }
}

impl From<TypeError> for ErlError {
  fn from(value: TypeError) -> Self {
    ErlError::TypeError(value)
  }
}