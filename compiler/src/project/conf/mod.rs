use serde_derive::Deserialize;
use core::fmt;
use core::fmt::Debug;
use core::option::Option;
use core::result::Result;
use std::convert::Into;
use std::fs;
use crate::project::conf::compiler_opts::CompilerOptsConf;
use crate::project::conf::input_opts::InputOptsConf;
use crate::erl_error::ErlError;

pub mod compiler_opts;
pub mod input_opts;

// Defines configuration file as it is loaded by TOML and serde
#[derive(Deserialize)]
pub struct ErlProjectConf {
  /// Input search paths, output paths, flags, ... etc
  pub compiler_opts: Option<CompilerOptsConf>,

  /// Input files and directories (wildcards are allowed)
  pub inputs: Option<InputOptsConf>,
}

impl ErlProjectConf {
  pub fn from_project_file(file: &str) -> Result<Self, ErlError> {
    let config_str = fs::read_to_string(file)?;

    // Parse, and convert toml error into ErlError
    toml::from_str(&config_str).map_err(|e| e.into())
  }

  pub fn from_string(input: &str) -> Result<Self, ErlError> {
    // Parse, and convert toml error into ErlError
    toml::from_str(input).map_err(|e| e.into())
  }
}


impl Debug for ErlProjectConf {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "ErlProject({:?}, {:?})", self.inputs, self.compiler_opts)
  }
}
