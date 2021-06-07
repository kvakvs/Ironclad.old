use std::{fmt, fs};
use std::fmt::Debug;

use nom::Parser;
use serde_derive::Deserialize;

use compiler_opts::CompilerOpts;
use input_opts::InputOpts;

use crate::erl_error::ErlError;

mod input_opts;
pub mod compiler_opts;

// Erlang Program consists of
// - Module list
// - Compiler options (input search paths, output paths, flags)
#[derive(Deserialize)]
pub struct ErlProject {
    compiler_opts: CompilerOpts,
    inputs: InputOpts,
}

impl ErlProject {
    pub fn from_project_file(file: &str) -> Result<Self, ErlError> {
        let config_str = fs::read_to_string(file)?;

        // Parse, and convert toml error into ErlError
        toml::from_str(&config_str).map_err(|e| e.into())
    }
}


impl Debug for ErlProject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ErlProject({:?}, {:?})", self.inputs, self.compiler_opts)
    }
}
