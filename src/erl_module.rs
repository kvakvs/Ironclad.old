use std::fmt::Debug;
use std::fmt;
use crate::compiler_opts::CompilerOpts;

// Erlang Module consists of
// - List of forms: attributes, and Erlang functions
// - Compiler options used to produce this module
pub struct ErlModule {
    compiler_opts: CompilerOpts,
    name: String,
}

impl Default for ErlModule {
    fn default() -> Self {
        Self {
            compiler_opts: Default::default(),
            name: "".to_string()
        }
    }
}


impl Debug for ErlModule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ErlModule({})", self.name)
    }
}
