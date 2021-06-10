#![warn(missing_docs)]
#![allow(dead_code)]

//! Typed Erlang - typed_erlc
//! A fusion of Erlang compiler erlc (lib/compile in OTP repository) and compile-time type checker,
//! somewhat like Dialyzer (lib/dialyzer in OTP repository).

mod types;
mod erl_parse;
mod project;
mod erl_module;
mod erl_error;
mod stage;

extern crate nom;
// extern crate tap;
extern crate toml;
extern crate serde;
extern crate serde_derive;
extern crate thiserror;
extern crate glob;
// extern crate errloc_macros;

use crate::project::ErlProject;
use crate::project::conf::ErlProjectConf;

fn main() {
  // Test default project from ""
  let default_project: ErlProject = ErlProjectConf::from_string("")
      .unwrap().into();
  println!("default {:?}", default_project);

  let mut project: ErlProject = ErlProjectConf::from_project_file("test_project/erlproject.toml")
      .unwrap().into();
  println!("{:?}", project);
  project.file_set = project.build_file_list().unwrap();

  ErlProject::compile(project).unwrap();
}
