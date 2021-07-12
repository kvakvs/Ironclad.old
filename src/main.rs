#![warn(missing_docs)]
#![allow(dead_code)]

//! Typed Erlang - typed_erlc
//! A fusion of Erlang compiler erlc (lib/compile in OTP repository) and compile-time type checker,
//! somewhat like Dialyzer (lib/dialyzer in OTP repository).

mod syntaxtree;
mod project;
mod erl_module;
mod erl_error;
mod stage;
mod typing;

extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate toml;
extern crate serde;
extern crate serde_derive;
extern crate thiserror;
extern crate glob;
extern crate alphabet;
#[macro_use]
extern crate lazy_static;

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
