#![warn(missing_docs)]

mod types;
mod erl_parse;
mod project;
mod erl_module;
mod erl_error;
mod stage;

extern crate nom;
extern crate toml;
extern crate serde;
extern crate serde_derive;
extern crate thiserror;
extern crate glob;
extern crate errloc_macros;

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

  ErlProject::compile(project);

  ()
}
