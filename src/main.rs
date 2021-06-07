mod erl_parse;
mod project;
mod erl_module;
mod erl_error;

extern crate nom;
extern crate toml;
extern crate serde;
extern crate serde_derive;
extern crate thiserror;

use crate::project::ErlProject;
use crate::project::conf::ErlProjectConf;

fn main() {
    // Test default project from ""
    let default_project: ErlProject = ErlProjectConf::from_string("")
        .unwrap().into();
    println!("default {:?}", default_project);

    let project: ErlProject = ErlProjectConf::from_project_file("test_project/erlproject.toml")
        .unwrap().into();
    println!("{:?}", project)
}
