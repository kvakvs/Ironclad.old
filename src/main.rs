use crate::project::ErlProject;

mod erl_parse;
mod project;
mod erl_module;
mod erl_error;

extern crate nom;
extern crate toml;
extern crate serde;
extern crate serde_derive;
extern crate thiserror;

fn main() {
    let project = ErlProject::from_project_file("test_project/erlproject.toml");
    println!("{:?}", project)
}
