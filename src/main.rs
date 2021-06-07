mod erl_parse;
mod erl_project;
mod erl_module;
mod compiler_opts;
mod source_tree;
mod erl_error;

extern crate nom;
extern crate toml;
extern crate serde;
extern crate serde_derive;
extern crate thiserror;

use crate::erl_project::ErlProject;

fn main() {
    let project = ErlProject::from_project_file("erlproject");
    println!("{:?}", project)
}
