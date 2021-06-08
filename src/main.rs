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

    // Print all
    // project.file_set.into_iter()
    //     .for_each(|p| println!("Input file: {}", p.display()));

    // Load files and store contents in the hashmap
    let file_cache = stage::preload::run(&mut project);

    // Parse all ERL and HRL files
    let ast_cache = stage::parse::run(&mut project, file_cache.clone());

    // Preprocess erl files, and store preprocessed (Text|AST?) in a new hashmap
    // let preproc_cache = stage::preprocess::run(&mut project, file_cache.clone());

    ()
}
