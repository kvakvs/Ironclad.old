extern crate libironclad;
extern crate libironclad_error;

use std::process::exit;

use libironclad::project::ErlProject;
use libironclad::project::conf::ProjectConf;
use libironclad::erl_error::{ErlResult};
use libironclad_error::ic_error::IcResult;

fn main_do() -> IcResult<()> {
  // Test default project from ""
  let default_project: ErlProject = ProjectConf::from_string("")?.into();
  println!("default {:?}", default_project);

  let mut project: ErlProject = ProjectConf::from_project_file("test_project/ironclad.toml")?.into();
  println!("{:?}", project);

  let inputs = project.build_file_list()?;
  project.compile(inputs)
}

fn main() {
  match main_do() {
    Ok(_) => {
      println!("...finished.");
      exit(0);
    }
    Err(e) => {
      println!("Error occured: {}", e);
      exit(e.process_exit_code())
    }
  }
}
