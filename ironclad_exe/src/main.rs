extern crate libironclad;
extern crate libironclad_erlang;

use std::process::exit;

use libironclad::project::conf::ProjectConf;
use libironclad::project::ErlProject;
use libironclad_erlang::erl_syntax::parsers::parser_input::ParserInputImpl;
use libironclad_erlang::error::ic_error::IcResult;

fn main_do() -> IcResult<()> {
  // Test default project from ""
  let default_project: ErlProject = match ProjectConf::from_string(ParserInputImpl::from_str("")) {
    Ok(dp) => dp.into(),
    Err(e) => return Err(Box::new(e)),
  };
  println!("default {:?}", default_project);

  let mut project: ErlProject = match ProjectConf::from_project_file("test_project/ironclad.toml") {
    Ok(erlp) => erlp.into(),
    Err(e) => return Err(Box::new(e)),
  };
  println!("{:?}", project);

  let inputs = match project.build_file_list() {
    Ok(i) => i,
    Err(e) => return Err(Box::new(e)),
  };

  project.compile(inputs)
}

fn main() {
  match main_do() {
    Ok(_) => {
      println!("Ironclad finished.");
      exit(0);
    }
    Err(e) => {
      println!("{}", e);
      exit(e.get_process_exit_code())
    }
  }
}
