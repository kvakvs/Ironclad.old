//! # Ironclad parser and type analyzer + compiler
//!
//! A fusion of Erlang erlc (lib/compile in OTP repository) and compile-time type checker,
//! somewhat like Dialyzer (lib/dialyzer in OTP repository).
extern crate function_name;
extern crate libironclad_erlang;

use std::process::exit;

use libironclad_erlang::error::ic_error::IcResult;
use libironclad_erlang::exit_codes::{erl_fatal_error, erl_fatal_icerror};
use libironclad_erlang::project::conf::ProjectConf;
use libironclad_erlang::project::project_impl::ErlProjectImpl;
use libironclad_erlang::project::ErlProject;
use stage::stage_parse::ErlParseStage;

pub mod stage;

// /// Test default project created from "" empty config
// fn test_empty_config_project() -> IcResult<()> {
//   let default_project: ErlProject = match ProjectConf::from_string("") {
//     Ok(dp) => ErlProjectImpl::from(dp).into(),
//     Err(e) => return Err(Box::new(e)),
//   };
//   // println!("default {:?}", default_project);
//   Ok(())
// }

fn main_do() -> IcResult<()> {
  // if cfg!(debug_assertions) {
  //   test_empty_config_project()?;
  // }

  let project: ErlProject = match ProjectConf::from_project_file("test_project/ironclad.toml") {
    Ok(erlp) => ErlProjectImpl::from(erlp).into(),
    Err(e) => return Err(Box::new(e)),
  };
  println!("{}", project);

  project.build_file_list()?;

  // Parse all ERL files and their included includes
  match ErlParseStage::run_parse_stage(&project) {
    Err(e) => {
      erl_fatal_icerror(e);
    }
    _ => (),
  }
  Ok(())
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
