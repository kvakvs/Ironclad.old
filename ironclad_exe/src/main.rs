//! # Ironclad parser and type analyzer + compiler
//!
//! A fusion of Erlang erlc (lib/compile in OTP repository) and compile-time type checker,
//! somewhat like Dialyzer (lib/dialyzer in OTP repository).
extern crate function_name;
extern crate libironclad_erlang;

use std::process::exit;

use libironclad_erlang::error::ic_error::IroncladResult;
use libironclad_erlang::exit_codes::erl_fatal_icerror;
use libironclad_erlang::project::conf::ProjectConf;
use libironclad_erlang::project::project_impl::ErlProjectImpl;
use libironclad_erlang::project::ErlProject;
use stage::stage_parse::ErlParseStage;

pub mod stage;

fn main_do() -> IroncladResult<()> {
  let project: ErlProject = match ProjectConf::from_project_file("test_project/ironclad.toml") {
    Ok(erlp) => ErlProjectImpl::from(erlp).into(),
    Err(e) => return Err(e),
  };
  println!("{}", project);

  project.build_file_list()?;

  // Parse all ERL files and their included includes
  if let Err(e) = ErlParseStage::run_parse_stage(&project) {
    erl_fatal_icerror(e);
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
