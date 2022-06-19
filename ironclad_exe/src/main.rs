extern crate libironclad;
extern crate libironclad_erlang;

use std::process::exit;

use libironclad::stage::file_preload::FilePreloadStage;
use libironclad::stage::parse::ErlParseStage;
use libironclad_erlang::error::ic_error::IcResult;
use libironclad_erlang::project::conf::ProjectConf;
use libironclad_erlang::project::project_impl::ErlProjectImpl;
use libironclad_erlang::project::ErlProject;

/// Test default project created from "" empty config
fn test_empty_config_project() -> IcResult<()> {
  let default_project: ErlProject = match ProjectConf::from_string("") {
    Ok(dp) => ErlProjectImpl::from(dp).into(),
    Err(e) => return Err(Box::new(e)),
  };
  println!("default {:?}", default_project);
  Ok(())
}

fn main_do() -> IcResult<()> {
  if cfg!(debug_assertions) {
    test_empty_config_project()?;
  }

  let project: ErlProject = match ProjectConf::from_project_file("test_project/ironclad.toml") {
    Ok(erlp) => ErlProjectImpl::from(erlp).into(),
    Err(e) => return Err(Box::new(e)),
  };
  println!("{:?}", project);

  project.build_file_list()?;

  // Load files and store contents in the hashmap
  let mut preload_stage = FilePreloadStage::default();
  let all_inputs = if let Ok(r_inputs) = project.inputs.read() {
    r_inputs.inputs.clone()
  } else {
    panic!("Can't lock project inputs to retrieve names of the preload files")
  };
  let file_cache = match preload_stage.run_preload_stage(&all_inputs) {
    Ok(fc) => fc,
    Err(e) => return Err(Box::new(e)),
  };

  // Parse all ERL files and their included includes
  ErlParseStage::run_parse_stage(&project, file_cache).unwrap();
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
