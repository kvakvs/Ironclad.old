extern crate compiler;

use compiler::project::ErlProject;
use compiler::project::conf::ErlProjectConf;

fn main() {
  // Test default project from ""
  let default_project: ErlProject = ErlProjectConf::from_string("")
      .unwrap().into();
  println!("default {:?}", default_project);

  let mut project: ErlProject = ErlProjectConf::from_project_file("test_project/erlproject.toml")
      .unwrap().into();
  println!("{:?}", project);
  project.file_set = project.build_file_list().unwrap();

  ErlProject::compile(project).unwrap();
}
