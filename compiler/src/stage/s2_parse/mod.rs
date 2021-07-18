//! Parses Erlang source into AST

use crate::project::ErlProject;
use std::sync::{Arc, Mutex};
use crate::stage::file_contents_cache::FileContentsCache;
use crate::erl_error::{ErlResult};
use crate::stage::compile_module::CompileModule;
use crate::project::source_file::SourceFile;
use crate::syntaxtree::erl::erl_ast::{ErlAstTree};
use crate::stage::code_cache::CodeCache;

/// Run syntax parser on an ERL or HRL source file
fn parse_file(source_file: &Arc<SourceFile>) -> ErlResult<Arc<ErlAstTree>> {
  let tree = ErlAstTree::from_source_file(&source_file)?;
  Ok(Arc::new(tree))
}

/// Parse stage
/// * Parse loaded ERL files as Erlang.
/// Returns: Collection of AST trees for all affected ERL modules
pub fn run(project: &mut ErlProject,
           contents_cache: Arc<Mutex<FileContentsCache>>) -> ErlResult<Arc<Mutex<CodeCache>>> {
  // let mut ast_cache = ErlAstCache::new_empty();
  let mut code_cache = CodeCache::new();

  let contents_cache_r = contents_cache.lock().unwrap();

  for (path, source_file) in &contents_cache_r.all_files {
    let path_s = path.to_string_lossy();

    // Take only .erl and .hrl files
    if path_s.ends_with(".erl") || path_s.ends_with(".hrl") {
      let compile_options = project.get_compiler_options_for(path);

      let mut module = CompileModule::new(&source_file, compile_options);
      module.compile(parse_file(&source_file)?);

      code_cache.items.insert(module.module_name.clone(),
                              Arc::new(Mutex::new(module)));
    }
  }

  println!("Compiler processed {} sources (.erl and .hrl)", code_cache.items.len());

  let result = Arc::new(Mutex::new(code_cache));
  Ok(result)
}
