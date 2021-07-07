use crate::project::ErlProject;
use std::sync::{Arc, RwLock, Mutex};
use crate::stage::file_contents_cache::FileContentsCache;
use std::path::{Path};
use crate::erl_error::{ErlResult};
use crate::stage::ast_cache::{AstCache, AstTree};
use crate::stage::compile_module::CompileModule;
use crate::project::compiler_opts::CompilerOpts;
use crate::project::source_file::SourceFile;
use crate::erl_parse::erl_ast::{ErlAstTree, ErlAstCache};

/// Run syntax parser on an ERL or HRL source file
fn parse_file(file_name: &Path,
              source_file: Arc<SourceFile>,
              compile_options: Arc<CompilerOpts>) -> ErlResult<ErlAstTree> {
  let _module = CompileModule::new(file_name, compile_options);

  // Dummy result
  Ok(ErlAstTree::new(source_file, vec![]))
}

/// Parse stage
/// * Parse loaded ERL files as Erlang.
/// Returns: Collection of AST trees for all affected ERL modules
pub fn run(project: &mut ErlProject,
           contents_cache: Arc<Mutex<FileContentsCache>>) -> ErlResult<Arc<ErlAstCache>> {
  let mut ast_cache = ErlAstCache::new_empty();
  let contents_cache_r = contents_cache.lock().unwrap();

  for (path, source_file) in &contents_cache_r.all_files {
    let path_s = path.to_string_lossy();

    // Take only .erl and .hrl files
    if path_s.ends_with(".erl") || path_s.ends_with(".hrl") {
      let compile_options = project.get_compiler_options_for(path);
      let ast_tree = parse_file(
        &path,
        source_file.clone(),
        compile_options,
      )?;
      ast_cache.items.insert(path.clone(), Arc::new(ast_tree));
    }
  }

  println!("Preprocessor parsed {} sources (.erl and .hrl)", ast_cache.items.len());

  let arc = Arc::new(ast_cache);
  Ok(arc)
}
