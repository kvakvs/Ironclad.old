use crate::project::ErlProject;
use std::sync::{Arc, RwLock};
use crate::stage::file_contents_cache::FileContentsCache;
use std::path::{Path};
use crate::erl_error::{ErlResult, ErlError, ErrorLocation};
use crate::stage::ast_cache::{AstCache, ModuleAST};
use crate::erl_parse::ast::ErlAstNode;
use crate::stage::compile_module::CompileModule;
use crate::project::compiler_opts::CompilerOpts;
use crate::types::ArcRw;
use crate::project::source_file::SourceFile;

/// Run syntax parser on an ERL or HRL source file
fn parse_file(file_name: &Path,
              file_contents: Arc<SourceFile>,
              compile_options: ArcRw<CompilerOpts>) -> ErlResult<Vec<ErlAstNode>> {
  let _module = CompileModule::new(file_name, compile_options);

  // let (remaining, ast_tree) = erl_parse::parse_module(file_contents)?;

  // if remaining.is_empty() {
  //   Ok(ast_tree)
  // } else {
  //   let msg = String::from("Source file still contains unconsumed data after parse");
  //   Err(ErlError::ErlParse(ErrorLocation::from_source_file(file_name), msg))
  // }
  ErlError::not_impl("parse module")
}

/// Parse stage
/// * Parse loaded ERL files.
/// Returns: Collection of AST trees for all affected ERL modules
pub fn run(project: &mut ErlProject,
           contents_cache: Arc<RwLock<FileContentsCache>>) -> ErlResult<Arc<RwLock<AstCache>>> {
  let mut ast_cache = AstCache::new();

  let file_contents_r = contents_cache.read().unwrap();

  for (path, source_file) in &file_contents_r.all_files {
    let path_s = path.to_string_lossy();

    // Take only .erl and .hrl files
    if path_s.ends_with(".erl") || path_s.ends_with(".hrl") {
      let compile_options = project.get_compiler_options_for(path);
      let ast_tree = parse_file(
        &path,
        source_file.clone(),
        compile_options
      )?;
      ast_cache.syntax_trees.insert(path.clone(),
                                    ModuleAST::new(ast_tree));
    }
  }

  println!("Parsed {} sources (.erl and .hrl)", ast_cache.syntax_trees.len());

  let arc = Arc::new(RwLock::new(ast_cache));
  Ok(arc)
}
