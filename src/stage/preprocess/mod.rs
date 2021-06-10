use crate::erl_error::{ErlResult, ErlError};
use crate::erl_parse;
use crate::project::ErlProject;
use crate::stage::file_contents_cache::FileContentsCache;
use errloc_macros::errloc;
use std::borrow::{BorrowMut, Borrow};
use std::collections::HashSet;
use std::path::PathBuf;
use std::sync::{Arc, RwLock};
use crate::erl_parse::pp_ast::{PpAstNode, PpAstCache};
use crate::stage::ast_cache::AstCache;
use crate::types::ArcRw;

/// Returns: True if a file was preprocessed
fn preprocess_file(file_name: &PathBuf,
                   hrl_cache: &mut PpAstCache,
                   erl_cache: &mut FileContentsCache) -> ErlResult<bool> {
  let contents = erl_cache.contents.get(file_name).unwrap(); // trust that file exists
  let (tail, pp_ast) = erl_parse::pp_parse::parse_module(&contents)?;
  println!("\n\
        filename: {}\n\
        PP AST {:?}", file_name.display(), pp_ast);
  if tail.len() > 0 {
    println!("Parse did not succeed. Remaining input: {}", tail);
    return Ok(false);
  }

  let output = handle_pp_ast(pp_ast, hrl_cache);

  // Success: insert new string into preprocessed source cache
  erl_cache.contents.insert(file_name.clone(), output);
  Ok(true)
}

/// Interpret parsed attributes/preprocessor directives from top to bottom
/// - Exclude ifdef/if/ifndef sections where the condition check fails
/// - Load include files and paste them where include directive was found. Continue interpretation.
/// - Substitute macros.
/// In the end, return a new preprocessed string.
fn handle_pp_ast(ast: Vec<PpAstNode>, hrl_cache: &mut PpAstCache) -> String {
  let mut output: Vec<String> = Vec::with_capacity(ast.len());

  // From top to down interpret the preprocessed AST. Includes will restart the intepretation
  // from the pasted included AST.
  let mut pos = 0usize;

  while pos < ast.len() {
    match ast[pos] {
      PpAstNode::Comment(_) => {}
      PpAstNode::Text(_) => {}
      // An attribute without parens
      PpAstNode::Attr0(_) => {}
      PpAstNode::Attr(_, _) => {}
      PpAstNode::PasteMacro(_, _) => {}
      PpAstNode::PasteMacroAsString(_, _) => {}
    }

    pos += 1;
  }

  output.join("")
}

/// Preprocessor stage
/// * Rough pre-parse loaded ERL files, being only interested in preprocessor tokens.
/// * Pre-parse include files AST and paste into include locations.
/// * Drop AST branches covered by the conditional compile directives.
/// Side effects: Updates file contents cache
/// Returns: preprocessed collection of module sources
pub fn run(project: &mut ErlProject,
           file_contents: ArcRw<FileContentsCache>,
) -> ErlResult<()> {
  let mut hrl_cache = PpAstCache::new();

  // Take only .erl files
  let mut erl_cache_rw = file_contents.write().unwrap();

  let all_erl_files: HashSet<PathBuf> = erl_cache_rw.contents.keys()
      .into_iter()
      .filter(|path| path.to_string_lossy().ends_with(".erl"))
      .cloned()
      .collect();
  let mut preprocessed_count = 0;

  // For all input files, run preprocess parse and interpred the preprocessor directives
  // Loaded and parsed HRL files are cached to be inserted into every include location
  all_erl_files.into_iter().for_each(
    |path| {
      if preprocess_file(&path,
                         &mut hrl_cache,
                         erl_cache_rw.borrow_mut()).unwrap() {
        preprocessed_count += 1;
      }
    });

  println!("Preprocessed {} sources, {} includes",
           preprocessed_count,
           hrl_cache.syntax_trees.len());
  Ok(())
}
