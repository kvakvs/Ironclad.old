use crate::erl_error::{ErlResult};
use crate::erl_parse;
use crate::project::ErlProject;
use crate::stage::file_contents_cache::FileContentsCache;
use std::path::{PathBuf, Path};
use crate::erl_parse::pp_ast::{PpAstNode, PpAstCache, PpAstTree};
use crate::types::{ArcRw, create_arcrw, with_arcrw_write, with_arcrw_read};

fn load_and_parse_pp_ast(file_name: &Path, contents: &str) -> ErlResult<ArcRw<PpAstTree>> {
  let pp_ast = erl_parse::pp_parse::parse_module(file_name, &contents)?;
  println!("\n\
        filename: {}\n\
        PP AST {:?}", file_name.display(), pp_ast.nodes);
  Ok(create_arcrw(pp_ast))
}

/// Returns: True if a file was preprocessed
fn preprocess_file(file_name: &Path,
                   ast_cache: ArcRw<PpAstCache>,
                   file_cache: ArcRw<FileContentsCache>) -> ErlResult<bool> {
  // let mut file_cache_r = file_cache.read().unwrap();
  // let contents = file_cache_r.contents.get(file_name).unwrap(); // trust that file exists
  // drop(file_cache_r);
  let contents = with_arcrw_read(
    &file_cache,
    |fc| fc.contents.get(file_name).unwrap().clone(),
  ); // trust that file exists

  let ast_tree = load_and_parse_pp_ast(file_name, &contents)?;

  // let ast_cache_rw = ast_cache.write().unwrap();
  // ast_cache_rw.syntax_trees.insert(file_name.clone(), ast_tree.clone());
  // drop(ast_cache_rw);
  with_arcrw_write(
    &ast_cache,
    |ac| ac.syntax_trees.insert(file_name.to_path_buf(), ast_tree.clone()),
  );

  let output = interpret_pp_ast(&file_name, ast_tree, ast_cache,
                                file_cache.clone())?;

  // Success: insert new string into preprocessed source cache
  let mut file_cache_rw = file_cache.write().unwrap();
  file_cache_rw.contents.insert(file_name.to_path_buf(), output);
  drop(file_cache_rw);

  Ok(true)
}

fn interpret_include_directive(source_file_path: &Path,
                               node: &PpAstNode,
                               ast_cache: ArcRw<PpAstCache>,
                               file_cache: ArcRw<FileContentsCache>) -> PpAstNode {
  match node {
    // Found an attr directive which is -include("something")
    // TODO: Refactor into a outside function with error handling
    // TODO: -include_lib()
    PpAstNode::Attr(name, args) if name == "include" => {
      let path_in_quotes = args[0].trim_matches(|c| c == '\"');

      // Take source file's parent dir and append to it the include path (unless it was absolute?)
      let path_in_include_directive = Path::new(path_in_quotes);
      let source_path = Path::new(source_file_path);

      let include_path = if path_in_include_directive.is_absolute() {
        PathBuf::from(path_in_quotes)
      } else {
        source_path.parent().unwrap().join(path_in_quotes)
      };

      // TODO: Path resolution relative to the file path
      let ast_r = ast_cache.read().unwrap();
      let get_cached = ast_r.syntax_trees.get(&include_path);

      match get_cached {
        None => {
          drop(ast_r);

          let include_contents = with_arcrw_write(
            &file_cache,
            |fc| fc.get_or_load(&include_path),
          ).unwrap();
          let ast_tree = load_and_parse_pp_ast(&include_path,
                                               &include_contents).unwrap();
          with_arcrw_write(&ast_cache,
                           |ac| {
                             ac.syntax_trees.insert(include_path.clone(), ast_tree.clone());
                           });
          PpAstNode::IncludedFile(ast_tree)
        }
        Some(arc_ast) => {
          let result = PpAstNode::IncludedFile(arc_ast.clone());
          drop(ast_r);
          result
        }
      }
    }
    _ => node.clone()
  }
}

/// Interpret parsed attributes/preprocessor directives from top to bottom
/// - Exclude ifdef/if/ifndef sections where the condition check fails
/// - Load include files and paste them where include directive was found. Continue interpretation.
/// - Substitute macros.
///
/// Return: a new preprocessed string joined together.
fn interpret_pp_ast(source_file_path: &Path,
                    ast_tree: ArcRw<PpAstTree>,
                    ast_cache: ArcRw<PpAstCache>,
                    file_cache: ArcRw<FileContentsCache>) -> ErlResult<String> {
  let ast_tree_r = ast_tree.read().unwrap();
  let mut output: Vec<String> = Vec::with_capacity(ast_tree_r.nodes.len());

  // From top to down interpret the preprocessed AST. Includes will restart the intepretation
  // from the pasted included AST.
  let mut pos = 0usize;

  let a: Vec<PpAstNode> = ast_tree_r.nodes.iter()
      .map(|node| {
        interpret_include_directive(source_file_path,
                                    node,
                                    ast_cache.clone(),
                                    file_cache.clone())
      })
      .collect();

  a.into_iter()
      .for_each(|node| {
        println!("Interpret: {:?}", node);

        match node {
          PpAstNode::Comment(_) => {} // skip
          PpAstNode::Text(t) => output.push(t), // TODO: Push spans from input

          // An attribute without parens
          PpAstNode::Attr0(_) => {}
          PpAstNode::Attr(_, _) => {}

          PpAstNode::PasteMacro(name, _) => println!("paste macro ?{}", name),
          PpAstNode::PasteMacroAsString(name, _) => println!("paste macro param ??{}", name),

          PpAstNode::IncludedFile(include_ast_tree) => {
            // TODO: Return ErlResult
            let include_interpret_output = interpret_pp_ast(
              source_file_path,
              include_ast_tree,
              ast_cache.clone(),
              file_cache.clone(),
            ).unwrap();
            output.push(include_interpret_output);
          }
        }

        pos += 1;
      });

  drop(ast_tree_r);
  Ok(output.join(""))
}

/// Preprocessor stage
/// * Rough pre-parse loaded ERL files, being only interested in preprocessor tokens.
/// * Pre-parse include files AST and paste into include locations.
/// * Drop AST branches covered by the conditional compile directives.
/// Side effects: Updates file contents cache
/// Returns: preprocessed collection of module sources
pub fn run(_project: &mut ErlProject,
           file_cache: ArcRw<FileContentsCache>,
) -> ErlResult<()> {
  let ast_cache = create_arcrw(PpAstCache::new());

  // Take only .erl files
  let file_cache_r = file_cache.read().unwrap();
  let all_files: Vec<PathBuf> = file_cache_r.contents.keys().cloned().collect();
  drop(file_cache_r);

  let mut preprocessed_count = 0;

  all_files.into_iter()
      .filter(|path| path.to_string_lossy().ends_with(".erl"))
      // For all input files, run preprocess parse and interpred the preprocessor directives
      // Loaded and parsed HRL files are cached to be inserted into every include location
      .for_each(
        |path| {
          if preprocess_file(&path,
                             ast_cache.clone(),
                             file_cache.clone()).unwrap() {
            preprocessed_count += 1;
          }
        });

  let cached_ast_trees_count = with_arcrw_read(&ast_cache, |ac| ac.syntax_trees.len());
  println!("Preprocessed {} sources, {} includes",
           preprocessed_count,
           cached_ast_trees_count);
  Ok(())
}
