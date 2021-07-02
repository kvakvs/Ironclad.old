use crate::erl_error::{ErlResult};
use crate::project::ErlProject;
use crate::stage::file_contents_cache::FileContentsCache;
use std::path::{PathBuf, Path};
use crate::erl_parse::pp_ast::{PpAstNode, PpAstCache, PpAstTree};
use crate::types::{ArcRw, create_arcrw, with_arcrw_write, with_arcrw_read};
use std::sync::Arc;
use crate::project::source_file::SourceFile;
use std::collections::HashMap;

enum PpCondition {
  Ifdef(String),
  Ifndef(String),
}

impl PpCondition {
  /// Produces inverse of ifdef/indef, for -else. directive
  pub fn invert(&self) -> Self {
    match self {
      PpCondition::Ifdef(n) => PpCondition::Ifndef(n.clone()),
      PpCondition::Ifndef(n) => PpCondition::Ifdef(n.clone()),
    }
  }

  pub fn get_condition_value(&self, symbols: &HashMap<String, String>) -> bool {
    match self {
      PpCondition::Ifdef(n) => symbols.contains_key(n),
      PpCondition::Ifndef(n) => !symbols.contains_key(n),
    }
  }
}

/// Preprocessor state with AST cache, macro definitions, etc
struct PpState {
  ast_cache: ArcRw<PpAstCache>,
  file_cache: ArcRw<FileContentsCache>,

  condition_stack: Vec<PpCondition>,

  /// Contains unparsed symbol definitions. For preprocessor its either direct copy & paste into the
  /// code output, or use symbol existence for ifdef/ifndef conditions.
  pp_symbols: HashMap<String, String>,
}

fn load_and_parse_pp_ast(source_file: &Arc<SourceFile>) -> ErlResult<Arc<PpAstTree>> {
  let pp_ast = PpAstTree::from_source_file(&source_file)?;
  // println!("\n\
  //       filename: {}\n\
  //       PP AST ", source_file.file_name.display());
  // pp_ast.nodes.iter().for_each(|n| print!("{}", n.fmt(&source_file)));
  Ok(Arc::new(pp_ast))
}

impl PpState {
  /// Returns: True if a file was preprocessed
  fn preprocess_file(&mut self, file_name: &Path) -> ErlResult<bool> {
    let contents = with_arcrw_read(
      &self.file_cache,
      |fc| fc.all_files.get(file_name).unwrap().clone(),
    ); // trust that file exists

    let ast_tree = load_and_parse_pp_ast(&contents)?;

    with_arcrw_write(
      &self.ast_cache,
      |ac| ac.syntax_trees.insert(file_name.to_path_buf(), ast_tree.clone()),
    );

    let pp_ast = self.interpret_pp_ast(&contents, ast_tree.clone())?;
    let output: String = pp_ast
        .into_iter()
        .map(|node| node.fmt())
        .collect::<Vec<String>>()
        .join("\n");

    // Success: insert new string into preprocessed source cache
    let mut file_cache_rw = self.file_cache.write().unwrap();
    file_cache_rw.update_source_text(file_name, output);
    drop(file_cache_rw);

    // Cleanup
    Ok(true)
  }
}

fn interpret_include_directive(source_file: &SourceFile,
                               node: &PpAstNode,
                               ast_cache: ArcRw<PpAstCache>,
                               file_cache: ArcRw<FileContentsCache>) -> ErlResult<PpAstNode> {
  match node {
    // Found an attr directive which is -include("something")
    // TODO: Refactor into a outside function with error handling
    PpAstNode::IncludeLib(path)
    | PpAstNode::Include(path) => {
      // Take source file's parent dir and append to it the include path (unless it was absolute?)
      let source_path = &source_file.file_name;

      let include_path0 = PathBuf::from(path);
      let include_path = if include_path0.is_absolute()
      { include_path0 } else { source_path.parent().unwrap().join(include_path0) };

      // TODO: Path resolution relative to the file path
      let ast_r = ast_cache.read().unwrap();
      let get_cached = ast_r.syntax_trees.get(&include_path);

      match get_cached {
        None => {
          drop(ast_r);

          let include_source_file = with_arcrw_write(
            &file_cache,
            |fc| fc.get_or_load(&include_path),
          ).unwrap();
          let ast_tree = load_and_parse_pp_ast(&include_source_file).unwrap();
          with_arcrw_write(&ast_cache,
                           |ac| {
                             ac.syntax_trees.insert(include_path.clone(), ast_tree.clone());
                           });
          Ok(PpAstNode::IncludedFile(ast_tree))
        }
        Some(arc_ast) => {
          let result = PpAstNode::IncludedFile(arc_ast.clone());
          drop(ast_r);
          Ok(result)
        }
      }
    }
    _ => Ok(node.clone())
  }
}

impl PpState {
  /// This is called for each Preprocessor AST node to make the final decision whether the node
  /// is passed into the output or replaced with a SKIP.
  fn interpret_pp_rule_map(&mut self, node: &PpAstNode, source_file: &SourceFile) -> Option<PpAstNode> {
    // First process define/ifdef/if!def/else/endif
    match node {
      PpAstNode::Define(symbol, value) => {
        self.pp_symbols.insert(symbol.clone(), value.clone());
        return None;
      }

      PpAstNode::Ifdef(symbol) => {
        self.condition_stack.push(PpCondition::Ifdef(symbol.clone()));
        return None;
      }

      PpAstNode::Ifndef(symbol) => {
        self.condition_stack.push(PpCondition::Ifndef(symbol.clone()));
        return None;
      }

      PpAstNode::Else => {
        // TODO: If condition stack is empty, produce an error
        let last_condition = self.condition_stack.pop().unwrap();
        self.condition_stack.push(last_condition.invert());
        return None;
      }

      PpAstNode::Endif => {
        // TODO: If condition stack is empty, produce an error
        self.condition_stack.pop();
        return None;
      }
      _ => {}
    }

    // Then check if condition stack is not empty and last condition is valid
    if !self.condition_stack.is_empty() {
      if !self.condition_stack.last().unwrap().get_condition_value(&self.pp_symbols) {
        return None;
      }
    }

    // Then copy or modify remaining AST nodes
    match node {
      PpAstNode::Comment(_)
      | PpAstNode::Text(_) => {} // default behaviour: clone into output

      // An attribute without parens
      // PpAstNode::Attr { name: _, args: _ } => println!("{:?}", node.fmt(source_file)),

      PpAstNode::IncludedFile(include_ast_tree) => {
        // TODO: Return ErlResult
        self.interpret_pp_ast(source_file, include_ast_tree.clone()).unwrap();
      }

      PpAstNode::Include(_) => unreachable!("-include() must be eliminated at this stage"),
      PpAstNode::IncludeLib(_) => unreachable!("-include_lib() must be eliminated at this stage"),
      PpAstNode::File(_) => unreachable!("File() root AST node must be eliminated on load"),
      _ => {}
    }

    Some(node.clone())
  }

  /// Preallocate so many slots for ifdef/ifndef stack
  const DEFAULT_CONDITION_STACK_SIZE: usize = 16;

  pub fn new(ast_cache: &ArcRw<PpAstCache>,
             file_cache: &ArcRw<FileContentsCache>) -> Self {
    Self {
      ast_cache: ast_cache.clone(),
      file_cache: file_cache.clone(),
      condition_stack: Vec::with_capacity(Self::DEFAULT_CONDITION_STACK_SIZE),
      pp_symbols: Default::default(),
    }
  }

  /// Interpret parsed attributes/preprocessor directives from top to bottom
  /// - Exclude ifdef/if/ifndef sections where the condition check fails
  /// - Load include files and paste them where include directive was found. Continue interpretation.
  /// - Substitute macros.
  ///
  /// Return: a new preprocessed string joined together.
  fn interpret_pp_ast(&mut self,
                      source_file: &SourceFile,
                      ast_tree: Arc<PpAstTree>) -> ErlResult<Vec<PpAstNode>> {
    // From top to down interpret the preprocessed AST. Includes will restart the intepretation
    // from the pasted included AST.
    let interpreted = ast_tree.nodes.iter()
        .filter_map(|node| {
          let result = self.interpret_pp_rule_map(node, &source_file);
          match &result {
            Some(r) => println!("Interpret: {:30} → {}", node.fmt(), r.fmt()),
            None => println!("Interpret: {:30} → ×", node.fmt() ),
          }
          result
        })
        .collect();
    Ok(interpreted)
  }
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
  let all_files: Vec<PathBuf> = file_cache_r.all_files.keys().cloned().collect();
  drop(file_cache_r);

  let mut preprocessed_count = 0;

  all_files.into_iter()
      .filter(|path| path.to_string_lossy().ends_with(".erl"))
      // For all input files, run preprocess parse and interpred the preprocessor directives
      // Loaded and parsed HRL files are cached to be inserted into every include location
      .for_each(
        |path| {
          let mut pp_state = PpState::new(&ast_cache, &file_cache);
          if pp_state.preprocess_file(&path).unwrap() {
            preprocessed_count += 1;
          }
        });

  let cached_ast_trees_count = with_arcrw_read(&ast_cache, |ac| ac.syntax_trees.len());
  println!("Preprocessed {} sources, {} includes",
           preprocessed_count,
           cached_ast_trees_count);
  Ok(())
}
