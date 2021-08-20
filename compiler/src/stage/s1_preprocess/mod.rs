//! Preprocess Stage - parses and interprets the Erlang source and gets rid of -if/-ifdef/-ifndef
//! directives, substitutes HRL files contents in place of -include/-include_lib etc.

use std::collections::HashMap;
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::{Arc, Mutex};

use crate::erl_error::{ErlError, ErlResult};
use crate::project::ErlProject;
use crate::project::source_file::SourceFile;
use crate::source_loc::{ErrorLocation, SourceLoc};
use crate::stage::file_contents_cache::FileContentsCache;
use crate::preprocessor::syntax_tree::pp_ast::{PpAst, PpAstCache, PpAstTree};

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
  ast_cache: Arc<Mutex<PpAstCache>>,
  file_cache: Arc<Mutex<FileContentsCache>>,

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
    // trust that file exists
    let contents = {
      let file_cache1 = self.file_cache.lock().unwrap();
      file_cache1.all_files.get(file_name).unwrap().clone()
    };

    let ast_tree = load_and_parse_pp_ast(&contents)?;

    {
      let mut ast_cache = self.ast_cache.lock().unwrap();
      ast_cache.items.insert(file_name.to_path_buf(), ast_tree.clone());
    }

    let pp_ast = self.interpret_pp_ast(&contents, ast_tree)?;

    let output: String = pp_ast.to_string();

    { // Success: insert new string into preprocessed source cache
      let mut file_cache2 = self.file_cache.lock().unwrap();
      file_cache2.update_source_text(file_name, output);
    }

    // Cleanup
    Ok(true)
  }
}

fn interpret_include_directive(source_file: &SourceFile,
                               node: &PpAst,
                               ast_cache: Arc<Mutex<PpAstCache>>,
                               file_cache: Arc<Mutex<FileContentsCache>>) -> ErlResult<PpAst> {
  match node {
    // Found an attr directive which is -include("something")
    // TODO: Refactor into a outside function with error handling
    PpAst::IncludeLib(path)
    | PpAst::Include(path) => {
      // Take source file's parent dir and append to it the include path (unless it was absolute?)
      let source_path = &source_file.file_name;

      let include_path0 = PathBuf::from(path);
      let include_path = if include_path0.is_absolute()
      { include_path0 } else { source_path.parent().unwrap().join(include_path0) };

      // TODO: Path resolution relative to the file path
      let mut ast_cache1 = ast_cache.lock().unwrap();
      let find_result = ast_cache1.items.get(&include_path);

      match find_result {
        None => {
          let include_source_file = {
            let mut file_cache1 = file_cache.lock().unwrap();
            file_cache1.get_or_load(&include_path).unwrap()
          };
          let ast_tree = load_and_parse_pp_ast(&include_source_file).unwrap();

          // let mut ast_cache1 = ast_cache.lock().unwrap();
          ast_cache1.items.insert(include_path.clone(), ast_tree.clone());

          Ok(PpAst::IncludedFile(ast_tree))
        }
        Some(arc_ast) => {
          let result = PpAst::IncludedFile(arc_ast.clone());
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
  fn interpret_pp_rule_map(&mut self, node: &Rc<PpAst>,
                           source_file: &SourceFile) -> Option<Rc<PpAst>> {
    // First process ifdef/if!def/else/endif
    match node.deref() {
      PpAst::Ifdef(symbol) => {
        self.condition_stack.push(PpCondition::Ifdef(symbol.clone()));
        return None;
      }

      PpAst::Ifndef(symbol) => {
        self.condition_stack.push(PpCondition::Ifndef(symbol.clone()));
        return None;
      }

      PpAst::Else => {
        // TODO: If condition stack is empty, produce an error
        let last_condition = self.condition_stack.pop().unwrap();
        self.condition_stack.push(last_condition.invert());
        return None;
      }

      PpAst::Endif => {
        // TODO: If condition stack is empty, produce an error
        self.condition_stack.pop();
        return None;
      }
      _ => {}
    }

    // Then check if condition stack is not empty and last condition is valid
    if !self.condition_stack.is_empty() &&
        !self.condition_stack.last().unwrap().get_condition_value(&self.pp_symbols) {
      return None;
    }

    // Then copy or modify remaining AST nodes
    match node.deref() {
      PpAst::Define(symbol, value) => {
        println!("define {} = {}", symbol, value);
        self.pp_symbols.insert(symbol.clone(), value.clone());
        return None;
      }

      PpAst::Undef(symbol) => {
        self.pp_symbols.remove(symbol);
        return None;
      }

      PpAst::Comment(_)
      | PpAst::Text(_) => {} // default behaviour: clone into output

      // An attribute without parens
      // PpAstNode::Attr { name: _, args: _ } => println!("{:?}", node.fmt(source_file)),
      PpAst::Include(_path) => {
        println!("TODO: interpret Include directive");
        return None;
      }
      PpAst::IncludeLib(_path) => {
        println!("TODO: interpret IncludeLib directive");
        return None;
      }

      PpAst::IncludedFile(include_ast_tree) => {
        // TODO: Return ErlResult
        self.interpret_pp_ast(source_file, include_ast_tree.clone()).unwrap();
      }

      #[allow(unreachable_patterns)]
      PpAst::Include(_) => unreachable!("-include() must be eliminated at this stage"),

      #[allow(unreachable_patterns)]
      PpAst::IncludeLib(_) => unreachable!("-include_lib() must be eliminated at this stage"),

      #[allow(unreachable_patterns)]
      PpAst::File(_) => unreachable!("File() root AST node must be eliminated on load"),

      _ => {}
    }

    Some(node.clone())
  }

  /// Preallocate so many slots for ifdef/ifndef stack
  const DEFAULT_CONDITION_STACK_SIZE: usize = 16;

  /// Create a preprocessor state struct for processing a file.
  /// Preprocessor symbols are filled from the command line and project TOML file settings.
  pub fn new(ast_cache: &Arc<Mutex<PpAstCache>>,
             file_cache: &Arc<Mutex<FileContentsCache>>,
             init_symbols: HashMap<String, String>) -> Self {
    Self {
      ast_cache: ast_cache.clone(),
      file_cache: file_cache.clone(),
      condition_stack: Vec::with_capacity(Self::DEFAULT_CONDITION_STACK_SIZE),
      pp_symbols: init_symbols,
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
                      ast_tree: Arc<PpAstTree>) -> ErlResult<Rc<PpAst>> {
    // From top to down interpret the preprocessed AST. Includes will restart the intepretation
    // from the pasted included AST.
    if let PpAst::File(nodes) = ast_tree.nodes.deref() {
      let interpreted: Vec<Rc<PpAst>> = nodes.iter()
          .filter_map(|node| {
            let result = self.interpret_pp_rule_map(node, &source_file);
            match &result {
              Some(r) => println!("Interpret: {:40} → {}", node.to_dbg_str(), r.to_dbg_str()),
              None => println!("Interpret: {:40} → ×", node.to_dbg_str()),
            }
            result
          })
          .collect();
      return Ok(Rc::new(PpAst::File(interpreted)));
    }
    let err_s =
        format!("Preprocessor parse did not return a root AST node, got something else: {:?}",
                ast_tree.nodes);
    Err(ErlError::PreprocessorParse {
      loc: ErrorLocation::new(Some(source_file.file_name.clone()),
                              SourceLoc::default()),
      msg: err_s,
    })
  }
}

/// Stage 1 - Preprocessor stage
/// ----------------------------
/// * Preparse loaded ERL files ignoring the syntax only paying attention to preprocessor tokens.
/// * Preparse include files AST and paste preprocessor AST into include locations.
/// * Drop AST branches covered by the conditional compile directives.
///
/// Side effects: Updates file contents cache
/// Returns preprocessed collection of module sources
pub fn run(project: &mut ErlProject,
           file_cache: Arc<Mutex<FileContentsCache>>,
) -> ErlResult<Arc<Mutex<PpAstCache>>> {
  let ast_cache = Arc::new(Mutex::new(PpAstCache::default()));

  // Take only .erl files
  let all_files: Vec<PathBuf> = {
    let file_cache_r = file_cache.lock().unwrap();
    file_cache_r.all_files.keys().cloned().collect()
  };

  let mut preprocessed_count = 0;

  all_files.into_iter()
      .filter(|path| path.to_string_lossy().ends_with(".erl"))
      // For all input files, run s1_preprocess s2_parse and interpred the preprocessor directives
      // Loaded and parsed HRL files are cached to be inserted into every include location
      .for_each(
        |path| {
          let init_symbols = project.get_preprocessor_symbols(&path);
          let mut pp_state = PpState::new(&ast_cache, &file_cache, init_symbols);
          if pp_state.preprocess_file(&path).unwrap() {
            preprocessed_count += 1;
          }
        });

  let cached_ast_trees_count = {
    let ast_cache_r = ast_cache.lock().unwrap();
    ast_cache_r.items.len()
  };

  println!("Preprocessed {} sources, {} includes",
           preprocessed_count,
           cached_ast_trees_count);
  Ok(ast_cache)
}
