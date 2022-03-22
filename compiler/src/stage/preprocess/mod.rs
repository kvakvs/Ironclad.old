//! Preprocess Stage - parses and interprets the Erlang source and gets rid of -if/-ifdef/-ifndef
//! directives, substitutes HRL files contents in place of -include/-include_lib etc.
pub mod pp_scope;
pub mod pp_define;

use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use nom::Finish;

use crate::erl_error::{ErlError, ErlResult};
use crate::preprocessor::nom_parser::PreprocessorParser;
use crate::project::ErlProject;
use crate::project::source_file::SourceFile;
use crate::source_loc::{ErrorLocation, SourceLoc};
use crate::stage::file_contents_cache::FileContentsCache;
use crate::preprocessor::syntax_tree::pp_ast::{PpAst, PpAstCache};
use crate::stage::preprocess::pp_scope::PreprocessorScope;

enum PreprocessorCondition {
  Ifdef(String),
  Ifndef(String),
}

// TODO: Move into pp_scope
impl PreprocessorCondition {
  /// Produces inverse of ifdef/indef, for -else. directive
  pub fn invert(&self) -> Self {
    match self {
      PreprocessorCondition::Ifdef(n) => PreprocessorCondition::Ifndef(n.clone()),
      PreprocessorCondition::Ifndef(n) => PreprocessorCondition::Ifdef(n.clone()),
    }
  }

  /// Given current preprocessor scope, check whether preprocessor condition stands true.
  pub fn get_condition_value(&self, scope: &PreprocessorScope) -> bool {
    // TODO: scope belongs to the condition stack as it mutates while we parse the file
    match self {
      PreprocessorCondition::Ifdef(n) => scope.is_defined(n),
      PreprocessorCondition::Ifndef(n) => !scope.is_defined(n),
    }
  }
}

/// Preprocessor state with AST cache, macro definitions, etc
pub struct ErlPreprocessStage {
  /// For headers included more than once, parse them and cache here for reinterpretation as needed
  ast_cache: Arc<Mutex<PpAstCache>>,

  file_cache: Arc<Mutex<FileContentsCache>>,

  condition_stack: Vec<PreprocessorCondition>,

  /// Contains preprocessor definitions from config, from command line or from the file. Evolves as
  /// the parser progresses through the file and encounters new preprocessor directives.
  scope: Arc<PreprocessorScope>,
}

impl ErlPreprocessStage {
  // fn ast_from_source_file(source_file: &Arc<SourceFile>) -> ErlResult<Arc<PpAst>> {
  //   let pp_ast = PpAst::from_source_file(source_file)?;
  //   Ok(pp_ast.into())
  // }
  /// Split input file into fragments using preprocessor directives as separators
  pub fn from_source_file(source_file: &Arc<SourceFile>) -> ErlResult<Arc<PpAst>> {
    // let successful_parse = PpParser::parse(Rule::file, &source_file.text)?.next().unwrap();
    // let pp_tree = Arc::new(PpAst::Empty);
    // pp_tree.pp_parse_tokens_to_ast(successful_parse)
    let input = &source_file.text;
    let parse_result = PreprocessorParser::parse_module(input);

    #[cfg(debug_assertions)]
    if parse_result.is_err() {
      println!("NomError: {:?}", parse_result);
    }

    match parse_result.finish() {
      Ok((tail, ast)) => {
        // println!("Parse result AST: «{}»", &forms);

        assert!(tail.trim().is_empty(),
                "Preprocessor: Not all input was consumed by parse.\n
                \tTail: «{}»\n
                \tAst: {}", tail, ast);
        Ok(ast)
      }
      Err(err) => Err(ErlError::from_nom_error(input, err)),
    }
  }

  /// Returns: True if a file was preprocessed
  fn preprocess_file(&mut self, file_name: &Path) -> ErlResult<()> {
    // trust that file exists
    let contents = {
      let file_cache1 = self.file_cache.lock().unwrap();
      file_cache1.all_files
          .get(file_name).unwrap()
          .clone()
    };

    // If cached, try get it, otherwise parse and save
    let ast_tree = {
      let mut ast_cache = self.ast_cache.lock().unwrap();
      match ast_cache.items.get(file_name) {
        Some(ast) => ast.clone(),
        None => {
          // Parse and cache
          let ast = Self::from_source_file(&contents)?;
          // Save to preprocessor AST cache
          ast_cache.items.insert(file_name.to_path_buf(), ast.clone());
          ast
        }
      }
    };

    let pp_ast = self.interpret_pp_ast(&contents, ast_tree)?;

    // TODO: Output preprocessed source as iolist, and stream-process in Erlang parser? to minimize the copying
    let output: String = pp_ast.to_string();

    { // Success: insert new string into preprocessed source cache
      let mut file_cache2 = self.file_cache.lock().unwrap();
      file_cache2.update_source_text(file_name, output);
    }

    // Cleanup
    Ok(())
  }
}

fn interpret_include_directive(source_file: &SourceFile,
                               node: &Arc<PpAst>,
                               ast_cache: Arc<Mutex<PpAstCache>>,
                               file_cache: Arc<Mutex<FileContentsCache>>) -> ErlResult<Arc<PpAst>> {
  match node.deref() {
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
          let ast_tree = ErlPreprocessStage::from_source_file(&include_source_file).unwrap();

          // let mut ast_cache1 = ast_cache.lock().unwrap();
          ast_cache1.items.insert(include_path.clone(), ast_tree.clone());

          Ok(PpAst::new_included_file(&include_path, ast_tree))
        }
        Some(arc_ast) => {
          let result = PpAst::new_included_file(&include_path, arc_ast.clone());
          Ok(result)
        }
      }
    }
    _ => Ok(node.clone())
  }
}

impl ErlPreprocessStage {
  /// This is called for each Preprocessor AST node to make the final decision whether the node
  /// is passed into the output or replaced with a SKIP.
  fn interpret_pp_rule_map(&mut self, node: &Arc<PpAst>,
                           source_file: &SourceFile) -> Option<Arc<PpAst>> {
    // First process ifdef/if!def/else/endif
    match node.deref() {
      PpAst::Ifdef(symbol) => {
        self.condition_stack.push(PreprocessorCondition::Ifdef(symbol.clone()));
        return None;
      }

      PpAst::Ifndef(symbol) => {
        self.condition_stack.push(PreprocessorCondition::Ifndef(symbol.clone()));
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
    if let Some(last) = self.condition_stack.last() {
      if !last.get_condition_value(&self.scope) {
        return None;
      }
    }
    // if !self.condition_stack.is_empty() &&
    //     !self.condition_stack.last().unwrap().get_condition_value(&self.scope) {
    //   return None;
    // }

    // Then copy or modify remaining AST nodes
    match node.deref() {
      PpAst::Define(symbol, value) => {
        println!("define {} = {}", symbol, value);
        self.scope = self.scope.define(symbol, None, Some(value.clone()));
        return None;
      }

      PpAst::Undef(symbol) => {
        self.scope.undefine(symbol);
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

      PpAst::IncludedFile { nested: include_ast_tree, .. } => {
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

  /// Create a preprocess state struct for processing a file.
  /// Preprocessor symbols are filled from the command line and project TOML file settings.
  pub fn new(ast_cache: &Arc<Mutex<PpAstCache>>,
             file_cache: &Arc<Mutex<FileContentsCache>>,
             scope: Arc<PreprocessorScope>) -> Self {
    Self {
      ast_cache: ast_cache.clone(),
      file_cache: file_cache.clone(),
      condition_stack: Vec::with_capacity(Self::DEFAULT_CONDITION_STACK_SIZE),
      scope,
    }
  }

  /// Interpret parsed attributes/preprocess directives from top to bottom
  /// - Exclude ifdef/if/ifndef sections where the condition check fails
  /// - Load include files and paste them where include directive was found. Continue interpretation.
  /// - Substitute macros.
  ///
  /// Return: a new preprocessed string joined together.
  fn interpret_pp_ast(&mut self,
                      source_file: &SourceFile,
                      ast_tree: Arc<PpAst>) -> ErlResult<Arc<PpAst>> {
    // From top to down interpret the preprocessed AST. Includes will restart the intepretation
    // from the pasted included AST.
    if let PpAst::File(nodes) = ast_tree.deref() {
      let interpreted: Vec<Arc<PpAst>> = nodes.iter()
          .filter_map(|node| {
            self.interpret_pp_rule_map(node, source_file)
          })
          .collect();
      return Ok(PpAst::File(interpreted).into());
    }
    let err_s =
        format!("Preprocessor parse did not return a root AST node, got something else: {:?}",
                ast_tree);
    Err(ErlError::PreprocessorParse {
      loc: ErrorLocation::new(Some(source_file.file_name.clone()),
                              SourceLoc::None),
      msg: err_s,
    })
  }

  /// Stage 1 - Preprocessor stage
  /// ----------------------------
  /// * Preparse loaded ERL files ignoring the syntax only paying attention to preprocess tokens.
  /// * Preparse include files AST and paste preprocess AST into include locations.
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

    let erl_files: Vec<PathBuf> = all_files.into_iter()
        .filter(|path| path.to_string_lossy().ends_with(".erl"))
        .collect();

    for path in erl_files.iter() {
      // For all input files, run preprocess parse and interpred the preprocess directives
      // Loaded and parsed HRL files are cached to be inserted into every include location
      // Create starting scope (from project settings and command line)
      let starting_scope = project.get_preprocessor_scope(&path);
      let mut stage = ErlPreprocessStage::new(&ast_cache, &file_cache, starting_scope);
      stage.preprocess_file(&path)?;
      preprocessed_count += 1;
    }

    let cached_ast_trees_count = {
      let ast_cache_r = ast_cache.lock().unwrap();
      ast_cache_r.items.len()
    };

    println!("Preprocessed {} sources, {} includes",
             preprocessed_count,
             cached_ast_trees_count);
    Ok(ast_cache)
  }
}
