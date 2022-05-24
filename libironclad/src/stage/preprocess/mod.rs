//! Preprocess Stage - parses and interprets the Erlang source and gets rid of -if/-ifdef/-ifndef
//! directives, substitutes HRL files contents in place of -include/-include_lib etc.
pub mod pp_define;
pub mod pp_scope;

use libironclad_error::ic_error::{IcResult, IroncladError};
use libironclad_error::ic_error_trait::IcError;
use libironclad_preprocessor::nom_parser::pp_parse_types::{PpAstParserResult, PreprocessorParser};
use libironclad_preprocessor::pp_error::PpError;
use libironclad_preprocessor::syntax_tree::pp_ast::{PpAst, PpAstCache};
use nom::Finish;
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};

use crate::project::source_file::SourceFile;
use crate::project::ErlProject;
use crate::stage::file_contents_cache::FileContentsCache;
use crate::stage::preprocess::pp_scope::PreprocessorScope;

/// Preprocessor state with AST cache, macro definitions, etc
pub struct PreprocessState {
  /// For headers included more than once, parse them and cache here for reinterpretation as needed
  ast_cache: Arc<RwLock<PpAstCache>>,

  file_cache: Arc<RwLock<FileContentsCache>>,

  /// Contains preprocessor definitions from config, from command line or from the file. Evolves as
  /// the parser progresses through the file and encounters new preprocessor directives.
  scope: Arc<PreprocessorScope>,
}

impl PreprocessState {
  /// Split input file into fragments using preprocessor directives as separators
  pub fn from_source_file(source_file: &Arc<SourceFile>) -> IcResult<Arc<PpAst>> {
    let input = &source_file.text;
    Self::from_source(input)
  }

  /// Split input file into fragments using preprocessor directives as separators
  pub fn from_source(input: &str) -> IcResult<Arc<PpAst>> {
    Self::parse_helper(input, PreprocessorParser::parse_module)
  }

  /// Parse AST using provided parser function, check that input is consumed, print some info.
  /// The parser function must take `&str` and return `Arc<PpAst>` wrapped in a `ParserResult`
  pub fn parse_helper<Parser>(input: &str, parser: Parser) -> IcResult<Arc<PpAst>>
  where
    Parser: Fn(&str) -> PpAstParserResult,
  {
    let parse_result = parser(input).finish();

    #[cfg(debug_assertions)]
    if parse_result.is_err() {
      println!("NomError: {:?}", parse_result);
    }

    match parse_result {
      Ok((tail, ast)) => {
        assert!(
          tail.trim().is_empty(),
          "Preprocessor: Not all input was consumed by parse.\n
                \tTail: «{}»\n
                \tAst: {}",
          tail,
          ast
        );
        Ok(ast)
      }
      Err(err) => PpError::from_nom_error(input, err),
    }
  }

  /// Returns: True if a file was preprocessed
  fn preprocess_file(&mut self, file_name: &Path) -> IcResult<()> {
    // trust that file exists
    let contents = {
      let file_cache1 = self.file_cache.read().unwrap();
      file_cache1.all_files.get(file_name).unwrap().clone()
    };

    // If cached, try get it, otherwise parse and save
    let ast_tree = {
      let mut ast_cache = self.ast_cache.write().unwrap();
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

    let pp_ast = self.interpret_pp_ast(&contents, &ast_tree)?;

    // TODO: Output preprocessed source as iolist, and stream-process in Erlang parser? to minimize the copying
    let output: String = pp_ast.to_string();

    {
      // Success: insert new string into preprocessed source cache
      let mut file_cache2 = self.file_cache.write().unwrap();
      file_cache2.update_source_text(file_name, output);
    }

    // Cleanup
    Ok(())
  }
}

fn interpret_include_directive(
  source_file: &SourceFile,
  node: &Arc<PpAst>,
  ast_cache: Arc<RwLock<PpAstCache>>,
  file_cache: Arc<RwLock<FileContentsCache>>,
) -> IcResult<Arc<PpAst>> {
  match node.deref() {
    // Found an attr directive which is -include("something")
    // TODO: Refactor into a outside function with error handling
    PpAst::IncludeLib(path) | PpAst::Include(path) => {
      // Take source file's parent dir and append to it the include path (unless it was absolute?)
      let source_path = &source_file.file_name;

      let include_path0 = PathBuf::from(path);
      let include_path = if include_path0.is_absolute() {
        include_path0
      } else {
        source_path.parent().unwrap().join(include_path0)
      };

      // TODO: Path resolution relative to the file path
      let mut ast_cache1 = ast_cache.write().unwrap();
      let find_result = ast_cache1.items.get(&include_path);

      match find_result {
        None => {
          let include_source_file = {
            let mut file_cache1 = file_cache.write().unwrap();
            file_cache1.get_or_load(&include_path).unwrap()
          };
          let ast_tree = PreprocessState::from_source_file(&include_source_file).unwrap();

          // let mut ast_cache1 = ast_cache.lock().unwrap();
          ast_cache1
            .items
            .insert(include_path.clone(), ast_tree.clone());

          Ok(PpAst::new_included_file(&include_path, ast_tree))
        }
        Some(arc_ast) => {
          let result = PpAst::new_included_file(&include_path, arc_ast.clone());
          Ok(result)
        }
      }
    }
    _ => Ok(node.clone()),
  }
}

impl PreprocessState {
  fn load_include(&mut self, path: &Path) -> IcResult<(Arc<SourceFile>, Arc<PpAst>)> {
    unimplemented!("load_include not impl: {}", path.to_string_lossy())
  }

  fn find_include(&mut self, path: &str) -> IcResult<PathBuf> {
    // unimplemented!("find_include not impl: {}", path)
    Ok(PathBuf::from(path))
  }

  fn find_include_lib(&mut self, path: &str) -> IcResult<PathBuf> {
    // unimplemented!("find_include_lib not impl: {}", path)
    Ok(PathBuf::from(path))
  }

  /// This is called for each Preprocessor AST node to make the final decision whether the node
  /// is passed into the output or replaced with a SKIP. "Scope" is global for module and as the
  /// interpretation goes top to bottom, the scope is updated globally and is not nested inside
  /// ifdef/if blocks.
  fn interpret_preprocessor_node(
    &mut self,
    node: &Arc<PpAst>,
    source_file: &Arc<SourceFile>,
    nodes_out: &mut Vec<Arc<PpAst>>,
    warnings_out: &mut Vec<IcError>,
    errors_out: &mut Vec<IcError>,
  ) -> IcResult<()> {
    // First process ifdef/if!def/else/endif
    match node.deref() {
      PpAst::File(nodes) => {
        for n in nodes {
          self.interpret_preprocessor_node(n, source_file, nodes_out, warnings_out, errors_out)?;
        }
      }
      PpAst::Include(arg) => {
        let found_path = self.find_include(arg)?;
        let (incl_file, node) = self.load_include(&found_path)?;
        self.interpret_preprocessor_node(&node, &incl_file, nodes_out, warnings_out, errors_out)?;
      }
      PpAst::IncludeLib(arg) => {
        let found_path = self.find_include_lib(arg)?;
        let (incl_file, node) = self.load_include(&found_path)?;
        self.interpret_preprocessor_node(&node, &incl_file, nodes_out, warnings_out, errors_out)?;
      }
      PpAst::IncludedFile { ast, .. } => {
        self.interpret_preprocessor_node(ast, source_file, nodes_out, warnings_out, errors_out)?;
      }
      PpAst::IfdefBlock { macro_name, cond_true, cond_false } => {
        if self.scope.is_defined(macro_name) {
          if let Some(nodes) = cond_true {
            nodes_out.extend(nodes.iter().cloned());
          }
        } else if let Some(nodes) = cond_false {
          nodes_out.extend(nodes.iter().cloned());
        }
      }
      PpAst::Text(_) => nodes_out.push(node.clone()),
      PpAst::EmptyText => {} // skip
      PpAst::Define { name, args, body } => {
        self.scope = self.scope.define(name, args.clone(), body.clone());
      }
      PpAst::DefineFun { name, args, body } => {
        self.scope = self
          .scope
          .define(name, Some(args.clone()), Some(body.clone()));
      }
      PpAst::Undef(_) => {}
      PpAst::IfBlock { .. } => {}
      PpAst::Error(msg) => {
        errors_out.push(PpError::new_error_directive(msg.clone()));
      }
      PpAst::Warning(_) => {}
      _ => {}
    }

    Ok(())
  }

  /// Create a preprocess state struct for processing a file.
  /// Preprocessor symbols are filled from the command line and project TOML file settings.
  pub fn new(
    ast_cache: &Arc<RwLock<PpAstCache>>,
    file_cache: &Arc<RwLock<FileContentsCache>>,
    scope: Arc<PreprocessorScope>,
  ) -> Self {
    Self {
      ast_cache: ast_cache.clone(),
      file_cache: file_cache.clone(),
      scope,
    }
  }

  /// Interpret parsed attributes/preprocess directives from top to bottom
  /// - Exclude ifdef/if/ifndef sections where the condition check fails
  /// - Load include files and paste them where include directive was found. Continue interpretation.
  /// - Substitute macros.
  ///
  /// Return: a new preprocessed string joined together.
  fn interpret_pp_ast(
    &mut self,
    source_file: &Arc<SourceFile>,
    ast_tree: &Arc<PpAst>,
  ) -> IcResult<Arc<PpAst>> {
    let mut nodes_out: Vec<Arc<PpAst>> = Vec::default();
    let mut warnings_out: Vec<IcError> = Vec::default();
    let mut errors_out: Vec<IcError> = Vec::default();

    self.interpret_preprocessor_node(
      ast_tree,
      source_file,
      &mut nodes_out,
      &mut warnings_out,
      &mut errors_out,
    )?;

    if !errors_out.is_empty() {
      Err(IroncladError::multiple(errors_out))
    } else if !warnings_out.is_empty() {
      Err(IroncladError::multiple(warnings_out))
    } else {
      Ok(PpAst::File(nodes_out).into())
    }
  }

  /// Stage 1 - Preprocessor stage
  /// ----------------------------
  /// * Preparse loaded ERL files ignoring the syntax only paying attention to preprocess tokens.
  /// * Preparse include files AST and paste preprocess AST into include locations.
  /// * Drop AST branches covered by the conditional compile directives.
  ///
  /// Side effects: Updates file contents cache
  /// Returns preprocessed collection of module sources
  pub fn run(
    project: &mut ErlProject,
    file_cache: Arc<RwLock<FileContentsCache>>,
  ) -> IcResult<Arc<RwLock<PpAstCache>>> {
    let ast_cache = RwLock::new(PpAstCache::default()).into();

    // Take only .erl files
    let all_files: Vec<PathBuf> = {
      let file_cache_r = file_cache.read().unwrap();
      file_cache_r.all_files.keys().cloned().collect()
    };

    let mut preprocessed_count = 0;

    let erl_files: Vec<PathBuf> = all_files
      .into_iter()
      .filter(|path| path.to_string_lossy().ends_with(".erl"))
      .collect();

    for path in erl_files.iter() {
      // For all input files, run preprocess parse and interpred the preprocess directives
      // Loaded and parsed HRL files are cached to be inserted into every include location
      // Create starting scope (from project settings and command line)
      let starting_scope = project.get_preprocessor_scope(path);
      let mut stage = PreprocessState::new(&ast_cache, &file_cache, starting_scope);
      stage.preprocess_file(path)?;
      preprocessed_count += 1;
    }

    let cached_ast_trees_count = {
      let ast_cache_r = ast_cache.read().unwrap();
      ast_cache_r.items.len()
    };

    println!(
      "Preprocessed {} sources, {} includes",
      preprocessed_count, cached_ast_trees_count
    );
    Ok(ast_cache)
  }
}
