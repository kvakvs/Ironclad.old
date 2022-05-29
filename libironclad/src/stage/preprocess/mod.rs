//! Preprocess Stage - parses and interprets the Erlang source and gets rid of -if/-ifdef/-ifndef
//! directives, substitutes HRL files contents in place of -include/-include_lib etc.
pub mod pp_define;
pub mod pp_scope;

use libironclad_erlang::erl_syntax::literal_bool::LiteralBool;
use libironclad_erlang::erl_syntax::parsers::misc::panicking_parser_error_reporter;
use libironclad_error::ic_error::{IcResult, IroncladError};
use libironclad_error::ic_error_trait::IcError;
use libironclad_error::source_loc::SourceLoc;
use libironclad_preprocessor::parsers::pp_parse_types::{PpAstParserResult, PreprocessorParser};
use libironclad_preprocessor::pp_error::PpError;
use libironclad_preprocessor::preprocessor_syntax::pp_ast::PpAstType::{
  Define, EmptyText, Error, File, IfBlock, IfdefBlock, Include, IncludeLib, IncludedFile, Text,
  Undef, Warning,
};
use libironclad_preprocessor::preprocessor_syntax::pp_ast::{PpAst, PpAstCache};
use nom::Finish;
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
  // /// Split input file into fragments using preprocessor directives as separators
  // pub fn from_source_file(source_file: &Arc<SourceFile>) -> IcResult<Arc<PpAst>> {
  //   let input = &source_file.text;
  //   Self::from_source(input)
  // }

  // /// Split input file into fragments using preprocessor directives as separators
  // pub fn from_source(input: &str) -> IcResult<Arc<PpAst>> {
  //   Self::parse_helper(input, PreprocessorParser::parse_module)
  // }

  /// Check AST cache if the file is already parsed, and then return the cached ppAst
  /// Otherwise pass the control to the parser
  pub fn parse_file_helper<Parser>(
    &self,
    input_file: &SourceFile,
    parser: Parser,
  ) -> IcResult<Arc<PpAst>>
  where
    Parser: Fn(&str) -> PpAstParserResult,
  {
    // Check if already parsed in AST cache
    if let Ok(ast_cache) = self.ast_cache.read() {
      let maybe_cache_hit = ast_cache.items.get(&input_file.file_name);
      if let Some(maybe_cache_hit1) = maybe_cache_hit {
        // Found already parsed
        return Ok(maybe_cache_hit1.clone());
      }
    }

    let result = self.parse_helper(&input_file.text, parser);
    // let result = Self::from_source_file(&contents)?;

    if let Ok(result_ok) = &result {
      if let Ok(mut ast_cache) = self.ast_cache.write() {
        // Save to preprocessor AST cache
        ast_cache
          .items
          .insert(input_file.file_name.clone(), result_ok.clone());
      }
    }

    result
  }

  /// Parse AST using provided parser function, check that input is consumed, print some info.
  /// The parser function must take `&str` and return `Arc<PpAst>` wrapped in a `ParserResult`
  pub fn parse_helper<Parser>(&self, input: &str, parser: Parser) -> IcResult<Arc<PpAst>>
  where
    Parser: Fn(&str) -> PpAstParserResult,
  {
    let (tail, ast) = panicking_parser_error_reporter(input, parser(input).finish());
    assert!(
      tail.trim().is_empty(),
      "Preprocessor: Not all input was consumed by parse.\n\tTail: «{}»\n\tAst: {}",
      tail,
      ast
    );
    Ok(ast)
    //   Err(err) => PpError::from_nom_error(input, err),
  }

  /// Returns: True if a file was preprocessed
  fn preprocess_file(&mut self, project: &ErlProject, file_name: &Path) -> IcResult<()> {
    // trust that file exists
    let contents = {
      let file_cache1 = self.file_cache.read().unwrap();
      file_cache1.all_files.get(file_name).unwrap().clone()
    };

    // If cached, try get it, otherwise parse and save
    let ast_tree = self.parse_file_helper(&contents, PreprocessorParser::parse_module)?;
    let pp_ast = self.interpret_pp_ast(project, &contents, &ast_tree)?;

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

  fn interpret_include_directive(
    &self,
    source_file: &SourceFile,
    node: &Arc<PpAst>,
    ast_cache: Arc<RwLock<PpAstCache>>,
    file_cache: Arc<RwLock<FileContentsCache>>,
  ) -> IcResult<Arc<PpAst>> {
    match &node.node_type {
      // Found an attr directive which is -include("something")
      // TODO: Refactor into a outside function with error handling
      IncludeLib(path) | Include(path) => {
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
            let ast_tree =
              self.parse_file_helper(&include_source_file, PreprocessorParser::parse_module)?;

            // let mut ast_cache1 = ast_cache.lock().unwrap();
            ast_cache1
              .items
              .insert(include_path.clone(), ast_tree.clone());

            Ok(PpAst::new_included_file(&node.location, &include_path, ast_tree))
          }
          Some(arc_ast) => {
            let result = PpAst::new_included_file(&node.location, &include_path, arc_ast.clone());
            Ok(result)
          }
        }
      }
      _ => Ok(node.clone()),
    }
  }
}

impl PreprocessState {
  fn load_include(&mut self, location: &SourceLoc, file: &Path) -> IcResult<Arc<SourceFile>> {
    // Check if already loaded in the File Cache?
    if let Ok(mut cache) = self.file_cache.write() {
      return cache.get_or_load(file).map_err(IcError::from);
    }
    IroncladError::file_not_found(location, file, "loading an include file")
  }

  fn find_include(
    &mut self,
    project: &ErlProject,
    location: &SourceLoc,
    path: &Path,
  ) -> IcResult<PathBuf> {
    for inc_path in &project.input_opts.include_paths {
      let try_path = Path::new(&inc_path).join(path);
      if try_path.exists() {
        return Ok(try_path);
      }
    }
    IroncladError::file_not_found(location, path, "searching for an -include() path")
  }

  /// `include_lib` is similar to `include`, but should not point out an absolute file. Instead,
  /// the first path component (possibly after variable substitution) is assumed to be the name
  /// of an application.
  ///
  /// Example:
  ///     -include_lib("kernel/include/file.hrl").
  ///
  /// The code server uses `code:lib_dir(kernel)` to find the directory of the current (latest)
  /// version of Kernel, and then the subdirectory include is searched for the file `file.hrl`.
  fn find_include_lib(
    &mut self,
    _project: &ErlProject,
    location: &SourceLoc,
    path: &Path,
  ) -> IcResult<PathBuf> {
    IroncladError::file_not_found(location, path, "searching for an -include_lib() path")
  }

  /// This is called for each Preprocessor AST node to make the final decision whether the node
  /// is passed into the output or replaced with a SKIP. "Scope" is global for module and as the
  /// interpretation goes top to bottom, the scope is updated globally and is not nested inside
  /// ifdef/if blocks.
  fn interpret_preprocessor_node(
    &mut self,
    project: &ErlProject,
    node: &Arc<PpAst>,
    source_file: &Arc<SourceFile>,
    nodes_out: &mut Vec<Arc<PpAst>>,
    warnings_out: &mut Vec<IcError>,
    errors_out: &mut Vec<IcError>,
  ) -> IcResult<()> {
    // First process ifdef/if!def/else/endif
    match &node.node_type {
      File(nodes) => {
        for n in nodes {
          self.interpret_preprocessor_node(
            project,
            n,
            source_file,
            nodes_out,
            warnings_out,
            errors_out,
          )?;
        }
      }
      Include(arg) => {
        let found_path = self.find_include(project, &node.location, Path::new(arg))?;
        let incl_file = self.load_include(&node.location, &found_path)?;
        let node = self.parse_file_helper(&incl_file, PreprocessorParser::parse_module)?;
        self.interpret_preprocessor_node(
          project,
          &node,
          &incl_file,
          nodes_out,
          warnings_out,
          errors_out,
        )?;
      }
      IncludeLib(arg) => {
        let found_path = self.find_include_lib(project, &node.location, Path::new(arg))?;
        let incl_file = self.load_include(&node.location, &found_path)?;
        let node = self.parse_file_helper(&incl_file, PreprocessorParser::parse_module)?;
        self.interpret_preprocessor_node(
          project,
          &node,
          &incl_file,
          nodes_out,
          warnings_out,
          errors_out,
        )?;
      }
      IncludedFile { ast, .. } => {
        self.interpret_preprocessor_node(
          project,
          ast,
          source_file,
          nodes_out,
          warnings_out,
          errors_out,
        )?;
      }
      IfdefBlock { macro_name, cond_true, cond_false } => {
        if self.scope.is_defined(macro_name) {
          nodes_out.extend(cond_true.iter().cloned());
        } else {
          nodes_out.extend(cond_false.iter().cloned());
        }
      }
      IfBlock { cond, cond_true, cond_false } => match cond.walk_boolean_litexpr() {
        LiteralBool::True => nodes_out.extend(cond_true.iter().cloned()),
        LiteralBool::False => nodes_out.extend(cond_false.iter().cloned()),
        LiteralBool::NotABoolean => {
          return PpError::new_if_directive_error(
            &node.location,
            "The expression must resolve to a boolean true or false".to_string(),
          )
        }
      },
      Text(_) => nodes_out.push(node.clone()),
      EmptyText => {} // skip
      Define { name, args, body } => {
        self.scope = self.scope.define(name, args, body);
      }
      // PpAst::DefineFun { name, args, body } => {
      //   self.scope = self
      //     .scope
      //     .define(name, Some(args.clone()), Some(body.clone()));
      // }
      Undef(_) => {}
      Error(msg) => {
        errors_out.push(PpError::new_error_directive(&node.location, msg.clone()));
      }
      Warning(_) => {}
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
    project: &ErlProject,
    source_file: &Arc<SourceFile>,
    ast_tree: &Arc<PpAst>,
  ) -> IcResult<Arc<PpAst>> {
    let mut nodes_out: Vec<Arc<PpAst>> = Vec::default();
    let mut warnings_out: Vec<IcError> = Vec::default();
    let mut errors_out: Vec<IcError> = Vec::default();

    self.interpret_preprocessor_node(
      project,
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
      let included_ast = PpAst::construct_with_location(&ast_tree.location, File(nodes_out));
      Ok(included_ast)
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
      stage.preprocess_file(project, path)?;
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
