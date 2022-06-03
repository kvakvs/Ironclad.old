#![cfg(feature = "separate_preprocessor_lib")]
//! Preprocessor state for one file

use crate::project::source_file::SourceFile;
use crate::project::ErlProject;
use crate::stage::file_contents_cache::FileContentsCache;
use crate::stage::preprocess::pp_scope::PreprocessorScope;
use crate::stats::cache_stats::CacheStats;
use crate::stats::io_stats::IOStats;
use crate::stats::preprocessor_stats::PreprocessorStats;
use libironclad_erlang::erl_syntax::literal_bool::LiteralBool;
use libironclad_erlang::erl_syntax::parsers::misc::panicking_parser_error_reporter;
use libironclad_error::ic_error::{IcResult, IroncladError};
use libironclad_error::ic_error_trait::IcError;
use libironclad_error::source_loc::SourceLoc;
use nom::Finish;
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};

/// Preprocessor state for 1 file with AST cache, macro definitions, etc
pub struct PreprocessFile {
  /// For headers included more than once, parse them and cache here for reinterpretation as needed
  ast_cache: Arc<RwLock<PpAstCache>>,

  file_cache: Arc<RwLock<FileContentsCache>>,
  /// Contains preprocessor definitions from config, from command line or from the file. Evolves as
  /// the parser progresses through the file and encounters new preprocessor directives.
  scope: Arc<PreprocessorScope>,
}

impl PreprocessFile {
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

  /// Create a self-contained preprocessor stage used for testing, does not take any inputs and
  /// builds everything for itself
  pub fn new_self_contained() -> Self {
    let ast_cache = Arc::new(RwLock::new(PpAstCache::default()));
    let file_cache = Arc::new(RwLock::new(FileContentsCache::default()));
    let scope = PreprocessorScope::default().into();
    Self::new(&ast_cache, &file_cache, scope)
  }

  /// Check AST cache if the file is already parsed, and then return the cached ppAst
  /// Otherwise pass the control to the parser
  pub fn parse_file_helper<Parser>(
    &self,
    ast_cache_stats: &mut CacheStats,
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
        ast_cache_stats.hits += 1;
        return Ok(maybe_cache_hit1.clone());
      } else {
        ast_cache_stats.misses += 1;
      }
    }

    let result = self.parse_helper(&input_file.text, parser);

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
  pub fn preprocess_file(
    &mut self,
    stats: &mut PreprocessorStats,
    project: &ErlProject,
    file_name: &Path,
  ) -> IcResult<()> {
    let contents = {
      if let Ok(file_cache1) = self.file_cache.read() {
        stats.file_cache.hits += 1;
        file_cache1.all_files.get(file_name).unwrap().clone()
      } else {
        stats.file_cache.misses += 1;
        panic!("Can't lock file cache for reading")
      }
    };

    // If cached, try get it, otherwise parse and save
    let ast_tree =
      self.parse_file_helper(&mut stats.ast_cache, &contents, PreprocessorParser::module)?;

    let pp_ast = self.interpret_preprocessor_ast(project, stats, &contents, &ast_tree)?;

    // Render to string
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

  fn load_include(
    &mut self,
    io_stats: &mut IOStats,
    file_cache_stats: &mut CacheStats,
    location: &SourceLoc,
    file: &Path,
  ) -> IcResult<Arc<SourceFile>> {
    // Check if already loaded in the File Cache?
    if let Ok(mut cache) = self.file_cache.write() {
      return cache
        .get_or_load(file_cache_stats, io_stats, file)
        .map_err(IcError::from);
    }
    IroncladError::file_not_found(location, file, "loading an include file")
  }

  fn find_include(
    &mut self,
    project: &ErlProject,
    location: &SourceLoc,
    path: &Path,
  ) -> IcResult<PathBuf> {
    for inc_path in &project.inputs.input_opts.include_paths {
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
    stats: &mut PreprocessorStats,
    node: &Arc<PpAst>,
    source_file: &Arc<SourceFile>,
    nodes_out: &mut Vec<Arc<PpAst>>,
    warnings_out: &mut Vec<IcError>,
    errors_out: &mut Vec<IcError>,
  ) -> IcResult<()> {
    // First process ifdef/if!def/else/endif
    match &node.node_type {
      PpAstType::File(nodes) => {
        for n in nodes {
          self.interpret_preprocessor_node(
            project,
            stats,
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
        let incl_file =
          self.load_include(&mut stats.io, &mut stats.file_cache, &node.location, &found_path)?;
        let node =
          self.parse_file_helper(&mut stats.ast_cache, &incl_file, PreprocessorParser::module)?;
        self.interpret_preprocessor_node(
          project,
          stats,
          &node,
          &incl_file,
          nodes_out,
          warnings_out,
          errors_out,
        )?;
      }
      IncludeLib(arg) => {
        let found_path = self.find_include_lib(project, &node.location, Path::new(arg))?;
        let incl_file =
          self.load_include(&mut stats.io, &mut stats.file_cache, &node.location, &found_path)?;
        let node =
          self.parse_file_helper(&mut stats.ast_cache, &incl_file, PreprocessorParser::module)?;
        self.interpret_preprocessor_node(
          project,
          stats,
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
          stats,
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
      PpAstType::Text(_) => nodes_out.push(node.clone()),
      PpAstType::EmptyText => {} // skip
      Define { name, args, body } => {
        self.scope = self.scope.define(name, args, body);
      }
      // PpAst::DefineFun { name, args, body } => {
      //   self.scope = self
      //     .scope
      //     .define(name, Some(args.clone()), Some(body.clone()));
      // }
      Undef(_) => {}
      PpAstType::Error(msg) => {
        errors_out.push(PpError::new_error_directive(&node.location, msg.clone()));
      }
      Warning(_) => {}
      _ => {}
    }

    Ok(())
  }

  /// Interpret parsed attributes/preprocess directives from top to bottom
  /// - Exclude ifdef/if/ifndef sections where the condition check fails
  /// - Load include files and paste them where include directive was found. Continue interpretation.
  /// - Substitute macros.
  ///
  /// Return: a new preprocessed string joined together.
  fn interpret_preprocessor_ast(
    &mut self,
    project: &ErlProject,
    stats: &mut PreprocessorStats,
    source_file: &Arc<SourceFile>,
    ast_tree: &Arc<PpAst>,
  ) -> IcResult<Arc<PpAst>> {
    let mut nodes_out: Vec<Arc<PpAst>> = Vec::default();
    let mut warnings_out: Vec<IcError> = Vec::default();
    let mut errors_out: Vec<IcError> = Vec::default();

    self.interpret_preprocessor_node(
      project,
      stats,
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
      let included_ast =
        PpAst::construct_with_location(&ast_tree.location, PpAstType::File(nodes_out));
      Ok(included_ast)
    }
  }
}
