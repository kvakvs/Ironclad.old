//! Parsing impl for `ErlModule`

use crate::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::misc::panicking_parser_error_reporter;
use crate::erl_syntax::parsers::parse_expr::parse_expr;
use crate::erl_syntax::parsers::parse_fn::parse_fndef;
use crate::erl_syntax::parsers::parse_module;
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::erl_syntax::parsers::token_stream::token::format_tok_stream;
use crate::error::ic_error::IroncladResult;
use crate::project::compiler_opts::CompilerOpts;
use crate::project::module::module_impl::{ErlModule, ErlModuleImpl};
use crate::project::ErlProject;
use libironclad_util::source_file::SourceFile;
use nom::Finish;

impl ErlModuleImpl {
  /// Generic parse helper for any Nom entry point.
  /// Input comes as string in the `SourceFile`, the input is tokenized and then parsed.
  pub fn parse_helper<T>(
    project: &ErlProject,
    src_file: SourceFile,
    parse_fn: T,
    compiler_options: Option<CompilerOpts>,
  ) -> IroncladResult<ErlModule>
  where
    T: Fn(ParserInput) -> ParserResult<AstNode>,
  {
    println!("{}", project);

    let mut module_impl = ErlModuleImpl {
      source_file: src_file.clone(),
      ..ErlModuleImpl::default()
    };
    if let Some(o) = compiler_options {
      module_impl.compiler_options = o;
    }
    let module: ErlModule = module_impl.into();
    let tokens = ErlModuleImpl::tokenize(project, &module, &src_file)?;
    // println!("TOKENS {}", format_tok_stream(&tokens, tokens.len()));

    //----------------------
    // Real parsing begins: tokens to AST
    //----------------------
    let (tail, forms) = if tokens.is_empty() {
      (
        ParserInput::new_slice(module.clone(), &tokens),
        AstNodeImpl::new_module_forms(vec![]),
      )
    } else {
      let tokens_input = ParserInput::new(&src_file, module.clone(), &tokens);
      panicking_parser_error_reporter(
        src_file.text.as_str(),
        tokens_input.clone(),
        parse_fn(tokens_input.clone()).finish(),
        true,
      )
    };

    let trim_tail = tail.tokens.iter().filter(|t| !t.is_eol()).count();
    assert_eq!(
      trim_tail,
      0,
      "Not all input was consumed by parse.\n\tTail: «{}»",
      format_tok_stream(tail.tokens, 50),
    );

    module.ast.replace(forms.clone());

    // Scan AST and find FnDef nodes, update functions knowledge
    module.root_scope.update_from_ast(&forms);

    Ok(module)
  }

  /// Parses code fragment starting with "-module(...)." and containing some function definitions
  /// and the usual module stuff.
  pub fn from_module_source(
    project: &ErlProject,
    source_file: &SourceFile,
    compiler_options: Option<CompilerOpts>,
  ) -> IroncladResult<ErlModule> {
    Self::parse_helper(project, source_file.clone(), parse_module, compiler_options)
  }

  /// Creates a module, where its AST comes from an expression
  pub fn from_expr_source(
    project: &ErlProject,
    source_file: &SourceFile,
    compiler_options: Option<CompilerOpts>,
  ) -> IroncladResult<ErlModule> {
    Self::parse_helper(project, source_file.clone(), parse_expr, compiler_options)
  }

  /// Creates a module, where its AST comes from a function
  pub fn from_fun_source(
    project: &ErlProject,
    source_file: &SourceFile,
    compiler_options: Option<CompilerOpts>,
  ) -> IroncladResult<ErlModule> {
    Self::parse_helper(project, source_file.clone(), parse_fndef, compiler_options)
  }

  // /// Creates a 'module', where its AST comes from a typespec source `-spec myfun(...) -> ...`
  // pub fn from_fun_spec_source(
  //   project: &ErlProject,
  //   source_file: &SourceFile,
  //   compiler_options: Option<CompilerOpts>,
  // ) -> IcResult<ErlModule> {
  //   Self::parse_helper(project, source_file.clone(), fn_spec_attr, compiler_options)
  // }

  /// Creates a 'module', where its AST comes from a type `integer() | 42`
  pub fn from_type_source(
    project: &ErlProject,
    source_file: &SourceFile,
    compiler_options: Option<CompilerOpts>,
  ) -> IroncladResult<ErlModule> {
    use crate::erl_syntax::parsers::parse_type::parse_type_as_ast_node;
    Self::parse_helper(project, source_file.clone(), parse_type_as_ast_node, compiler_options)
  }
}
