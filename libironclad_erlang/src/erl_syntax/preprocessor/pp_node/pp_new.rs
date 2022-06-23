//! Construction routines for Preprocessor AST nodes

use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::node::erl_record::RecordField;
use crate::erl_syntax::preprocessor::pp_node::pp_impl::PreprocessorNodeImpl;
use crate::erl_syntax::preprocessor::pp_node::pp_type::PreprocessorNodeType;
use crate::erl_syntax::preprocessor::pp_node::PreprocessorNode;
use crate::erl_syntax::token_stream::token::Token;
use crate::source_loc::SourceLoc;
use crate::typing::erl_type::ErlType;
use libironclad_util::mfarity::MFArity;
use std::path::{Path, PathBuf};
use std::sync::Arc;

impl PreprocessorNodeImpl {
  // /// Generic constructor no location
  // #[inline]
  // pub(crate) fn new_without_location(content: PreprocessorNodeType) -> PreprocessorNode {
  //   Self { location: SourceLoc::None, content }.into()
  // }

  /// Generic constructor + location
  #[inline]
  pub(crate) fn new_with_location(
    location: SourceLoc,
    content: PreprocessorNodeType,
  ) -> PreprocessorNode {
    Self { location, content }.into()
  }

  /// Create new nested included file AST node
  #[inline]
  #[allow(dead_code)]
  pub(crate) fn new_included_file(
    location: SourceLoc,
    file: &Path,
    tokens: Vec<Token>,
  ) -> PreprocessorNode {
    Self::new_with_location(
      location,
      PreprocessorNodeType::IncludedFile { filename: PathBuf::from(file), tokens },
    )
  }

  /// Create new macro definition
  #[inline]
  pub(crate) fn new_define(
    location: SourceLoc,
    name: String,
    args: Vec<String>,
    body: Vec<Token>,
  ) -> PreprocessorNode {
    Self::new_with_location(location, PreprocessorNodeType::Define { name, args, body })
  }

  // /// Create new macro definition with name only
  // pub(crate) fn new_define_name_only(location: SourceLoc, name: String) -> AstNode {
  //   Self::construct_with_location(
  //     location,
  //     Define {
  //       name,
  //       args: Vec::default(),
  //       body: String::default(),
  //     },
  //   )
  // }

  // /// Create new text fragment
  // pub(crate) fn new_text(location: SourceLoc, text: &str) -> AstNode {
  //   if text.trim().is_empty() {
  //     Self::construct_with_location(location, PreprocessorNodeType::EmptyText)
  //   } else {
  //     Self::construct_with_location(location, PreprocessorNodeType::Text(String::new(text)))
  //   }
  // }

  // /// Creates a new preprocessor IF node
  // #[deprecated = "not used"]
  // pub(crate) fn new_if(
  //   location: SourceLoc,
  //   expr: AstNode,
  //   cond_true: Vec<AstNode>,
  //   cond_false: Vec<AstNode>,
  // ) -> AstNode {
  //   Self::construct_with_location(location, IfBlock { cond: expr, cond_true, cond_false })
  // }

  // /// Create a new `-if()` temporary node.
  // pub(crate) fn new_group_node_temporary(nodes: Vec<AstNode>) -> AstNode {
  //   Self::construct_without_location(Group(nodes))
  // }

  /// Create a new `-TAG(TERM).` generic module attribute.
  pub(crate) fn new_generic_attr(
    location: SourceLoc,
    tag: String,
    term: Option<AstNode>,
  ) -> PreprocessorNode {
    match tag.as_str() {
      "warning" | "error" | "include" | "include_lib" | "define" | "if" | "ifdef" | "ifndef"
      | "else" | "endif" | "undef" => panic!(
        "Trying to create -{}(). attribute as GenericAttr, there is a specific impl for that!",
        tag
      ),
      _ => {
        PreprocessorNodeImpl::new_with_location(location, PreprocessorNodeType::Attr { tag, term })
      }
    }
  }

  /// Create a new `-if()` data node
  #[inline]
  pub(crate) fn new_if(location: SourceLoc, expr: AstNode) -> PreprocessorNode {
    Self::new_with_location(location, PreprocessorNodeType::If(expr))
  }

  /// Create a new `-elif()` data node
  #[inline]
  pub(crate) fn new_elif(location: SourceLoc, expr: AstNode) -> PreprocessorNode {
    Self::new_with_location(location, PreprocessorNodeType::ElseIf(expr))
  }

  /// Create a new `-ifdef()` data node
  #[inline]
  pub(crate) fn new_ifdef(location: SourceLoc, ident: String) -> PreprocessorNode {
    Self::new_with_location(location, PreprocessorNodeType::Ifdef(ident))
  }

  /// Create a new `-ifndef()` data node
  #[inline]
  pub(crate) fn new_ifndef(location: SourceLoc, ident: String) -> PreprocessorNode {
    Self::new_with_location(location, PreprocessorNodeType::Ifndef(ident))
  }

  /// Create a new `-undef(SYMBOL).` data node
  #[inline]
  pub(crate) fn new_undef(location: SourceLoc, ident: String) -> PreprocessorNode {
    Self::new_with_location(location, PreprocessorNodeType::Undef(ident))
  }

  /// Create a new `-include(PATH).` data node
  #[inline]
  pub(crate) fn new_include(location: SourceLoc, p: String) -> PreprocessorNode {
    Self::new_with_location(location, PreprocessorNodeType::Include(p))
  }

  /// Create a new `-include_lib(PATH).` data node
  #[inline]
  pub(crate) fn new_include_lib(location: SourceLoc, p: String) -> PreprocessorNode {
    Self::new_with_location(location, PreprocessorNodeType::IncludeLib(p))
  }

  /// Create a new `-error()` node
  #[inline]
  pub(crate) fn new_error(location: SourceLoc, p: String) -> PreprocessorNode {
    Self::new_with_location(location, PreprocessorNodeType::Error(p))
  }

  /// Create a new `-warning()` node
  #[inline]
  pub(crate) fn new_warning(location: SourceLoc, p: String) -> PreprocessorNode {
    Self::new_with_location(location, PreprocessorNodeType::Warning(p))
  }

  /// Create a new `-export([...]).` module attr.
  #[inline]
  pub(crate) fn new_export_attr(
    location: SourceLoc,
    fun_arities: Vec<MFArity>,
  ) -> PreprocessorNode {
    Self::new_with_location(location, PreprocessorNodeType::Export { fun_arities })
  }

  /// Create a new `-export_type([...]).` module attr.
  #[inline]
  pub(crate) fn new_export_type_attr(
    location: SourceLoc,
    type_arities: Vec<MFArity>,
  ) -> PreprocessorNode {
    Self::new_with_location(location, PreprocessorNodeType::ExportType { type_arities })
  }

  /// Create a new `-type IDENT(ARG1, ...) :: TYPE.` module attr.
  #[inline]
  pub(crate) fn new_type_attr(
    location: SourceLoc,
    name: String,
    vars: Vec<String>,
    ty: Arc<ErlType>,
  ) -> PreprocessorNode {
    Self::new_with_location(location, PreprocessorNodeType::NewType { name, vars, ty })
  }

  /// Create a new `-import(modulename, [...]).` module attr.
  #[inline]
  pub(crate) fn new_import_attr(
    location: SourceLoc,
    module: String,
    fun_arities: Vec<MFArity>,
  ) -> PreprocessorNode {
    Self::new_with_location(location, PreprocessorNodeType::Import { module, fun_arities })
  }

  /// Create a new record definition from a `-record(name, {fields...}).` attribute
  #[inline]
  pub(crate) fn new_record_definition(
    location: SourceLoc,
    tag: String,
    fields: Vec<RecordField>,
  ) -> PreprocessorNode {
    Self::new_with_location(location, PreprocessorNodeType::NewRecord { tag, fields })
  }

  /// Create a new AST node for a function `-spec FN(ARG, ...) -> RETURN.`
  pub(crate) fn new_fn_spec(
    location: SourceLoc,
    funarity: MFArity,
    spec: Arc<ErlType>,
  ) -> PreprocessorNode {
    Self::new_with_location(location, PreprocessorNodeType::FnSpec { funarity, spec })
  }

  /// Create a new module start node
  pub(crate) fn new_module_start(location: SourceLoc, module_name: String) -> PreprocessorNode {
    Self::new_with_location(location, PreprocessorNodeType::ModuleName { name: module_name })
  }
}
