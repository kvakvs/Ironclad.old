//! Similar to `Scope` but contains root definitions for entire module

use crate::erl_syntax::erl_ast::ast_iter::IterableAstNodeT;
use crate::erl_syntax::erl_ast::node_impl::AstNodeType;
use crate::erl_syntax::erl_ast::AstNode;
use crate::project::module::scope::mod_attr::ModuleAttributes;
use crate::record_def::RecordDefinition;
use crate::typing::erl_type::ErlType;
use libironclad_util::mfarity::MFArity;
use libironclad_util::rw_hashmap::RwHashMap;
use libironclad_util::rw_mfarity_set::RwHashSet;
use std::sync::Arc;

/// Implements module root scope, stuff available directly from the module root
#[derive(Debug)]
pub struct RootScopeImpl {
  /// Contains definitions, added by `-spec` attribute
  pub function_specs: RwHashMap<MFArity, Arc<ErlType>>,
  /// Contains `-type NAME() ...` definitions for new types
  pub user_types: RwHashMap<MFArity, Arc<ErlType>>,
  /// Functions can only be found on the module root scope (but technically can be created in the
  /// other internal scopes too)
  pub function_defs: RwHashMap<MFArity, AstNode>,
  /// Collection of record definitions
  pub record_defs: RwHashMap<String, Arc<RecordDefinition>>,
  /// Collection of all custom attributes coming in form of `- <TAG> ( <EXPR> ).` tag is key in this
  /// collection and not unique.
  pub attributes: RwHashMap<String, Arc<ModuleAttributes>>,
  /// Exported function names and arities
  pub exports: RwHashSet<MFArity>,
  /// Imported function names keyed by the MFArity
  pub imports: RwHashSet<MFArity>,
}

/// Alias type for `Arc<>`
pub type RootScope = Arc<RootScopeImpl>;

impl Default for RootScopeImpl {
  fn default() -> Self {
    RootScopeImpl {
      function_specs: RwHashMap::default(),
      user_types: RwHashMap::default(),
      function_defs: RwHashMap::default(),
      record_defs: RwHashMap::default(),
      attributes: RwHashMap::default(),
      exports: RwHashSet::default(),
      imports: RwHashSet::default(),
    }
  }
}

impl std::fmt::Display for RootScopeImpl {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "RootScope[ function_specs={function_specs}; user_types={user_types}; function_defs={function_defs}; \
      record_defs={record_defs}; attributes={attributes}; exports={exports}; imports={imports} ]",
      function_specs = self.function_specs,
      user_types = self.user_types,
      function_defs = self.function_defs,
      record_defs = self.record_defs,
      attributes = self.attributes,
      exports = self.exports,
      imports = self.imports
    )
  }
}

impl RootScopeImpl {
  /// Add a named attribute
  pub fn add_attr(&self, attr_tag: &str, term: Option<AstNode>) {
    let tag_str = attr_tag.to_string();
    let row = self.attributes.get(&tag_str).unwrap_or_default();
    row.push(tag_str.clone(), term);

    if let Ok(mut w_attrs) = self.attributes.collection.write() {
      w_attrs.insert(tag_str, row);
    } else {
      panic!("Can't lock scope to add an attribute")
    }
  }

  /// Recursive descend into AST saving FnDef nodes
  pub fn update_from_ast(&mut self, ast: &AstNode) {
    if let AstNodeType::FnDef(fndef) = &ast.content {
      self.function_defs.add(fndef.funarity.clone(), ast.clone());
    }

    if let Some(children) = ast.children() {
      for c in children {
        self.update_from_ast(&c)
      }
    }
  }
}
