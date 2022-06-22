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
use std::collections::HashMap;
use std::sync::{Arc, RwLock};

/// Implements module root scope, stuff available directly from the module root
#[derive(Debug)]
pub struct RootScopeImpl {
  /// Contains definitions, added by `-spec` attribute
  function_specs: RwHashMap<MFArity, Arc<ErlType>>,
  /// Contains `-type NAME() ...` definitions for new types
  user_types: RwHashMap<MFArity, Arc<ErlType>>,
  /// Functions can only be found on the module root scope (but technically can be created in the
  /// other internal scopes too)
  pub function_defs: RwHashMap<MFArity, AstNode>,
  /// Collection of record definitions
  record_defs: RwHashMap<String, Arc<RecordDefinition>>,
  /// Collection of all custom attributes coming in form of `- <TAG> ( <EXPR> ).` tag is key in this
  /// collection and not unique.
  attributes: RwHashMap<String, Arc<ModuleAttributes>>,
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
  /// Retrieve named attributes
  pub fn get_attr(&self, attr_tag: &str) -> Option<Arc<ModuleAttributes>> {
    if let Ok(r_attrs) = self.attributes.collection.read() {
      r_attrs.get(&attr_tag.to_string()).cloned()
    } else {
      panic!("Can't lock scope to read attributes")
    }
  }

  /// Add a named attribute
  pub fn add_attr(&self, attr_tag: &str, term: Option<AstNode>) {
    let row = self.get_attr(attr_tag).unwrap_or_default();
    row.push(attr_tag.to_string(), term);

    if let Ok(mut w_attrs) = self.attributes.collection.write() {
      w_attrs.insert(attr_tag.to_string(), row);
    } else {
      panic!("Can't lock scope to add an attribute")
    }
  }

  /// Retrieve named function spec
  pub fn get_spec(&self, mfa: &MFArity) -> Option<Arc<ErlType>> {
    if let Ok(r_specs) = self.function_specs.collection.read() {
      r_specs.get(&mfa).cloned()
    } else {
      panic!("Can't lock scope to lookup function specs")
    }
  }

  /// Retrieve a user type (newtype) by name and arity
  pub fn get_user_type(&self, typename_arity: &MFArity) -> Option<Arc<ErlType>> {
    if let Ok(r_types) = self.user_types.collection.read() {
      r_types.get(&typename_arity).cloned()
    } else {
      panic!("Can't lock scope to lookup a user type")
    }
  }

  /// Retrieve a record definition by name
  pub fn get_record_def(&self, tag: &str) -> Option<Arc<RecordDefinition>> {
    if let Ok(r_rdefs) = self.record_defs.collection.read() {
      r_rdefs.get(&tag.to_string()).cloned()
    } else {
      panic!("Can't lock scope to lookup a user type")
    }
  }

  // /// Attempt to find a function in the scope, or delegate to the parent scope
  // pub fn get_fn(&self, mfa: &MFArity) -> Option<AstNode> {
  //   if let Ok(r_funs) = self.function_defs.collection.read() {
  //     match r_funs.get(mfa) {
  //       Some(val) => {
  //         if val.is_fn_def() {
  //           return Some(val.clone());
  //         }
  //         panic!("Only FnDef AST nodes must be stored in module scope")
  //       }
  //       None => None,
  //     }
  //   } else {
  //     panic!("Can't lock RootScope functions for lookup")
  //   }
  // }

  /// Add a function by MFA and its type
  pub(crate) fn add_fn(&self, mfa: &MFArity, ast: AstNode) {
    if let Ok(mut w_funs) = self.function_defs.collection.write() {
      w_funs.insert(mfa.clone(), ast);
    } else {
      panic!("Can't lock scope to update functions")
    }
  }

  /// Recursive descend into AST saving FnDef nodes
  pub fn update_from_ast(&mut self, ast: &AstNode) {
    if let AstNodeType::FnDef(fndef) = &ast.content {
      self.add_fn(&fndef.funarity, ast.clone());
    }

    if let Some(children) = ast.children() {
      for c in children {
        self.update_from_ast(&c)
      }
    }
  }
}
