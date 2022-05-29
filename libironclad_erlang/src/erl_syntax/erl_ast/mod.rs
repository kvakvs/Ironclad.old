//! AST syntax structure of an Erlang file
use crate::erl_syntax::erl_ast::ErlAstType::{FnDef, ModuleForms};
use crate::erl_syntax::erl_error::ErlError;
use crate::erl_syntax::erl_op::ErlBinaryOp;
use crate::erl_syntax::node::erl_apply::ErlApply;
use crate::erl_syntax::node::erl_binary_element::BinaryElement;
use crate::erl_syntax::node::erl_binop::ErlBinaryOperatorExpr;
use crate::erl_syntax::node::erl_case_clause::ErlCaseClause;
use crate::erl_syntax::node::erl_catch_clause::CatchClause;
use crate::erl_syntax::node::erl_fn_def::ErlFnDef;
use crate::erl_syntax::node::erl_if_clause::ErlIfClause;
use crate::erl_syntax::node::erl_record::RecordField;
use crate::erl_syntax::node::erl_token::ErlToken;
use crate::erl_syntax::node::erl_unop::ErlUnaryOperatorExpr;
use crate::erl_syntax::node::erl_var::ErlVar;
use crate::literal::Literal;
use crate::typing::erl_type::ErlType;
use crate::typing::type_error::TypeError;
use libironclad_error::ic_error::IcResult;
use libironclad_error::source_loc::SourceLoc;
use libironclad_util::mfarity::MFArity;
use std::ops::Deref;
use std::sync::Arc;

pub mod ast_as;
pub mod ast_expr;
pub mod ast_extract_var;
pub mod ast_is;
pub mod ast_iter;
pub mod ast_new;
pub mod ast_print;

/// AST node in parsed Erlang source
#[derive(Debug)]
pub struct ErlAst {
  /// Source file pointer
  pub location: SourceLoc,
  /// Node type and optional content
  pub content: ErlAstType,
}

/// Type for an Erlang AST node
#[derive(Debug)]
pub enum ErlAstType {
  /// Default value for when AST tree is empty. Should create error, not a valid AST node.
  Empty,

  /// A token to be consumed by AST builder, temporary, must not exist in final AST
  Token {
    /// The token enum
    token: ErlToken,
  },

  /// Forms list, root of a module
  ModuleForms(Vec<Arc<ErlAst>>),

  /// -module(name). attribute, defines module start
  ModuleStartAttr {
    /// Module name atom, stored as string
    name: String,
  },

  /// `-export([f/a, ......]).` attribute, defines exports
  ExportAttr {
    /// List of funarities (MFAs with module=None)
    exports: Vec<MFArity>,
  },

  /// `-export_type([f/a, ......]).` attribute, defines exports for types
  ExportTypeAttr {
    /// List of typename/arities (MFAs with module=None)
    exports: Vec<MFArity>,
  },

  /// `-type ATOM(ARG, ...) :: TYPE` attribute, defines a new type
  TypeAttr {
    /// Type name
    name: String,
    /// List of type variables
    vars: Vec<String>,
    /// The defined new type
    ty: Arc<ErlType>,
  },

  /// `-import([f/a, ......]).` attribute, defines imports
  ImportAttr {
    /// Module to import from
    import_from: String,
    /// List of funarities to import (MFAs with module=None)
    imports: Vec<MFArity>,
  },

  /// A generic attribute `"-name" ... "."\n` extracted from the source, parsed at a later stage
  GenericAttr {
    /// Attr name atom, stored as string
    tag: String,
    /// Attr value (any expression)
    term: Option<Arc<ErlAst>>,
  },

  /// Defines a new function, with clauses, or an inline lambda (then name will be `None`).
  /// Each clause has same quantity of args (some AST nodes), bindable expressions,
  /// and a return type, initially Any.
  FnDef(ErlFnDef),

  /// Points to a function, replaced from apply calls and fun exprs, where we know that the
  /// expression must be callable
  FnRef {
    /// Function name
    mfa: MFArity,
  },

  /// A function spec, written as `-spec myfun(...) -> <ret type> when ... <optional when>.`
  FnSpec {
    /// The function name and arity, module as None
    funarity: MFArity,
    /// Type for all function clauses
    spec: Arc<ErlType>,
  },

  /// A temporary node wrapper for parsed types. TODO: Use more extensively in typespecs and maybe in the augmented syntax?
  Type {
    /// The type
    ty: Arc<ErlType>,
  },

  /// Case clause for a `case x of` switch
  CClause(SourceLoc, ErlCaseClause),

  /// Name/arity which refers to a function in the current module
  MFA {
    /// fun/arity in the current module, or full mod:fun/arity if external
    mfarity: MFArity,
  },

  /// A named variable
  Var(ErlVar),

  /// Apply arguments to expression
  Apply(ErlApply),

  /// Case switch containing the argument to check, and case clauses
  CaseStatement {
    /// Argument X in `case X of`
    expr: Arc<ErlAst>,
    /// All case clauses in order
    clauses: Vec<ErlCaseClause>,
  },

  /// A literal value, constant. Type is known via literal.get_type()
  Lit {
    /// The literal value
    value: Arc<Literal>,
  },

  /// Binary operation with two arguments
  BinaryOp {
    /// The contained ironclad_exe A&B expression
    expr: ErlBinaryOperatorExpr,
  },

  /// Unary operation with 1 argument
  UnaryOp {
    /// The contained unary &B expression
    expr: ErlUnaryOperatorExpr,
  },

  /// A list of some expressions, TODO: constant folding convert into ErlAst::Lit(ErlLit::List())
  List {
    /// List elements
    elements: Vec<Arc<ErlAst>>,
    /// Optional tail element if not NIL
    tail: Option<Arc<ErlAst>>,
  },

  /// A tuple of some expressions, TODO: constant folding
  Tuple {
    /// Tuple elements
    elements: Vec<Arc<ErlAst>>,
  },

  /// A map of some keys and some values
  Map {
    /// Map keys, matching values by index
    keys: Vec<Arc<ErlAst>>,
    /// Map values, matching keys by index
    values: Vec<Arc<ErlAst>>,
  },

  /// Comma-separated list of expressions, final expression is the result
  CommaExpr {
    /// Comma-expression elements
    elements: Vec<Arc<ErlAst>>,
  },

  /// A list comprehension expression
  ListComprehension {
    /// The result expression
    expr: Arc<ErlAst>,
    /// The generators which produce the list comprehension inpits, and the conditions
    generators: Vec<Arc<ErlAst>>,
  },

  /// A list comprehension generator expression `Expr <- Expr`
  ListComprehensionGenerator {
    /// The output match expression
    left: Arc<ErlAst>,
    /// The input expression (source of the values)
    right: Arc<ErlAst>,
  },

  /// Try/Catch block with optional OF... branches and multiple catch clauses
  TryCatch {
    /// The expression to be tried
    body: Arc<ErlAst>,
    /// Optional `try ... of Pattern -> ...` clauses
    of_branches: Option<Vec<ErlCaseClause>>,
    /// One or more `catch Class:Exc:Stack -> ...` clauses
    catch_clauses: Vec<CatchClause>,
  },

  /// IF statement: `if COND -> EXPR; ... end`
  IfStatement {
    /// The branches to be tried till `true` is found
    clauses: Vec<ErlIfClause>,
  },

  /// A list of ironclad_exe elements constructing a ironclad_exe value, or serving as a ironclad_exe match expression
  BinaryExpr {
    /// Comma separated elements of a ironclad_exe, with `:bit-widths` and `/type-specs`
    elements: Vec<BinaryElement>,
  },
  /// A new record definition, created by `-record(name, {fields,...}).` attribute
  RecordDefinition {
    /// Record tag
    tag: String,
    /// Fields with optional initializers and optional type ascriptions
    fields: Vec<RecordField>,
  },
}

impl ErlAst {
  /// Returns true for ErlAst::Var
  pub fn is_var(&self) -> bool {
    matches!(&self.content, ErlAstType::Var(..))
  }

  // /// Swaps a value and Empty AST, returns the taken value
  // pub fn take(from: &mut ErlAst) -> ErlAst {
  //   let mut swap_in = ErlAst::Empty;
  //   std::mem::swap(from, &mut swap_in);
  //   swap_in
  // }

  /// Create a new temporary token, which holds a place temporarily, it must be consumed in the
  /// same function and not exposed to the rest of the program.
  pub fn temporary_token(t: ErlToken) -> Arc<ErlAst> {
    ErlAst {
      location: SourceLoc::None,
      content: ErlAstType::Token { token: t },
    }
    .into()
  }

  /// Retrieve Some(atom text) if AST node is atom
  pub fn get_atom_text(&self) -> Option<String> {
    match &self.content {
      ErlAstType::Lit { value: lit, .. } => {
        if let Literal::Atom(s) = lit.deref() {
          Some(s.clone())
        } else {
          None
        }
      }
      _ => None,
    }
  }

  #[deprecated = "Not used since CoreAST is gone"]
  /// Given a comma operator, unroll the comma nested tree into a vector of AST, this is used for
  /// function calls, where args are parsed as a single Comma{} and must be converted to a vec.
  /// A non-comma AST-node becomes a single result element.
  pub fn comma_to_vec(comma_ast: &Arc<ErlAst>, dst: &mut Vec<Arc<ErlAst>>) {
    match &comma_ast.content {
      ErlAstType::BinaryOp { expr: binexpr, .. } if binexpr.operator == ErlBinaryOp::Comma => {
        Self::comma_to_vec(&binexpr.left, dst);
        Self::comma_to_vec(&binexpr.right, dst);
      }
      _ => dst.push(comma_ast.clone()),
    }
  }

  /// Scan forms and find a module definition AST node. For finding a function by funarity, check
  /// function registry `ErlModule::env`
  pub fn find_function_def(this: &Arc<ErlAst>, funarity: &MFArity) -> IcResult<Arc<ErlAst>> {
    match &this.content {
      FnDef(erl_fndef) if *funarity == erl_fndef.funarity => {
        return Ok(this.clone());
      }
      ModuleForms(forms) => {
        // Find first in forms for which `find_function_def` returns something
        let find_result = forms
          .iter()
          .find(|&each_fndef| ErlAst::find_function_def(each_fndef, funarity).is_ok())
          .cloned();
        if let Some(fr) = find_result {
          return Ok(fr);
        }
      }
      _ => {}
    }
    ErlError::type_error(&this.location, TypeError::FunctionNotFound { mfa: funarity.clone() })
  }
}

// / A tree of Erlang nodes with attached file name, and root element removed
// pub type ErlAstTree = AstTree<ErlAst>;

// /// A cache of trees of Erlang nodes, keyed by filename or module name
// pub type ErlAstCache = AstCache<ErlAst>;
//
// impl Default for ErlAst {
//   fn default() -> Self {
//     ErlAst::Empty
//   }
// }
