//! Contains code for AST node implementation

use crate::erl_syntax::erl_ast::node_impl::AstNodeType::{FnDef, ModuleRoot};
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::erl_error::ErlError;
use crate::erl_syntax::node::erl_apply::ErlApply;
use crate::erl_syntax::node::erl_binary_element::BinaryElement;
use crate::erl_syntax::node::erl_binop::ErlBinaryOperatorExpr;
use crate::erl_syntax::node::erl_case_clause::ErlCaseClause;
use crate::erl_syntax::node::erl_catch_clause::CatchClause;
use crate::erl_syntax::node::erl_fn_def::ErlFnDef;
use crate::erl_syntax::node::erl_if_clause::ErlIfClause;
use crate::erl_syntax::node::erl_map::MapBuilderMember;
use crate::erl_syntax::node::erl_record::RecordField;
use crate::erl_syntax::node::erl_unop::ErlUnaryOperatorExpr;
use crate::erl_syntax::node::erl_var::ErlVar;
use crate::erl_syntax::preprocessor::ast::PreprocessorNodeType;
use crate::error::ic_error::IcResult;
use crate::literal::Literal;
use crate::source_loc::SourceLoc;
use crate::typing::erl_type::ErlType;
use crate::typing::type_error::TypeError;
use libironclad_util::mfarity::MFArity;
use std::ops::Deref;
use std::sync::Arc;

/// AST node in parsed Erlang source
#[derive(Debug)]
pub struct AstNodeImpl {
  /// Source file pointer
  pub location: SourceLoc,
  /// Node type and optional content
  pub content: AstNodeType,
}

/// Type for an Erlang AST node
#[derive(Debug)]
pub enum AstNodeType {
  /// Default value for when AST tree is empty. Should create error, not a valid AST node.
  Empty {
    /// How this `Empty` was created.
    comment: String,
  },

  /// Forms list, root of a module
  ModuleRoot {
    /// Name from `-module(name).` attribute
    name: String,
    /// Vector of module forms
    forms: Vec<AstNode>,
  },

  /// `-export([f/a, ......]).` attribute, defines exports
  ExportAttr {
    /// List of funarities (MFAs with module=None)
    exports: Vec<MFArity>,
  },

  /// `-export_type([f/a, ......]).` attribute, defines exports for types
  ExportTypesAttr {
    /// List of typename/arities (MFAs with module=None)
    exports: Vec<MFArity>,
  },

  /// `-type ATOM(ARG, ...) :: TYPE` attribute, defines a new type
  NewType {
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
    term: Option<AstNode>,
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
    expr: AstNode,
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
    /// The contained binary A&B expression
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
    elements: Vec<AstNode>,
    /// Optional tail element if not NIL
    tail: Option<AstNode>,
  },

  /// A tuple of some expressions, TODO: constant folding
  Tuple {
    /// Tuple elements
    elements: Vec<AstNode>,
  },

  /// A map of some keys and some values, using `=>` syntax for construction
  MapBuilder {
    /// Map keys, matching values by index
    members: Vec<MapBuilderMember>,
  },

  /// Comma-separated list of expressions, final expression is the result
  CommaExpr {
    /// Comma-expression elements
    elements: Vec<AstNode>,
  },

  /// A list comprehension expression
  ListComprehension {
    /// The result expression
    expr: AstNode,
    /// The generators which produce the list comprehension inpits, and the conditions
    generators: Vec<AstNode>,
  },

  /// A list comprehension generator expression `Expr <- Expr`
  ListComprehensionGenerator {
    /// The output match expression
    left: AstNode,
    /// The input expression (source of the values)
    right: AstNode,
  },

  /// Try/Catch block with optional OF... branches and multiple catch clauses
  TryCatch {
    /// The expression to be tried
    body: AstNode,
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

  /// A list of binary elements constructing a binary value, or serving as a binary match expression
  BinaryExpr {
    /// Comma separated elements of a binary, with `:bit-widths` and `/type-specs`
    elements: Vec<BinaryElement>,
  },
  /// A new record definition, created by `-record(name, {fields,...}).` attribute
  RecordDefinition {
    /// Record tag
    tag: String,
    /// Fields with optional initializers and optional type ascriptions
    fields: Vec<RecordField>,
  },

  /// Preprocessor node types grouped
  Preprocessor(PreprocessorNodeType),
}

impl AstNodeImpl {
  /// Returns true for ErlAst::Var
  pub fn is_var(&self) -> bool {
    matches!(&self.content, AstNodeType::Var(..))
  }

  /// Retrieve Some(atom text) if AST node is atom
  #[allow(dead_code)]
  pub(crate) fn get_atom_text(&self) -> Option<String> {
    match &self.content {
      AstNodeType::Lit { value: lit, .. } => {
        if let Literal::Atom(s) = lit.deref() {
          Some(s.clone())
        } else {
          None
        }
      }
      _ => None,
    }
  }

  // #[deprecated = "Not used since CoreAST is gone"]
  // /// Given a comma operator, unroll the comma nested tree into a vector of AST, this is used for
  // /// function calls, where args are parsed as a single Comma{} and must be converted to a vec.
  // /// A non-comma AST-node becomes a single result element.
  // #[allow(dead_code)]
  // pub(crate) fn comma_to_vec(comma_ast: &AstNode, dst: &mut Vec<AstNode>) {
  //   match &comma_ast.content {
  //     AstNodeType::BinaryOp { expr: binexpr, .. } if binexpr.operator == ErlBinaryOp::Comma => {
  //       Self::comma_to_vec(&binexpr.left, dst);
  //       Self::comma_to_vec(&binexpr.right, dst);
  //     }
  //     _ => dst.push(comma_ast.clone()),
  //   }
  // }

  /// Scan forms and find a module definition AST node. For finding a function by funarity, check
  /// function registry `ErlModule::env`
  pub fn find_function_def(this: &AstNode, funarity: &MFArity) -> IcResult<AstNode> {
    match &this.content {
      FnDef(erl_fndef) if *funarity == erl_fndef.funarity => {
        return Ok(this.clone());
      }
      ModuleRoot { forms, .. } => {
        // Find first in forms for which `find_function_def` returns something
        let find_result = forms
          .iter()
          .find(|&each_fndef| AstNodeImpl::find_function_def(each_fndef, funarity).is_ok())
          .cloned();
        if let Some(fr) = find_result {
          return Ok(fr);
        }
      }
      _ => {}
    }
    ErlError::type_error(
      this.location.clone(),
      TypeError::FunctionNotFound { mfa: funarity.clone() },
    )
  }
}
