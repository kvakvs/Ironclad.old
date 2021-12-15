//! Defines AST tree for Core Erlang-like intermediate language. A more generalized and simplified
//! intermediate language, allowing easier optimisations and easier code generation.
use ::function_name::named;
use std::sync::{Arc, RwLock};

use crate::mfarity::MFArity;
use crate::source_loc::SourceLoc;
use crate::core_erlang::syntax_tree::node::core_apply::Apply;
use crate::core_erlang::syntax_tree::node::core_call::Call;
use crate::core_erlang::syntax_tree::node::core_case::Case;
use crate::core_erlang::syntax_tree::node::core_expr::{BinaryOperatorExpr, UnaryOperatorExpr};
use crate::core_erlang::syntax_tree::node::core_fn_def::FnDef;
use crate::core_erlang::syntax_tree::node::core_let_expr::LetExpr;
use crate::core_erlang::syntax_tree::node::core_module_attr::ModuleAttr;
use crate::core_erlang::syntax_tree::node::core_prim_op::{PrimOp, ExceptionType};
use crate::core_erlang::syntax_tree::node::core_var::Var;
use crate::literal::Literal;
use std::ops::Deref;
use crate::display::Pretty;
use crate::erl_error::ErlResult;
use crate::typing::erl_type::ErlType;
use crate::typing::scope::Scope;
use crate::typing::synth::TypeBuilder;

/// AST node in Core Erlang (parsed or generated)
#[derive(Debug)]
pub enum CoreAst {
  /// Default value for AST tree not initialized
  Empty,
  /// Module header with the name and a collection of exported functions
  Module {
    /// Module name atom
    name: String,
    /// List of fun/arity (module always `None`)
    exports: Vec<MFArity>,
  },
  /// Module attributes collection, grouped all together
  Attributes(Vec<ModuleAttr>),
  /// Collection of new function definitions following the module header, exports and attrs
  ModuleFuns(Vec<Arc<CoreAst>>),

  //
  // Execution and branching AST nodes
  //

  /// Define a function with 1 clause (multiple clauses handled by a `case Args of`)
  FnDef(Arc<FnDef>),

  /// Refer to a function by module:fun/arity, if module is not None, this is an external ref.
  FnRef {
    /// The module:fun/arity pointer
    mfa: MFArity,
  },

  /// Branch based on whether an expression matches some conditions
  Case(Case),

  /// Create a scope by assigning 1 or more variables
  Let(LetExpr),

  /// Call an expression which has a function type
  Apply(Apply),

  /// Call a function export mod:fun/arity, or a local fun/arity
  Call(Call),

  /// Primitive operation, such as `raise`
  PrimOp {
    /// Source file pointer
    location: SourceLoc,
    /// The operation to perform
    op: PrimOp,
  },

  //
  // AST nodes carrying values and operations
  //

  /// A variable with optional name and assigned unique numbered typevar
  Var(Arc<Var>),

  /// A literal value (fully known at compile time)
  Lit {
    /// Source file pointer
    location: SourceLoc,
    /// The literal tree
    value: Arc<Literal>,
    // /// This literal's type TODO: Make this lazy
    // lit_type: Arc<ErlType>,
  },

  /// An operator with 2 arguments left and right (also comma operator)
  BinOp {
    /// Source file pointer
    location: SourceLoc,
    /// The left and right operands, and the operation
    op: BinaryOperatorExpr,
  },

  /// A single argument operator such as -, +, not, bnot
  UnOp {
    /// Source file pointer
    location: SourceLoc,
    /// The operand and the operation
    op: UnaryOperatorExpr,
  },

  /// A list of some expressions
  List {
    /// Source code location
    location: SourceLoc,
    /// List elements
    elements: Vec<Arc<CoreAst>>,
    /// Optional tail element if not NIL
    tail: Option<Arc<CoreAst>>,
  },

  /// A tuple of some expressions
  Tuple {
    /// Source code location
    location: SourceLoc,
    /// Tuple elements
    elements: Vec<Arc<CoreAst>>,
  },
}

impl CoreAst {
  /// Creates a new atom literal, and marks its type
  pub fn new_atom(name: &str) -> Self {
    let name_s = name.to_string();
    CoreAst::Lit {
      location: SourceLoc::None,
      value: Arc::new(Literal::Atom(name_s)),
      // lit_type: ErlType::Atom(name_s).into(),
    }
  }

  /// Creates a new function pointer node
  pub fn new_fnref(mfa: MFArity) -> Self {
    CoreAst::FnRef { mfa }
  }

  /// Shortcut to call the typebuilder's synthesize
  pub fn synthesize_type(&self, env: &Arc<RwLock<Scope>>) -> ErlResult<Arc<ErlType>> {
    TypeBuilder::synthesize(env, self)
  }

  // /// Gets the type of an AST node
  // #[named]
  // pub fn get_type(&self) -> Arc<ErlType> {
  //   match self {
  //     CoreAst::FnDef(fn_def) => ErlType::TVar(fn_def.ret_ty).into(),
  //     CoreAst::Var(core_var) => ErlType::TVar(core_var.ty).into(),
  //     CoreAst::Apply(app) => ErlType::TVar(app.ret_ty).into(),
  //     CoreAst::Case(case) => ErlType::TVar(case.ret_ty).into(),
  //     CoreAst::FnRef { fn_type, .. } => ErlType::TVar(*fn_type).into(),
  //     CoreAst::Lit { value: l, .. } => l.get_type(),
  //     CoreAst::BinOp { op, .. } => op.get_result_type(),
  //     CoreAst::UnOp { op, .. } => op.expr.get_type(), // same type as expr bool or num
  //     CoreAst::List { elements, tail, .. } => {
  //       assert!(tail.is_none()); // todo
  //       let union_t = ErlType::union_of(
  //         elements.iter().map(|e| e.get_type()).collect(),
  //         true);
  //       ErlType::List(union_t).into()
  //     }
  //     CoreAst::Tuple { elements, .. } => {
  //       let element_types = elements.iter().map(|e| e.get_type()).collect();
  //       ErlType::Tuple { elements: element_types }.into()
  //     }
  //     // CoreAst::MFA { clause_types, .. } => {
  //     //   let fn_type = FunctionType::new(None, clause_types.clone());
  //     //   ErlType::Fn(fn_type)
  //     // }
  //     CoreAst::PrimOp { op, .. } => {
  //       match op {
  //         PrimOp::Raise { .. } => ErlType::None.into(),
  //         PrimOp::ExcTrace => { panic!("TODO: get_type() for primop exctrace") }
  //       }
  //     }
  //
  //     _ => unreachable!("{}: Can't process {}", function_name!(), self),
  //   }
  // }

  /// Retrieve source file location for an AST element
  pub fn location(&self) -> SourceLoc {
    match self {
      CoreAst::FnDef(fndef) => fndef.location.clone(),
      CoreAst::Var(core_var) => core_var.location.clone(),
      CoreAst::Apply(app) => app.location.clone(),
      CoreAst::Case(case) => case.location.clone(),
      CoreAst::Lit { location, .. } => location.clone(),
      CoreAst::BinOp { location, .. } => location.clone(),
      CoreAst::UnOp { location, .. } => location.clone(),
      _ => SourceLoc::None,
    }
  }

  /// Create a new named variable with unique number id and a small text prefix
  pub fn new_unique_var(prefix: &str) -> Arc<CoreAst> {
    let new_v = Var::new_unique(SourceLoc::None, prefix);
    CoreAst::Var(new_v.into()).into()
  }

  /// Creates a primop raising badarg atom
  // TODO: Expand this to return better error information, see that it is compatible with how OTP erlc does this
  pub fn create_badarg_primop(location: SourceLoc) -> Self {
    CoreAst::PrimOp {
      location,
      op: PrimOp::Raise {
        exc: ExceptionType::Error,
        expr: CoreAst::new_atom("badarg").into(),
      },
    }
  }

  /// Creates a primop raising case clause error
  // TODO: Expand this to return better error information, see that it is compatible with how OTP erlc does this
  pub fn create_caseclause_primop(location: SourceLoc) -> Self {
    CoreAst::PrimOp {
      location,
      op: PrimOp::Raise {
        exc: ExceptionType::Error,
        expr: CoreAst::new_atom("case_clause").into(),
      },
    }
  }

  /// Scan forms and find a module definition AST node. For finding a function by funarity, check
  /// function registry `ErlModule::env`
  pub fn find_function_def(this: &Arc<CoreAst>, funarity: &MFArity) -> Option<Arc<CoreAst>> {
    match this.deref() {
      CoreAst::FnDef(erl_fndef) if *funarity == erl_fndef.funarity => Some(this.clone()),
      CoreAst::ModuleFuns(fndefs) => {
        // Find first in forms for which `find_function_def` returns something
        fndefs.iter()
            .find(|&each_fndef| CoreAst::find_function_def(each_fndef, funarity).is_some())
            .cloned()
      }
      _ => None,
    }
  }
}

impl std::fmt::Display for CoreAst {
  #[named]
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      CoreAst::Module { name, exports } => {
        writeln!(f, "module {} ", name)?;
        write!(f, "exports=")?;
        Pretty::display_square_list(exports, f)
      }
      CoreAst::Attributes(attrs) => {
        write!(f, "attributes=")?;
        Pretty::display_square_list(attrs, f)
      }
      CoreAst::ModuleFuns(fndefs) => {
        for fndef in fndefs.iter() {
          writeln!(f, "{}", fndef)?;
        }
        Ok(())
      }

      CoreAst::FnDef(fn_def) => {
        write!(f, "{} = (", fn_def.funarity)?;
        Pretty::display_semicolon_separated(&fn_def.clauses, f)?;
        write!(f, ")")
      }
      CoreAst::Var(core_var) => write!(f, "{}", core_var.name),
      CoreAst::Apply(app) => write!(f, "{}", app),
      CoreAst::Case(case) => write!(f, "{}", case),
      CoreAst::Lit { value, .. } => write!(f, "{}", value),

      CoreAst::BinOp { op, .. } => {
        write!(f, "({} {} {})", op.left, op.operator, op.right)
      }
      CoreAst::UnOp { op, .. } => {
        write!(f, "({} {})", op.operator, op.expr)
      }
      // CoreAst::MFA { mfarity: mfa, .. } => {
      //   match &mfa.module {
      //     None => write!(f, "(fun {}/{})", mfa.name, mfa.arity),
      //     Some(m) => write!(f, "(fun {}:{}/{})", m, mfa.name, mfa.arity),
      //   }
      // }
      CoreAst::List { elements, .. } => Pretty::display_square_list(elements, f),
      CoreAst::Tuple { elements, .. } => Pretty::display_curly_list(elements, f),
      CoreAst::PrimOp { op, .. } => write!(f, "{:?}", op),
      CoreAst::FnRef { mfa: mfarity, .. } => write!(f, "{}", mfarity),

      CoreAst::Let { .. } => todo!("display(let)"),
      CoreAst::Call { .. } => todo!("display(call)"),
      CoreAst::Empty => write!(f, "<empty ast>"),

      //other => unimplemented!("{}: Don't know how to display {:?}", function_name!(), other),
    }
  }
}

// impl std::fmt::Debug for CoreAst {
//   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//     match self {
//       CoreAst::FnDef(fn_def) => {
//         write!(f, "{}/{} = (fun ", fn_def.funarity.name, fn_def.funarity.arity)?;
//         display::display_paren_list(&fn_def.args, f)?;
//         write!(f, ":{} -> {}", fn_def.ret_ty, fn_def.body)?;
//         write!(f, "}})")
//         // write!(f, "fun {} -> {} {{", fn_def.funarity, fn_def.ret_ty)?;
//         // for fc in fn_def.clauses.iter() { write!(f, "{:?};", fc)?; }
//         // write!(f, "}}")
//       }
//       CoreAst::Var(core_var) => write!(f, "{}:{}", self, core_var.ty),
//       // CoreAst::Apply { app, .. } => write!(f, "{:?}", app),
//       // CoreAst::BinaryOp(_loc, binop) => {
//       //   write!(f, "({:?} {} {:?}):{}", binop.left, binop.operator, binop.right, binop.ty)
//       // }
//       // CoreAst::UnaryOp(_loc, unop) => {
//       //   write!(f, "({} {:?})", unop.operator, unop.expr)
//       // }
//       _ => write!(f, "{}", self),
//     }
//   }
// }
