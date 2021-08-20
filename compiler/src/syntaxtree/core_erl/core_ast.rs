//! Defines AST tree for Core Erlang-like intermediate language. A more generalized and simplified
//! intermediate language, allowing easier optimisations and easier code generation.
use ::function_name::named;

use crate::display;
use crate::mfarity::MFArity;
use crate::source_loc::SourceLoc;
use crate::syntaxtree::core_erl::node::apply::Apply;
use crate::syntaxtree::core_erl::node::call::Call;
use crate::syntaxtree::core_erl::node::case::Case;
use crate::syntaxtree::core_erl::node::expression::{BinaryOperatorExpr, UnaryOperatorExpr};
use crate::syntaxtree::core_erl::node::fn_def::FnDef;
use crate::syntaxtree::core_erl::node::let_expr::LetExpr;
use crate::syntaxtree::core_erl::node::module_attr::ModuleAttr;
use crate::syntaxtree::core_erl::node::prim_op::PrimOp;
use crate::syntaxtree::core_erl::node::var::Var;
use crate::syntaxtree::erl::node::literal::Literal;
use crate::typing::erl_type::ErlType;
use crate::typing::typevar::TypeVar;

/// AST node in Core Erlang (parsed or generated)
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

  //
  // Execution and branching AST nodes
  //

  /// Define a function with 1 clause (multiple clauses handled by a `case Args of`)
  FnDef {
    /// Source file pointer
    location: SourceLoc,
    /// The function definition struct
    fn_def: FnDef,
  },
  /// Branch based on whether an expression matches some conditions
  Case {
    /// Source file pointer
    location: SourceLoc,
    /// The case expression. Clauses are contained inside
    case: Case,
  },
  /// Create a scope by assigning 1 or more variables
  Let {
    /// Source file pointer
    location: SourceLoc,
    /// The variable set, the value expr, and the body expr
    letexpr: LetExpr,
  },
  /// Call an expression which has a function type
  Apply {
    /// Source file pointer
    location: SourceLoc,
    /// The apply struct with call target, return type and arguments
    app: Apply,
  },
  /// Call a function export mod:fun/arity, or a local fun/arity
  Call {
    /// Source file pointer
    location: SourceLoc,
    /// The call struct with call target, return type and arguments
    call: Call,
  },
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
  Var {
    /// Source file pointer
    location: SourceLoc,
    /// The variable name and typevar bundle
    var: Var,
  },
  /// A literal value (fully known at compile time)
  Lit {
    /// Source file pointer
    location: SourceLoc,
    /// The literal tree
    value: Literal,
    /// Literal type
    ty: TypeVar,
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
    elements: Vec<CoreAst>,
    /// Optional tail element if not NIL
    tail: Option<Box<CoreAst>>,
  },
  /// A tuple of some expressions
  Tuple {
    /// Source code location
    location: SourceLoc,
    /// Tuple elements
    elements: Vec<CoreAst>,
  },
}

impl CoreAst {
  /// Gets the type of an AST node
  #[named]
  pub fn get_type(&self) -> ErlType {
    match self {
      // CoreAst::Attributes { .. } => ErlType::Any,
      CoreAst::FnDef { fn_def, .. } => fn_def.ret_ty.into(),
      CoreAst::Var { var: v, .. } => v.ty.into(),
      CoreAst::Apply { app, .. } => app.ret_ty.into(),
      CoreAst::Case { case, .. } => case.ret_ty.into(),
      CoreAst::Lit { value: l, .. } => l.get_type(),
      CoreAst::BinOp { op, .. } => op.get_result_type(),
      CoreAst::UnOp { op, .. } => op.expr.get_type(), // same type as expr bool or num
      CoreAst::List { elements, tail, .. } => {
        assert!(tail.is_none()); // todo
        let union_t = ErlType::union_of(
          elements.iter().map(|e| e.get_type()).collect(),
          true);
        ErlType::List(Box::new(union_t))
      }
      CoreAst::Tuple { elements, .. } => {
        ErlType::Tuple(elements.iter().map(|e| e.get_type()).collect())
      }
      // CoreAst::MFA { clause_types, .. } => {
      //   let fn_type = FunctionType::new(None, clause_types.clone());
      //   ErlType::Fn(fn_type)
      // }
      _ => unreachable!("{}: Can't process {}", function_name!(), self),
    }
  }

  /// Retrieve source file location for an AST element
  pub fn location(&self) -> SourceLoc {
    match self {
      CoreAst::FnDef { location: loc, .. } => *loc,
      CoreAst::Var { location, .. } => *location,
      CoreAst::Apply { location, .. } => *location,
      CoreAst::Case { location, .. } => *location,
      CoreAst::Lit { location, .. } => *location,
      CoreAst::BinOp { location, .. } => *location,
      CoreAst::UnOp { location, .. } => *location,
      _ => SourceLoc::default(),
    }
  }
}

impl std::fmt::Display for CoreAst {
  #[named]
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      CoreAst::Module { name, exports } => {
        writeln!(f, "module {} ", name)?;
        display::display_square_list(exports, f)
      }
      CoreAst::Attributes(attrs) => {
        writeln!(f, "attributes ")?;
        display::display_square_list(attrs, f)
      }
      CoreAst::FnDef { fn_def, .. } => {
        write!(f, "{}/{} = (fun ", fn_def.funarity.name, fn_def.funarity.arity)?;
        display::display_paren_list(&fn_def.args, f)?;
        write!(f, " -> {}", fn_def.body)?;
        write!(f, "}})")
      }
      CoreAst::Var { var, .. } => match &var.name {
        None => write!(f, "{}", var.ty),
        Some(n) => write!(f, "{}", n),
      },
      CoreAst::Apply { app, .. } => write!(f, "{}", app),
      CoreAst::Case { case, .. } => write!(f, "{}", case),
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
      CoreAst::List { elements, .. } => display::display_square_list(elements, f),
      CoreAst::Tuple { elements, .. } => display::display_curly_list(elements, f),
      CoreAst::Let { .. } => todo!("display(let)"),
      CoreAst::Call { .. } => todo!("display(call)"),
      CoreAst::PrimOp { .. } => todo!("display(primop)"),

      other => unimplemented!("{}: Don't know how to display {:?}", function_name!(), other),
    }
  }
}

impl std::fmt::Debug for CoreAst {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      CoreAst::FnDef { fn_def, .. } => {
        write!(f, "{}/{} = (fun ", fn_def.funarity.name, fn_def.funarity.arity)?;
        display::display_paren_list(&fn_def.args, f)?;
        write!(f, ":{} -> {}", fn_def.ret_ty, fn_def.body)?;
        write!(f, "}})")
        // write!(f, "fun {} -> {} {{", fn_def.funarity, fn_def.ret_ty)?;
        // for fc in fn_def.clauses.iter() { write!(f, "{:?};", fc)?; }
        // write!(f, "}}")
      }
      CoreAst::Var { var, .. } => write!(f, "{}:{}", self, var.ty),
      // CoreAst::Apply { app, .. } => write!(f, "{:?}", app),
      // CoreAst::BinaryOp(_loc, binop) => {
      //   write!(f, "({:?} {} {:?}):{}", binop.left, binop.operator, binop.right, binop.ty)
      // }
      // CoreAst::UnaryOp(_loc, unop) => {
      //   write!(f, "({} {:?})", unop.operator, unop.expr)
      // }
      _ => write!(f, "{}", self),
    }
  }
}
