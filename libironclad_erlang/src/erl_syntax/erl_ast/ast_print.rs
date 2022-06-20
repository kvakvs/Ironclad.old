//! Adds debug printing for AST trees in a somewhat more compact way

use crate::erl_syntax::erl_ast::node_impl::AstNodeType::{
  Apply, BinaryExpr, BinaryOp, CClause, CaseStatement, CommaExpr, FnDef, FnRef, IfStatement, List,
  ListComprehension, ListComprehensionGenerator, Lit, MapBuilder, ModuleRoot, TryCatch, Tuple,
  Type, UnaryOp, Var, MFA,
};
use crate::erl_syntax::erl_ast::node_impl::{AstNodeImpl, AstNodeType};
use crate::erl_syntax::erl_op::{ErlBinaryOp, ErlUnaryOp};
use crate::literal::Literal;
use libironclad_util::pretty::Pretty;

impl std::fmt::Display for AstNodeImpl {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match &self.content {
      AstNodeType::Empty { comment } => writeln!(f, "%% {}", comment),
      ModuleRoot { forms, name } => {
        writeln!(f, "-module({}).", name)?;
        for form in forms.iter() {
          write!(f, "{}", form)?;
        }
        writeln!(f, "%%% end module",)
      }
      FnRef { mfa, .. } => write!(f, "fun {}", mfa),
      FnDef(erl_fndef) => {
        write!(f, "def-fun {} {{", erl_fndef.funarity.name)?;
        for fc in erl_fndef.clauses.iter() {
          write!(f, "{};", fc)?;
        }
        writeln!(f, "}}")
      }
      CClause(_loc, clause) => match &clause.guard {
        Some(g) => write!(f, "{} when {} -> {}", clause.pattern, g, clause.body),
        None => write!(f, "{} -> {}", clause.pattern, clause.body),
      },
      Var(var) => write!(f, "{}", var.name),
      Apply(app) => write!(f, "{}", app),
      CaseStatement { expr, clauses, .. } => {
        write!(f, "case {} of", expr)?;
        Pretty::display_semicolon_separated(clauses, f)?;
        writeln!(f, "end")
      }
      Lit { value: lit, .. } => write!(f, "{}", lit),

      BinaryOp { expr: binop_expr, .. } => {
        write!(f, "({} {} {})", binop_expr.left, binop_expr.operator, binop_expr.right)
      }
      UnaryOp { expr: unop_expr, .. } => {
        write!(f, "({} {})", unop_expr.operator, unop_expr.expr)
      }
      MFA { mfarity: mfa, .. } => match &mfa.module {
        None => write!(f, "(fun {}/{})", mfa.name, mfa.arity),
        Some(m) => write!(f, "(fun {}:{}/{})", m, mfa.name, mfa.arity),
      },
      CommaExpr { elements, .. } => Pretty::display_comma_separated(elements, f),
      List { elements, tail: maybe_tail, .. } => {
        write!(f, "[")?;
        Pretty::display_comma_separated(elements, f)?;
        if let Some(tail) = maybe_tail {
          write!(f, " | {}", tail)?;
        }
        write!(f, "]")
      }
      Tuple { elements, .. } => Pretty::display_curly_list(elements, f),
      Type { ty, .. } => write!(f, "{}", ty),
      MapBuilder { members } => {
        write!(f, "#{{")?;
        Pretty::display_comma_separated(members, f)?;
        write!(f, "}}")
      }
      ListComprehension { expr, generators, .. } => {
        write!(f, "[{} || ", expr)?;
        Pretty::display_comma_separated(generators, f)?;
        write!(f, "]")
      }
      ListComprehensionGenerator { left, right, .. } => {
        write!(f, "{} <- {}", left, right)
      }
      TryCatch { body, of_branches, catch_clauses, .. } => {
        write!(f, "try {}", body)?;
        if let Some(ofb) = of_branches {
          write!(f, "of")?;
          Pretty::display_semicolon_separated(ofb, f)?;
        }
        Pretty::display_semicolon_separated(catch_clauses, f)
      }
      IfStatement { clauses, .. } => {
        write!(f, "if ")?;
        Pretty::display_semicolon_separated(clauses, f)?;
        write!(f, " end")
      }
      BinaryExpr { elements, .. } => {
        write!(f, "<<")?;
        Pretty::display_comma_separated(elements, f)?;
        write!(f, ">>")
      }
    }
  }
}

impl std::fmt::Display for Literal {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      Literal::Integer(i) => write!(f, "{}", i),
      Literal::Float(flt) => write!(f, "{}", flt),
      Literal::Atom(a) => write!(f, "'{}'", a),
      Literal::Bool(b) => write!(f, "{}", b),
      Literal::List { elements, .. } => Pretty::display_square_list(elements, f),
      Literal::Nil => write!(f, "[]"),
      Literal::String(s) => write!(f, "\"{}\"", s),
      Literal::Tuple(t) => Pretty::display_curly_list(t, f),
    }
  }
}

impl std::fmt::Display for ErlBinaryOp {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      ErlBinaryOp::Add => write!(f, "+"),
      ErlBinaryOp::Sub => write!(f, "-"),
      ErlBinaryOp::Mul => write!(f, "*"),
      ErlBinaryOp::Div => write!(f, "/"),
      ErlBinaryOp::IntegerDiv => write!(f, "div"),
      ErlBinaryOp::Remainder => write!(f, "mod"),
      ErlBinaryOp::Less => write!(f, "<"),
      ErlBinaryOp::Greater => write!(f, ">"),
      ErlBinaryOp::LessEq => write!(f, "⩽"),
      ErlBinaryOp::GreaterEq => write!(f, "⩾"),
      ErlBinaryOp::Eq => write!(f, "≃"),
      ErlBinaryOp::NotEq => write!(f, "≄"),
      ErlBinaryOp::HardEq => write!(f, "≡"),
      ErlBinaryOp::HardNotEq => write!(f, "≢"),
      ErlBinaryOp::ListAppend => write!(f, "++"),
      ErlBinaryOp::ListSubtract => write!(f, "--"),
      ErlBinaryOp::Comma => write!(f, ","),
      ErlBinaryOp::Semicolon => write!(f, ";"),
      ErlBinaryOp::And => write!(f, "and"),
      ErlBinaryOp::AndAlso => write!(f, "andalso"),
      ErlBinaryOp::Or => write!(f, "or"),
      ErlBinaryOp::OrElse => write!(f, "orelse"),
      ErlBinaryOp::Xor => write!(f, "xor"),
      ErlBinaryOp::BinaryAnd => write!(f, "band"),
      ErlBinaryOp::BinaryOr => write!(f, "bor"),
      ErlBinaryOp::BinaryXor => write!(f, "bxor"),
      ErlBinaryOp::BinaryShiftLeft => write!(f, "bsl"),
      ErlBinaryOp::BinaryShiftRight => write!(f, "bsr"),
      ErlBinaryOp::Match => write!(f, "="),
      ErlBinaryOp::Send => write!(f, "!"),
    }
  }
}

impl std::fmt::Display for ErlUnaryOp {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      ErlUnaryOp::Not => write!(f, "not"),
      ErlUnaryOp::BinaryNot => write!(f, "bnot"),
      ErlUnaryOp::Negative => write!(f, "-"),
      ErlUnaryOp::Positive => write!(f, "+"),
      ErlUnaryOp::Catch => write!(f, "catch"),
    }
  }
}
