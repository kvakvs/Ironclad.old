//! Adds debug printing for AST trees in a somewhat more compact way

use crate::erl_syntax::erl_ast::node_impl::AstNodeType::{
  Apply, BeginEnd, BinaryComprehension, BinaryExpr, BinaryOp, CClause, CaseStatement, CommaExpr,
  FnDef, FnRef, IfStatement, List, ListComprehension, ListComprehensionGenerator, Lit, MapBuilder,
  ModuleRoot, RecordBuilder, TryCatch, Tuple, Type, UnaryOp, Var, MFA,
};
use crate::erl_syntax::erl_ast::node_impl::{AstNodeImpl, AstNodeType};
use crate::erl_syntax::erl_op::{ErlBinaryOp, ErlUnaryOp};
use crate::literal::Literal;
use libironclad_util::pretty::Pretty;

impl std::fmt::Display for AstNodeImpl {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match &self.content {
      AstNodeType::Empty { comment } => writeln!(f, "%% {}", comment),
      BeginEnd { exprs } => {
        write!(f, "begin ")?;
        Pretty::display_comma_separated(exprs.iter(), f)?;
        write!(f, " end")
      }
      ModuleRoot { forms } => {
        writeln!(f, "-module(...).")?;
        for form in forms.iter() {
          form.fmt(f)?;
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
      Var(var) => var.name.fmt(f),
      Apply(app) => app.fmt(f),
      CaseStatement { expr, clauses, .. } => {
        write!(f, "case {} of", expr)?;
        Pretty::display_semicolon_separated(clauses.iter(), f)?;
        writeln!(f, "end")
      }
      Lit { value: lit, .. } => lit.fmt(f),

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
      CommaExpr { elements, .. } => Pretty::display_comma_separated(elements.iter(), f),
      List { elements, tail: maybe_tail, .. } => {
        write!(f, "[")?;
        Pretty::display_comma_separated(elements.iter(), f)?;
        if let Some(tail) = maybe_tail {
          write!(f, " | {}", tail)?;
        }
        write!(f, "]")
      }
      Tuple { elements, .. } => Pretty::display_curly_list(elements.iter(), f),
      Type { ty, .. } => ty.fmt(f),
      MapBuilder { members } => {
        write!(f, "#{{")?;
        Pretty::display_comma_separated(members.iter(), f)?;
        write!(f, "}}")
      }
      RecordBuilder { base, tag, members } => {
        if let Some(base_var) = base {
          write!(f, "{}", base_var)?;
        }
        write!(f, "#{}{{", tag)?;
        Pretty::display_comma_separated(members.iter(), f)?;
        write!(f, "}}")
      }
      ListComprehension { expr, generators, .. } => {
        write!(f, "[{} || ", expr)?;
        Pretty::display_comma_separated(generators.iter(), f)?;
        write!(f, "]")
      }
      BinaryComprehension { expr, generators, .. } => {
        write!(f, "<<{} || ", expr)?;
        Pretty::display_comma_separated(generators.iter(), f)?;
        write!(f, ">>")
      }
      ListComprehensionGenerator { left, right, .. } => {
        write!(f, "{} <- {}", left, right)
      }
      TryCatch { body, of_branches, catch_clauses, .. } => {
        write!(f, "try {}", body)?;
        if let Some(ofb) = of_branches {
          write!(f, "of")?;
          Pretty::display_semicolon_separated(ofb.iter(), f)?;
        }
        Pretty::display_semicolon_separated(catch_clauses.iter(), f)
      }
      IfStatement { clauses, .. } => {
        write!(f, "if ")?;
        Pretty::display_semicolon_separated(clauses.iter(), f)?;
        write!(f, " end")
      }
      BinaryExpr { elements, .. } => {
        write!(f, "<<")?;
        Pretty::display_comma_separated(elements.iter(), f)?;
        write!(f, ">>")
      }
    }
  }
}

impl std::fmt::Display for Literal {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      Literal::Integer(i) => i.fmt(f),
      Literal::Float(flt) => flt.fmt(f),
      Literal::Atom(a) => write!(f, "'{}'", a),
      Literal::Bool(b) => b.fmt(f),
      Literal::List { elements, .. } => Pretty::display_square_list(elements.iter(), f),
      Literal::Nil => write!(f, "[]"),
      Literal::String(s) => write!(f, "\"{}\"", s),
      Literal::Tuple(t) => Pretty::display_curly_list(t.iter(), f),
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
