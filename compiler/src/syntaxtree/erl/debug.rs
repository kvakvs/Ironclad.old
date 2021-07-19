//! Adds debug printing for AST trees in a somewhat more compact way

use std::fmt;
use crate::syntaxtree::erl::erl_ast::{ErlAst, ErlToken};
use crate::syntaxtree::erl::literal::ErlLit;

impl fmt::Debug for ErlAst {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      ErlAst::Empty => write!(f, "∅"),
      ErlAst::Token(t) => write!(f, "⊥{:?}", t),
      ErlAst::Forms(forms) => {
        write!(f, "Forms")?;
        f.debug_list().entries(forms.iter()).finish()
      }
      ErlAst::ModuleAttr { name } => write!(f, "module('{}')", name),
      ErlAst::Comma { exprs, .. } => {
        write!(f, "Comma")?;
        f.debug_list().entries(exprs.iter()).finish()
      }
      ErlAst::NewFunction { name, clauses, .. } => {
        write!(f, "λ{} ", name)?;
        f.debug_list().entries(clauses.iter()).finish()
      }
      ErlAst::FClause { args, body, .. } => {
        f.debug_struct("fclause")
            .field("args", args)
            .field("->", body)
            .finish()
      }
      ErlAst::CClause { cond, guard, body, .. } => {
        f.debug_struct("clause")
            .field("cond", cond)
            .field("guard", guard)
            .field("->", body)
            .finish()
      }
      ErlAst::Var { name, .. } => write!(f, "{}", name),
      ErlAst::App { expr, args, .. } => {
        f.debug_tuple("apply")
            .field(expr)
            .field(args)
            .finish()
      }
      ErlAst::Let { var, value, in_expr, .. } => {
        f.debug_struct("let")
            .field("var", var)
            .field("=", value)
            .field("in", in_expr)
            .finish()
      }
      ErlAst::Case { arg, clauses, .. } => {
        f.debug_struct("case")
            .field("arg", arg)
            .field("of", clauses)
            .finish()
      }
      ErlAst::Lit(lit) => write!(f, "{:?}", lit),
      ErlAst::BinaryOp { left, right, op, .. } => {
        write!(f, "({:?} {:?} {:?})", left, op, right)
      }
      ErlAst::UnaryOp { expr, op, .. } => {
        write!(f, "({:?} {:?})", op, expr)
      }
    }
  }
}

impl fmt::Debug for ErlToken {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      ErlToken::Comma => write!(f, ","),
      ErlToken::Plus => write!(f, "+"),
      ErlToken::Minus => write!(f, "-"),
      ErlToken::Div => write!(f, "/"),
      ErlToken::Mul => write!(f, "*"),
      ErlToken::IntegerDiv => write!(f, "div"),
      ErlToken::Remainder => write!(f, "rem"),
      ErlToken::Not => write!(f, "not"),
      ErlToken::Or => write!(f, "or"),
      ErlToken::Xor => write!(f, "xor"),
      ErlToken::And => write!(f, "and"),
      ErlToken::BinaryNot => write!(f, "bnot"),
      ErlToken::BinaryAnd => write!(f, "band"),
      ErlToken::BinaryOr => write!(f, "bor"),
      ErlToken::BinaryXor => write!(f, "bxor"),
      ErlToken::BinaryShiftLeft => write!(f, "bsl"),
      ErlToken::BinaryShiftRight => write!(f, "bsr"),
      ErlToken::ListAppend => write!(f, "++"),
      ErlToken::ListSubtract => write!(f, "--"),
      ErlToken::Eq => write!(f, "≃"),
      ErlToken::NotEq => write!(f, "≄"),
      ErlToken::LessThanEq => write!(f, "⩽"),
      ErlToken::LessThan => write!(f, "<"),
      ErlToken::GreaterEq => write!(f, "⩾"),
      ErlToken::GreaterThan => write!(f, ">"),
      ErlToken::HardEq => write!(f, "≡"),
      ErlToken::HardNotEq => write!(f, "≢"),
      ErlToken::AndAlso => write!(f, "andalso"),
      ErlToken::OrElse => write!(f, "orelse"),
      ErlToken::Assign => write!(f, "="),
      ErlToken::Send => write!(f, "!"),
      ErlToken::Catch => write!(f, "catch"),
    }
  }
}

impl fmt::Debug for ErlLit {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      ErlLit::Integer(i) => write!(f, "{}", i),
      ErlLit::Float(flt) => write!(f, "{}", flt),
      ErlLit::Atom(a) => write!(f, "'{}'", a),
      ErlLit::Bool(b) => write!(f, "{}", b),
      ErlLit::List(lst) => {
        f.debug_list().entries(lst.iter()).finish()
      }
      ErlLit::String(s) => write!(f, "\"{}\"", s),
      ErlLit::Tuple(t) => {
        write!(f, "tuple")?;
        f.debug_list().entries(t.iter()).finish()
      }
    }
  }
}