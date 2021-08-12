//! Adds debug printing for AST trees in a somewhat more compact way

use std::fmt;
use std::fmt::Formatter;

use crate::display;
use crate::syntaxtree::erl::erl_ast::ErlAst;
use crate::syntaxtree::erl::erl_op::{ErlBinaryOp, ErlUnaryOp};
use crate::syntaxtree::erl::node::literal::Literal;

impl fmt::Display for ErlAst {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      ErlAst::Comment { .. } => writeln!(f, "% comment"),
      ErlAst::Empty => writeln!(f, "% empty"),
      ErlAst::Token { token: t, .. } => writeln!(f, "% token {}", t),
      ErlAst::ModuleForms(forms) => {
        for form in forms.iter() {
          writeln!(f, "{}", form)?;
        }
        Ok(())
      }
      ErlAst::ModuleAttr { name, .. } => writeln!(f, "-module('{}').", name),
      ErlAst::Comma { left, right, .. } => {
        write!(f, "{}, {}", left, right)
      }
      ErlAst::FunctionDef { funarity, fn_def, .. } => {
        write!(f, "newfun {}/{} {{", funarity.name, funarity.arity)?;
        for fc in fn_def.clauses.iter() { write!(f, "{};", fc)?; }
        write!(f, "}}")
      }
      ErlAst::CClause(_loc, clause) => {
        write!(f, "{} when {} -> {}", clause.cond, clause.guard, clause.body)
      }
      ErlAst::Var(_loc, v) => write!(f, "{}", v.name),
      ErlAst::Apply(_loc, app) => write!(f, "{}", app),
      ErlAst::Case(_loc, case) => write!(f, "{}", case),
      ErlAst::Lit(_loc, lit) => write!(f, "{}", lit),

      ErlAst::BinaryOp(_loc, binop) => {
        write!(f, "({} {} {})", binop.left, binop.operator, binop.right)
      }
      ErlAst::UnaryOp(_loc, unop) => {
        write!(f, "({} {})", unop.operator, unop.expr)
      }
      ErlAst::MFA { mfarity: mfa, .. } => {
        match &mfa.module {
          None => write!(f, "(fun {}/{})", mfa.name, mfa.arity),
          Some(m) => write!(f, "(fun {}:{}/{})", m, mfa.name, mfa.arity),
        }
      },
      ErlAst::List(_loc, elems) => display::display_list(elems, f),
      ErlAst::Tuple(_loc, elems) => display::display_tuple(elems, f),
    }
  }
}

impl fmt::Debug for ErlAst {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      // ErlAst::Empty => {}
      // ErlAst::Comment(_) => {}
      // ErlAst::Token { .. } => {}
      // ErlAst::ModuleForms(_) => {}
      // ErlAst::ModuleAttr { .. } => {}
      // ErlAst::Comma { .. } => {}
      ErlAst::FunctionDef { funarity, fn_def, ret_ty, .. } => {
        write!(f, "newfun {}/{} -> {} {{", funarity.name, funarity.arity, ret_ty)?;
        for fc in fn_def.clauses.iter() { write!(f, "{:?};", fc)?; }
        write!(f, "}}")
      }
      // ErlAst::CClause(_, _) => {}
      // ErlAst::FunArity(_, _) => {}
      ErlAst::Var(_, v) => write!(f, "{}:{}", self, v.ty),
      ErlAst::Apply(_loc, app) => write!(f, "{:?}", app),
      // ErlAst::Let(_, _) => {}
      // ErlAst::Case(_, _) => {}
      // ErlAst::Lit(_, _) => {}
      ErlAst::BinaryOp(_loc, binop) => {
        write!(f, "({:?} {} {:?}):{}", binop.left, binop.operator, binop.right, binop.ty)
      }
      ErlAst::UnaryOp(_loc, unop) => {
        write!(f, "({} {:?})", unop.operator, unop.expr)
      }
      // ErlAst::List(_, _) => {}
      // ErlAst::Tuple(_, _) => {}
      _ => write!(f, "{}", self),
    }
  }
}

impl std::fmt::Display for Literal {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Literal::Integer(i) => write!(f, "{}", i),
      Literal::Float(flt) => write!(f, "{}", flt),
      Literal::Atom(a) => write!(f, "'{}'", a),
      Literal::Bool(b) => write!(f, "{}", b),
      Literal::List(elems) => display::display_list(elems, f),
      Literal::String(s) => write!(f, "\"{}\"", s),
      Literal::Tuple(t) => display::display_tuple(t, f),
    }
  }
}

impl std::fmt::Display for ErlBinaryOp {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      ErlBinaryOp::Add => write!(f, "+"),
      ErlBinaryOp::Sub => write!(f, "-"),
      ErlBinaryOp::Mul => write!(f, "*"),
      ErlBinaryOp::Div => write!(f, "/"),
      ErlBinaryOp::IntegerDiv => write!(f, "div"),
      ErlBinaryOp::Modulo => write!(f, "mod"),
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
    }
  }
}

impl std::fmt::Display for ErlUnaryOp {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      ErlUnaryOp::Not => write!(f, "not"),
      ErlUnaryOp::Negative => write!(f, "-"),
      ErlUnaryOp::Positive => write!(f, "+"),
      ErlUnaryOp::Catch => write!(f, "catch"),
    }
  }
}
