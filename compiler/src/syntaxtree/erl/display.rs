//! Adds debug printing for AST trees in a somewhat more compact way

use std::fmt;
use std::fmt::Formatter;

use crate::display;
use crate::syntaxtree::erl::erl_ast::ErlAst;
use crate::syntaxtree::erl::erl_op::{ErlBinaryOp, ErlUnaryOp};
use crate::syntaxtree::erl::node::fun_clause_node::FunctionClauseNode;
use crate::syntaxtree::erl::node::literal_node::LiteralNode;

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
      ErlAst::NewFunction(_loc, nf) => {
        write!(f, "fun {}/{}: ", nf.funarity.name, nf.funarity.arity)?;
        let results: Vec<_> = nf.clauses.iter()
            .map(|fc| write!(f, "{};", fc))
            .filter(Result::is_err)
            .collect();
        if results.is_empty() {
          Ok(())
        } else {
          results[0]
        }
      }
      ErlAst::CClause(_loc, clause) => {
        write!(f, "{} when {} -> {}", clause.cond, clause.guard, clause.body)
      }
      ErlAst::Var(_loc, v) => write!(f, "{}", v.name),

      ErlAst::App(_loc, app) => {
        write!(f, "{}(", app.expr)?;
        display::display_comma_separated(&app.args, f)?;
        write!(f, ")")
      }

      ErlAst::Let(_loc, let_expr) => {
        write!(f, "let {} = {} in {}", let_expr.var, let_expr.value, let_expr.in_expr)
      }
      ErlAst::Case(_loc, case) => write!(f, "{}", case),
      ErlAst::Lit(_loc, lit) => write!(f, "{}", lit),

      ErlAst::BinaryOp(_loc, binop) => {
        write!(f, "({} {} {})", binop.left, binop.operator, binop.right)
      }

      ErlAst::UnaryOp(_loc, unop) => {
        write!(f, "({} {})", unop.operator, unop.expr)
      }
      ErlAst::FunArity(_loc, fa) => write!(f, "fun {}/{}", fa.name, fa.arity),
      ErlAst::List(_loc, elems) => display::display_list(elems, f),
      ErlAst::Tuple(_loc, elems) => display::display_tuple(elems, f),
    }
  }
}

impl std::fmt::Display for LiteralNode {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      LiteralNode::Integer(i) => write!(f, "{}", i),
      LiteralNode::Float(flt) => write!(f, "{}", flt),
      LiteralNode::Atom(a) => write!(f, "'{}'", a),
      LiteralNode::Bool(b) => write!(f, "{}", b),
      LiteralNode::List(elems) => display::display_list(elems, f),
      LiteralNode::String(s) => write!(f, "\"{}\"", s),
      LiteralNode::Tuple(t) => display::display_tuple(t, f),
    }
  }
}

impl std::fmt::Display for FunctionClauseNode {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}(", self.name)?;
    display::display_comma_separated(&self.args, f)?;
    write!(f, ") -> {}", self.body)
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
