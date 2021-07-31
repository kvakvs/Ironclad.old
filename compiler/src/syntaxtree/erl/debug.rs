//! Adds debug printing for AST trees in a somewhat more compact way

use std::fmt;
use crate::syntaxtree::erl::erl_ast::{ErlAst};
use crate::syntaxtree::erl::node::literal_node::LiteralNode;
use crate::syntaxtree::erl::erl_op::ErlBinaryOp;
use crate::syntaxtree::erl::node::fun_clause_node::FunctionClauseNode;
use crate::syntaxtree::erl::node::token::ErlToken;

impl fmt::Debug for ErlAst {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      ErlAst::Comment { .. } => write!(f, "%comment"),
      ErlAst::Empty => write!(f, "ʎʇdɯǝ"),
      ErlAst::Token { token: t, .. } => {
        // Tokens should look funny because they do not belong in final AST, must be removed while
        // parsing and processing AST
        write!(f, "uǝʞoʇ{:?}", t)
      }
      ErlAst::ModuleForms(forms) => {
        write!(f, "sɯɹoɟ")?;
        f.debug_list().entries(forms.iter()).finish()
      }
      ErlAst::ModuleAttr { name, .. } => write!(f, "module('{}')", name),
      ErlAst::Comma { left, right, .. } => {
        write!(f, "{:?}, {:?}", left, right)
        // f.debug_list().entries(exprs.iter()).finish()
      }
      ErlAst::NewFunction(_loc, nf) => {
        write!(f, "fun {}/{}: ", nf.funarity.name, nf.funarity.arity)?;
        let results: Vec<_> = nf.clauses.iter()
            .map(|fc| write!(f, "{:?};", fc))
            .filter(Result::is_err)
            .collect();
        if results.is_empty() {
          Ok(())
        } else {
          results[0]
        }
      }
      // ErlAst::FClause(_loc, fc) => write!(f, "{:?}", fc),
      ErlAst::CClause(_loc, clause) => {
        write!(f, "{:?} when {:?} -> {:?}", clause.cond, clause.guard, clause.body)
      }
      ErlAst::Var(_loc, v) => write!(f, "{}", v.name),
      ErlAst::App(_loc, app) => {
        write!(f, "apply({:?}, {:?})", app.expr, app.args)
      }
      ErlAst::Let(_loc, let_expr) => {
        write!(f, "let {} = {:?} in {:?}", let_expr.var, let_expr.value, let_expr.in_expr)
      }
      ErlAst::Case(_loc, case) => {
        write!(f, "case {:?} of {:?}", case.arg, case.clauses)
      }
      ErlAst::Lit(_loc, lit) => write!(f, "{:?}", lit),
      ErlAst::BinaryOp(_loc, binop) => {
        write!(f, "({:?} {:?} {:?})", binop.left, binop.operator, binop.right)
      }
      ErlAst::UnaryOp(_loc, unop) => {
        write!(f, "({:?} {:?})", unop.operator, unop.expr)
      }
      ErlAst::FunArity(_loc, fa) => write!(f, "fun {}/{}", fa.name, fa.arity),
    }
  }
}

impl fmt::Debug for ErlToken {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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

impl fmt::Debug for LiteralNode {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      LiteralNode::Integer(i) => write!(f, "{}", i),
      LiteralNode::Float(flt) => write!(f, "{}", flt),
      LiteralNode::Atom(a) => write!(f, "'{}'", a),
      LiteralNode::Bool(b) => write!(f, "{}", b),
      LiteralNode::List(lst) => {
        f.debug_list().entries(lst.iter()).finish()
      }
      LiteralNode::String(s) => write!(f, "\"{}\"", s),
      LiteralNode::Tuple(t) => {
        write!(f, "tuple")?;
        f.debug_list().entries(t.iter()).finish()
      }
    }
  }
}

impl fmt::Debug for FunctionClauseNode {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}{:?} -> {:?}", self.name, self.args, self.body)
  }
}

impl fmt::Debug for ErlBinaryOp {
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
    }
  }
}