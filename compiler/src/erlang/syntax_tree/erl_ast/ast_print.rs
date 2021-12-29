//! Adds debug printing for AST trees in a somewhat more compact way

use crate::display::Pretty;
use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::erlang::syntax_tree::erl_op::{ErlBinaryOp, ErlUnaryOp};
use crate::literal::Literal;

impl std::fmt::Display for ErlAst {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
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
      ErlAst::ModuleStartAttr { name, .. } => writeln!(f, "-module('{}').", name),
      ErlAst::UnparsedAttr { text, .. } => writeln!(f, "attr: {}", text),
      // ErlAst::Comma { left, right, .. } => {
      //   write!(f, "{}, {}", left, right)
      // }
      ErlAst::FnRef{mfa, ..} => write!(f, "fun {}", mfa),
      ErlAst::FnDef(erl_fndef) => {
        write!(f, "def-fun {} {{", erl_fndef.funarity.name)?;
        for fc in erl_fndef.clauses.iter() { write!(f, "{};", fc)?; }
        write!(f, "}}")
      }
      ErlAst::CClause(_loc, clause) => {
        write!(f, "{} when {} -> {}", clause.cond, clause.guard, clause.body)
      }
      ErlAst::Var(var) => write!(f, "{}", var.name),
      ErlAst::Apply(app) => write!(f, "{}", app),
      ErlAst::Case(_loc, case) => write!(f, "{}", case),
      ErlAst::Lit { value: lit, .. } => write!(f, "{}", lit),

      ErlAst::BinaryOp { expr: binop_expr, .. } => {
        write!(f, "({} {} {})", binop_expr.left, binop_expr.operator, binop_expr.right)
      }
      ErlAst::UnaryOp { expr: unop_expr, .. } => {
        write!(f, "({} {})", unop_expr.operator, unop_expr.expr)
      }
      ErlAst::MFA { mfarity: mfa, .. } => {
        match &mfa.module {
          None => write!(f, "(fun {}/{})", mfa.name, mfa.arity),
          Some(m) => write!(f, "(fun {}:{}/{})", m, mfa.name, mfa.arity),
        }
      }
      ErlAst::List { elements, .. } => Pretty::display_square_list(elements, f),
      ErlAst::Tuple { elements, .. } => Pretty::display_curly_list(elements, f),
      ErlAst::FnSpec { funarity, spec, .. } => {
        write!(f, "fun {}", funarity.name)?;
        Pretty::display_semicolon_separated(spec.as_fn_type().clauses(), f)
      }
    }
  }
}

// impl std::fmt::Debug for ErlAst {
//   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//     match self {
//       // ErlAst::Empty => {}
//       // ErlAst::Comment(_) => {}
//       // ErlAst::Token { .. } => {}
//       // ErlAst::ModuleForms(_) => {}
//       // ErlAst::ModuleAttr { .. } => {}
//       // ErlAst::Comma { .. } => {}
//       ErlAst::FnDef(erl_fndef) => {
//         write!(f, "def-fun {} {{", erl_fndef.funarity)?;
//         for fc in erl_fndef.clauses.iter() { write!(f, "{:?};", fc)?; }
//         write!(f, "}}")
//       }
//       // ErlAst::CClause(_, _) => {}
//       // ErlAst::FunArity(_, _) => {}
//       ErlAst::Var(var) => write!(f, "{}", var.name),
//       ErlAst::Apply(_loc, app) => write!(f, "{:?}", app),
//       // ErlAst::Let(_, _) => {}
//       // ErlAst::Case(_, _) => {}
//       // ErlAst::Lit(_, _) => {}
//       ErlAst::BinaryOp(_loc, binop) => {
//         write!(f, "({:?} {} {:?}):{}", binop.left, binop.operator, binop.right, binop.ty)
//       }
//       ErlAst::UnaryOp(_loc, unop) => {
//         write!(f, "({} {:?})", unop.operator, unop.expr)
//       }
//       // ErlAst::List(_, _) => {}
//       // ErlAst::Tuple(_, _) => {}
//       _ => write!(f, "{}", self),
//     }
//   }
// }

impl std::fmt::Display for Literal {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      Literal::Integer(i) => write!(f, "{}", i),
      Literal::BigInteger => write!(f, "<NotImpl:Printing BigInteger>"),
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
      ErlBinaryOp::Bang => write!(f, "!"),
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