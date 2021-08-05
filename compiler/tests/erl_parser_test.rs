extern crate compiler;

mod test_util;

use compiler::syntaxtree::erl::erl_parser::{Rule};
use compiler::syntaxtree::erl::erl_ast::{ErlAst};
use compiler::syntaxtree::erl::node::literal_node::LiteralNode;
use std::ops::Deref;
use compiler::erl_module::ErlModule;
use compiler::erl_error::ErlResult;

/// Try parse string
#[test]
fn parse_string_test() -> ErlResult<()> {
  let mut module1 = ErlModule::new_testing();
  module1.parse_str(Rule::string, "\"abc\"").unwrap();

  {
    let ast = module1.ast.read().unwrap();
    if let ErlAst::Lit(_loc, LiteralNode::String(_value)) = ast.deref() {
      // ok
    } else {
      panic!("Expected: Literal(String) result, got {}", ast)
    }
  }
  Ok(())
}

/// Try parse a flat + expression
#[test]
fn parse_expr_flat() -> ErlResult<()> {
  let mut module1 = ErlModule::new_testing();
  module1.parse_str(Rule::expr, "A + 123 + 333 + 6 + atom + Test")?;

  {
    let ast = module1.ast.read().unwrap();
    if let ErlAst::BinaryOp { .. } = ast.deref() {
      // ok
    } else {
      panic!("Expected: ErlAst::BinaryOp(+), got {}", ast);
    }
  }
  Ok(())
}

/// Try parse a more complex expression
#[test]
fn parse_expr_longer() -> ErlResult<()> {
  let mut module1 = ErlModule::new_testing();
  module1.parse_str(Rule::expr, "123 + 1 / (2 * hello)")?;

  {
    let ast = module1.ast.read().unwrap();
    if let ErlAst::BinaryOp { .. } = ast.deref() {
      //ok
    } else {
      panic!("Expected: ErlAst::BinaryOp(+), got {}", ast);
    }
  }
  Ok(())
}

/// Try parse a comma expression with some simpler nested exprs
#[test]
fn parse_expr_comma() -> ErlResult<()> {
  let mut module1 = ErlModule::new_testing();
  module1.parse_str(Rule::expr, "A, B, 123 * C")?;

  {
    let ast = module1.ast.read().unwrap();
    if let ErlAst::Comma { .. } = ast.deref() {
      // ok
    } else {
      panic!("Expected: ErlAst::Comma, got {}", ast);
    }
  }
  Ok(())
}

/// Try parse some function defs
#[test]
fn parse_fn1() -> ErlResult<()> {
  let mut module1 = ErlModule::new_testing();
  module1.parse_str(Rule::function_def, "f(A) -> atom123.")?;

  {
    let ast = module1.ast.read().unwrap();
    if let ErlAst::NewFunction { .. } = ast.deref() {
      // ok
    } else {
      panic!("Expected: ErlAst::NewFunction, got {}", ast);
    }
  }
  Ok(())
}

/// Try parse a function application expr. This can be any expression immediately followed by
/// a parenthesized comma expression.
#[test]
fn parse_application() -> ErlResult<()> {
  let mut module1 = ErlModule::new_testing();
  module1.parse_str(Rule::expr, "a_function()")?;
  println!("parse_application 1 parsed {}", module1.ast.read().unwrap());

  {
    let ast1 = module1.ast.read().unwrap();
    if let ErlAst::App { .. } = ast1.deref() {
      // ok
    } else {
      panic!("Expected: ErlAst::App, got {}", module1.ast.read().unwrap());
    }
  }

  let mut module2 = ErlModule::new_testing();
  module2.parse_str(Rule::expr, "(123 + atom)()")?;
  println!("parse_application 2 parsed {}", module2.ast.read().unwrap());

  {
    let ast2 = module2.ast.read().unwrap();
    if let ErlAst::App { .. } = ast2.deref() {
      // ok
    } else {
      panic!("Expected: ErlAst::App, got {}", module2.ast.read().unwrap());
    }
  }

  let mut module3 = ErlModule::new_testing();
  module3.parse_str(Rule::expr, "(F() + g())(test(), 123())")?;
  println!("parse_application 3 parsed {}", module3.ast.read().unwrap());

  {
    let ast3 = module3.ast.read().unwrap();
    if let ErlAst::App { .. } = ast3.deref() {
      // ok
    } else {
      panic!("Expected: ErlAst::App, got {}", module3.ast.read().unwrap());
    }
  }
  Ok(())
}