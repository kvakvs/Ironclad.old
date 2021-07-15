use crate::erl_error::ErlResult;
use crate::syntaxtree::erl::case_clause::CaseClause;
use crate::syntaxtree::erl::erl_ast::ErlAst;
use crate::syntaxtree::erl::erl_op::{ErlBinaryOp, ErlUnaryOp};
use crate::syntaxtree::erl::literal::ErlLiteral;
use crate::typing::equation::TypeEquation;
use crate::typing::erl_type::ErlType;

// /// An Erlang expression
// #[derive(Debug, Clone, PartialEq)]
// pub enum ErlExpr {
//   /// A named variable
//   Var {
//     name: String,
//     ty: ErlType,
//   },
//
//   /// Apply arguments to expression
//   App {
//     /// Target, to be called, expected to have function or lambda type
//     expr: Box<ErlExpr>,
//     /// Arguments. Their  inferred types are stored inside.
//     args: Vec<ErlExpr>,
//     /// Return inferred type.
//     ty: ErlType,
//   },
//
//   // /// A lambda definition or a function
//   // Function { args: Vec<ErlExpr>, expr: Box<ErlExpr> },
//
//   /// A haskell-style new variable introducing a new scope below it:
//   /// let x = expr1 in expr2
//   Let {
//     var: String,
//     /// Type which we believe is Var
//     var_ty: ErlType,
//     /// Value (type is in it)
//     value: Box<ErlExpr>,
//     /// Let x=y in <body> (type is in it, and becomes type of Expr::Let)
//     in_expr: Box<ErlExpr>,
//   },
//
//   // // TODO: Remove If because can be replaced with Case
//   // If {
//   //   cond: Box<ErlExpr>,
//   //   on_true: Box<ErlExpr>,
//   //   on_false: Box<ErlExpr>,
//   // },
//   Case {
//     /// A union type of all case clauses
//     ty: ErlType,
//     arg: Box<ErlExpr>,
//     clauses: Vec<CaseClause>,
//   },
//
//   /// A literal value, constant. Type is known via literal.get_type()
//   Lit(ErlLiteral),
//
//   BinaryOp { left: Box<ErlExpr>, right: Box<ErlExpr>, op: ErlBinaryOp },
//   UnaryOp { expr: Box<ErlExpr>, op: ErlUnaryOp },
// }

// impl ErlExpr {
//   pub fn new_var(name: &str) -> ErlExpr {
//     ErlExpr::Var {
//       name: name.to_string(),
//       ty: ErlType::new_typevar(),
//     }
//   }
//
//   // pub fn lit_integer(value: isize) -> Self {
//   //   ErlExpr::Lit(ErlLiteral::Integer(value))
//   // }
//
//   pub fn generate_equations_for_expr<'a>(&self,
//                                          ast: &'a ErlAst,
//                                          result: &mut Vec<TypeEquation<'a>>) -> ErlResult<()> {
//     match self {
//       ErlExpr::Var { .. } => Ok(()), // variables add no equations
//       ErlExpr::App { expr, args, ty } => {
//
//         let fn_type = ErlType::Function {
//           name: None,
//           arg_ty: args.iter()
//               .map(|a| a.get_type())
//               .cloned()
//               .collect(),
//           ret: Box::from(ty.clone()),
//         };
//         // Assume that call target expression type matches fun((args...) -> returntype)
//         result.push(TypeEquation::new(ast, expr.get_type(), fn_type));
//         Ok(())
//         // unimplemented!("Application type equation")
//       }
//       ErlExpr::Let { var, var_ty, value, in_expr } => {
//         // Type of let is its expression type (not added to equations in this match clause)
//         // but also type of variable in let is added as an equation
//         result.push(TypeEquation::new(ast, var_ty, value.get_type()));
//         Ok(())
//       }
//       ErlExpr::Lit(..) => Ok(()), // Literals create no equation
//       ErlExpr::BinaryOp { .. } => {}
//       ErlExpr::UnaryOp { .. } => {}
//       ErlExpr::Case { .. } => {}
//     }
//   }
//
//   /// Retrieve Expression's type field
//   pub fn get_type(&self) -> &ErlType {
//     match self {
//       ErlExpr::Var { name: _, ty } => &ty,
//       ErlExpr::App { ty, .. } => &ty,
//       ErlExpr::Let { in_expr, .. } => in_expr.get_type(),
//       ErlExpr::Case { ty, .. } => &ty,
//       ErlExpr::Lit(lit) => lit.get_type(),
//       ErlExpr::BinaryOp { .. } => {}
//       ErlExpr::UnaryOp { .. } => {}
//     }
//   }
// }
