//! Interpret AST tree and replace preprocessor nodes and `?macros` with their result.

// /// Process top level nodes in a module. At this level preprocessor nodes can occur and they need
// /// to be interpreted.
// #[named]
// pub fn interpret_preprocessor_node(module: &mut ErlModule, node: &AstNode) -> IcResult<AstNode> {
//   match node.content {
//     ErlAstType::Token { .. } | ErlAstType::Empty => {
//       unreachable!(
//         "{}:{}: {} is not an allowed AST node type in the final preprocessed tree",
//         file!(),
//         function_name!(),
//         node.content
//       )
//     }
//     ErlAstType::ModuleRoot { .. } => {}
//     ErlAstType::ExportAttr { .. } => {}
//     ErlAstType::ExportTypesAttr { .. } => {}
//     ErlAstType::TypeAttr { .. } => {}
//     ErlAstType::ImportAttr { .. } => {}
//     ErlAstType::GenericAttr { .. } => {}
//     ErlAstType::FnDef(_) => {}
//     ErlAstType::FnRef { .. } => {}
//     ErlAstType::FnSpec { .. } => {}
//     ErlAstType::Type { .. } => {}
//     ErlAstType::CClause(_, _) => {}
//     ErlAstType::MFA { .. } => {}
//     ErlAstType::Var(_) => {}
//     ErlAstType::Apply(_) => {}
//     ErlAstType::CaseStatement { .. } => {}
//     ErlAstType::Lit { .. } => {}
//     ErlAstType::BinaryOp { .. } => {}
//     ErlAstType::UnaryOp { .. } => {}
//     ErlAstType::List { .. } => {}
//     ErlAstType::Tuple { .. } => {}
//     ErlAstType::MapBuilder { .. } => {}
//     ErlAstType::CommaExpr { .. } => {}
//     ErlAstType::ListComprehension { .. } => {}
//     ErlAstType::ListComprehensionGenerator { .. } => {}
//     ErlAstType::TryCatch { .. } => {}
//     ErlAstType::IfStatement { .. } => {}
//     ErlAstType::BinaryExpr { .. } => {}
//     ErlAstType::RecordDefinition { .. } => {}
//     ErlAstType::Preprocessor(_) => {}
//   }
// }
