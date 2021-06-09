use crate::project::ErlProject;
use std::sync::{Arc, RwLock};
use crate::stage::file_contents_cache::FileContentsCache;
use std::path::PathBuf;
use std::borrow::{BorrowMut, Borrow};
use crate::erl_error::{ErlResult, ErlError, ErrorLocation};
use crate::stage::ast_cache::{ASTCache, ModuleAST};
use crate::erl_parse;
use crate::erl_parse::ast::ASTNode;

/// Run syntax parser on an ERL or HRL source file
fn parse_file(file_name: &PathBuf, file_contents: &String) -> ErlResult<Vec<ASTNode>> {
    let (remaining, ast_tree) = erl_parse::parse_module(&file_contents)?;
    if remaining.is_empty() {
        Ok(ast_tree)
    } else {
        let msg = String::from("Source file still contains unconsumed data after parse");
        let src_file_str = file_name.to_string_lossy().to_string();
        Err(ErlError::ErlParseError(ErrorLocation::SourceFile(src_file_str), msg))
    }
}

/// Parse stage
/// * Parse loaded ERL files.
/// Returns: Collection of AST trees for all affected ERL modules
pub fn run(project: &mut ErlProject,
           contents_cache: Arc<RwLock<FileContentsCache>>) -> ErlResult<Arc<RwLock<ASTCache>>> {
    let mut ast_cache = ASTCache::new();

    let file_contents_r = contents_cache.read().unwrap();

    for (path, contents) in &file_contents_r.contents {
        let path_s = path.to_string_lossy();

        // Take only .erl and .hrl files
        if path_s.ends_with(".erl") || path_s.ends_with(".hrl") {
            let ast_tree = parse_file(&path, &contents)?;
            ast_cache.syntax_trees.insert(path.clone(),
                                          ModuleAST::new(ast_tree));
        }
    }

    println!("Parsed {} sources (.erl and .hrl)", ast_cache.syntax_trees.len());

    let arc = Arc::new(RwLock::new(ast_cache));
    Ok(arc)
}
