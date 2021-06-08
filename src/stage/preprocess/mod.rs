use errloc_macros::errloc;
use crate::project::ErlProject;
use std::sync::{Arc, RwLock};
use crate::stage::file_contents_cache::FileContentsCache;
use std::path::PathBuf;
use std::borrow::{BorrowMut, Borrow};
use crate::erl_error::{ErlResult, ErlError};

fn preprocess_file(file_name: &PathBuf, file_contents: &String, cache: &mut FileContentsCache) {
    let output = String::new();
    cache.contents.insert(file_name.clone(), output);
}

/// Preprocessor stage
/// * Pre-parse loaded files being only interested in preprocessor tokens.
/// * Pre-parse and paste include files AST into the code.
/// * Drop AST branches covered by the conditional compile directives.
/// Returns: preprocessed collection of module sources
pub fn run(project: &mut ErlProject,
           file_contents: Arc<RwLock<FileContentsCache>>,
) -> ErlResult<Arc<RwLock<FileContentsCache>>> {
    let mut preproc_cache = FileContentsCache::new();

    // Take only .erl files
    let file_contents_r = file_contents.try_read().unwrap();

    for (path, contents) in &file_contents_r.contents   {
        if path.to_string_lossy().ends_with(".erl") {
            preprocess_file(&path, &contents, preproc_cache.borrow_mut());
        }
    }

    println!("Preprocessed {} sources", preproc_cache.contents.len());

    let arc = Arc::new(RwLock::new(preproc_cache));
    Ok(arc)
}
