#![allow(clippy::upper_case_acronyms)]
#![allow(missing_docs)]

#[derive(Parser)]
#[grammar = "syntaxtree/pp/pp_grammar.pest"]
pub struct PpParser;
