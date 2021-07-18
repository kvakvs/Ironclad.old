#![allow(clippy::upper_case_acronyms)]
#![allow(missing_docs)]

#[derive(Parser)]
#[grammar = "syntaxtree/erl/erl_grammar.pest"]
pub struct ErlParser;
