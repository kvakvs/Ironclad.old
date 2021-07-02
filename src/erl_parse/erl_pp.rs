// use pest::Parser;

#[derive(Parser)]
#[grammar = "erl_parse/erl_preprocess.pest"]
pub struct ErlPreprocessorParser;
