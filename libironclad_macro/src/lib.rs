#![feature(proc_macro_quote)]
// #![feature(trace_macros)]
// trace_macros!(true);

extern crate proc_macro;
extern crate syn;

use proc_macro::{quote, TokenStream};
use syn::__private::ToTokens;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{parse_macro_input, Token};

struct MakeTokMatchFnInput {
  pub name: syn::Ident,
  pub token_type: syn::Ident,
}

impl Parse for MakeTokMatchFnInput {
  fn parse(input: ParseStream) -> syn::Result<Self> {
    println!("input: {:?}", input);
    let mut args_punct = Punctuated::<syn::Ident, Token![,]>::parse_terminated(input)?;
    let name = args_punct.pop().unwrap();
    let token_type = args_punct.pop().unwrap();
    println!("name: {:?}", &name.value());
    println!("token_type: {:?}", &token_type.value());
    Ok(Self {
      name: name.into_value(),
      token_type: token_type.into_value(),
    })
  }
}

#[proc_macro]
pub fn make_tok_match_fn(input_stream: TokenStream) -> TokenStream {
  let input = parse_macro_input!(input_stream as MakeTokMatchFnInput);

  let name = input.name.into_token_stream();
  let token_type = input.token_type.into_token_stream();

  quote!(
    pub fn $name(input: ParserInput) -> ParserResult<()> {
      map(ws_before(tok(TokenKind::$token_type)), void_fn)(input)
    }
  )
}
