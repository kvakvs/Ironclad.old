// /// Generic module attribute -"string"(value, ...).
// pub struct ModuleAttr {
//   pub name: String,
//   pub values: Vec<ErlAstNode>,
// }

pub struct Atom {
  name: String,
}

pub struct FunArity {
  pub name: Atom,
  pub arity: usize,
}

pub struct TypeArity {
  pub name: Atom,
  pub arity: usize,
}

pub struct Variable {
  name: String,
}

pub struct TypeDef {
  pub name: Atom,
  pub type_args: Vec<Variable>,
}

pub enum ErlAstNode {
  // The code
  FunctionDef(Atom, usize),
}
