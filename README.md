# Typed ERLC

## The Problem

I have a dream, that one day there will be an Erlang compiler, which will generate high quality type-correct code from
deduced value usages and `-type` and `-spec`s. It will be able to print reasonable error messages about why the code is
broken and will suggest how to fix the errors.

The current standard compiler, `lib/compiler` in OTP, and the type checking tool, Dialyzer in `lib/dialyzer`, are two
separate projects:

* The compiler processes programs one file at a time, and never does it look at the rest of the program. This is nice
  when you compile one module to drop a `.beam` file into a running program and have them work together. The majority of
  Erlang projects are built together, from the project root, as single collection of sources. They are packaged together
  as a single unit and all modules run together.
* On the contrary, Dialyzer is what the compiler should be, it loads every file in the project together, and processes
  everything as a single program, checking all possible type usages and finding the type-related problems with high
  precision. Dialyzer does not compile Erlang code.

## The `typed_erlc` Project

> NOTE: This is an early stage work-in-progress.

This project is an alternate Erlang compiler, a fusion of OTP's `lib/compiler` and `lib/dialyzer` designed to load,
analyze and compile all `.erl` files in your project together as a single unit, and perform live type deduction and type
checking following the standard Erlang `-spec()` and `-type()` syntax.

## Project File `erlproject.toml`

> NOTE: The file format may and will change.

To perform the task, the tool needs to know all input files before the work begins. Hence the project file syntax is
introduced (experimental). The default name for the project file is `erlproject.toml`, and the contents may look like:

```toml
[compiler_options]
include_paths = []          # default []

[inputs]
files = ["*.erl"]           # default ["*.erl"]
directories = ["src"]       # default ["."]
exclude_files = []          # TODO: not implemented; default []
exclude_directories = []    # TODO: not implemented; default []
```

You can use `**` to match any portion of the path.

An empty `erlproject.toml`, is acceptable, consisting of comments, or no bytes at all. In this case entire current
directory will be scanned for `"*.erl"` files, with all nested subdirectories.

## Project Progress

> NOTE: This is an early stage work-in-progress. The task list grows.

- [x] Project configuration
- [ ] Preprocessor
    - [x] Parse and interpret `-define/-if*/-else/-endif` directives by removing chunks of guarded code
    - [x] Parse `-include/include_lib` directives
    - [ ] Perform inclusion
    - [ ] Parse and interpret `-if(COND)`
    - [ ] Substitute `?MACRO`
    - [ ] Parse and interpret macros with arguments `-define(MACRO(X, Y), ...)`
- [ ] Erlang syntax parser
    - [ ] Common keywords and constructs
    - [ ] Module attributes
    - [ ] Binary syntax
    - [ ] Scientific float syntax
    - [ ] Maps syntax
    - [ ] Records syntax
    - [ ] Typespec syntax
- [ ] Type system and type inference engine
    - [x] Define types, union types, special (any, none, ...)
    - [x] Define expressions AST
    - [x] Add support for types in the AST
    - [ ] Infer expression types
- [ ] ???
- [ ] Code generator
