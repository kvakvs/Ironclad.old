# Ironclad - The Erlang type checker

## The Vision

I have a dream, that one day there will be an Erlang compiler, which will generate high quality type-correct code from
deduced value usages and `-type` and `-spec`s. It will be able to print reasonable error messages about why the code is
broken and will suggest how to fix the errors.

But since there are only so many hours in the day and only 2 hands available, the work scope is limited to writing a
parser and a type checker before going any further. We aren't trying to solve hardest problems of typing the Erlang 
language (e.g. hot code reloading, typed messages, typed exceptions, and typed processed) but all comes in due time.

## The Problem We're Trying To Solve

1. Currently the work is aimed towards loading, parsing and type-checking Erlang program sources (without parse
   transforms).
   No new syntax is planned for Erlang, the tool will ingest standard Erlang/OTP sources.
   This more or less matches what [lib/dialyzer](https://github.com/erlang/otp/tree/master/lib/dialyzer)
   or [Gradualizer](https://github.com/josefs/Gradualizer) does.
2. The next step would be taking over the compiler's job and compiling the code to BEAM files,
   replacing [lib/compiler](https://github.com/erlang/otp/tree/master/lib/compiler). This is a goal in the distant
   future, might as well not happen.

The standard Erlang compiler and the type checking tool, Dialyzer, are two separate projects:
The compiler processes programs one file at a time, and never does it look at the rest of the program. The modules
are built to be interchangeable and hot reloadable, but often projects don't use hot code reloading and would rather
build and deploy entire project as a single unit. Dialyzer  loads all files in the project together, processes
everything as a single unit. Dialyzer does not compile Erlang code.

This tool, Ironclad, wants to become both.

## Project File

> NOTE: The file format may and will change.

To perform the task, the tool needs to know all input files before the work begins. Hence the project file syntax is
introduced (experimental). The default name for the project file is `ironclad.toml`, and the contents may look like:

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

An empty `ironclad.toml` is acceptable, which consists of comments, or has no bytes at all. In this case entire current
directory will be scanned for `"*.erl"` files, with all nested subdirectories.

## Work Progress

> NOTE: This is an early stage work-in-progress. The task list grows.

- Project configuration `libironclad`
  - [x] Basic TOML syntax to not be parsing Erlang terms early in project's life
  - [x] Load the config and overlay over the default settings
  - Preprocessor `libironclad_preprocessor`
    - [x] Parsing directives as a Preprocessor AST tree
    - [ ] Include directives `-include/include_lib`
    - [ ] Parse and interpret `-define/-if*/-else/-endif` directives by removing chunks of guarded code
    - [ ] Parse and interpret `-if(COND)`
    - [ ] Substitute `?MACRO`
    - [ ] Parse and interpret macros with arguments `-define(MACRO(X, Y), ...)`
    - [ ] Special macros (`?MODULE`, `?FILE`, `?LINE`, `?FUNCTION_NAME` etc)
----------
- Erlang syntax parser `libironclad_erlang`
    - [x] Common constructs
    - [x] Parser for functions
    - [x] Parser for lambdas
    - [x] Parser for types and typespecs
    - [x] Parser for `case`
    - [x] Parser for `try`
    - [x] Expressions and operator precedence
    - [ ] Module attributes
    - [ ] Binary syntax
    - [ ] Scientific float syntax
    - [ ] Maps syntax
    - [ ] Records syntax
    - [x] Typespec and types syntax
----------
- Type system and type inference engine `libironclad_erlang`
    - [x] Define types, union types, special (any, none, ...)
    - [x] Define expressions AST
    - [x] Add support for types in the AST
    - [x] Infer expression types
    - [x] Bidirectional typing: Synthesis (basic types)
    - [ ] Bidirectional typing: Narrowing
- ???
- [ ] Code generator
----------
- Harder problems 
  - [ ] Typed messages
  - [ ] Typed exceptions
  - [ ] Typed processes
  - ???
  - [ ] Hot code reloading