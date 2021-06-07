# Typed ERLC

> NOTE: This is an early stage work-in-progress.

This project is an alternate Erlang compiler, a fusion of OTP's `lib/compiler` and `lib/dialyzer` designed to load,
analyze and compile all `.erl` files in your project together as a single unit, and perform live type deduction and type
checking following the standard Erlang `-spec()` and `-type()` syntax.

## Project File `erlproject.toml`

> NOTE: This is an early stage work-in-progress.

To perform the task, the tool needs to know all input files before the work begins. Hence the project file syntax is
introduced (experimental). The default name for the project file is `erlproject.toml`, and the contents may look like:

```toml
[compiler_options]
include_paths = []          # default []

[inputs]
files = ["*.erl"]           # default ["*.erl"]
directories = ["src"]       # default ["."]
exclude_files = []          # default []
exclude_directories = []    # default []
```

An empty `erlproject.toml`, is acceptable, consisting of comments, or no bytes at all. In this case entire current
directory will be scanned for `"*.erl"` files, with all nested subdirectories.

## Project Progress

> NOTE: This is an early stage work-in-progress.

- [x] Project configuration
- [ ] Preprocessor 
- [ ] Erlang syntax parser
- [ ] ???
- [ ] Code generator
