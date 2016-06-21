# Thrift Utilities for Elixir

[![Build Status](https://travis-ci.org/pinterest/elixir-thrift.svg?branch=master)](https://travis-ci.org/pinterest/elixir-thrift)
[![Coverage Status](https://coveralls.io/repos/pinterest/elixir-thrift/badge.svg?branch=master&service=github)](https://coveralls.io/github/pinterest/elixir-thrift?branch=master)
![License](https://img.shields.io/badge/license-Apache%202-blue.svg)

This package contains a handful of useful utilities for working with
[Thrift](https://thrift.apache.org/) in Elixir.

In particular, it includes a copy of the Erlang Thrift runtime library.

## Setup

Start by adding this package to your project as a dependency:

```elixir
{:thrift, "~> 1.2"}
```

Or to track the GitHub master branch:

```elixir
{:thrift, github: "pinterest/elixir-thrift", submodules: true}
```

## Mix

This package includes a Mix compiler task that can be used to automate Thrift
code generation. Start by adding `:thrift` to your project's `:compilers` list.
For example:

```elixir
compilers: [:thrift | Mix.compilers]
```

It's important to add `:thrift` *before* the `:erlang` entry. The Thrift
compiler will generate Erlang source files, and we currently rely on this
ordering to ensure those generated source files get compiled.

Next, define the list of `:thrift_files` that should be compiled. In this
example, we gather all of the `.thrift` files under the `thrift` directory:

```elixir
thrift_files: Mix.Utils.extract_files(["thrift"], [:thrift])
```

By default, the generated source files will be written to the `src` directory,
but you can change that using the `thrift_output` option.

You can also pass additional options to the Thrift compiler by listing them in
the `thrift_options` option:

```elixir
thrift_options: ~w[-I my/include/dir]
```

If you require a specific version of the Thrift compiler, you can specify a
version requirement using the `thrift_version` option. Version requirements
use the [SemVer 2.0 schema][semver]. For example:

```elixir
thrift_version: ">= 0.9.3"  # Erlang maps support
```

You can also override the name of the Thrift compiler executable itself:

```elixir
thrift_executable: "thrift-0.9.3"
```

If you get something like `type set() undefined` when compiling the generated files
you can try:

```elixir
thrift_options: ~w[--gen erl:maps]
```


## Thrift IDL Parsing

This package also contains experimental support for parsing [Thrift IDL][idl]
files. For the moment, only an Erlang lexer is available, but the goal is to
provide more advanced parser support over time.

```erlang
:thrift_lexer.string('enum Colors { RED, GREEN, BLUE}')
{:ok,
 [{:enum, 1}, {:ident, 1, 'Colors'}, {:symbol, 1, '{'}, {:ident, 1, 'RED'},
  {:symbol, 1, ','}, {:ident, 1, 'GREEN'}, {:symbol, 1, ','},
  {:ident, 1, 'BLUE'}, {:symbol, 1, '}'}], 1}
```

[semver]: http://semver.org/
[idl]: https://thrift.apache.org/docs/idl
