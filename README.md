# Thrift Utilities for Elixir

This package contains a handful of useful utilities for working with
[Thrift](https://thrift.apache.org/) in Elixir.

In particular, it includes a copy of the Erlang Thrift runtime library.

## Setup

Start by adding this package to your project as a dependency:

```elixir
{:thrift, git: ..., submodules: true}
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
thrift_files: Mix.Utils.extract_files(["thrift"], [:thrift]),
```

By default, the generated source files will be written to the `src` directory,
but you can change that using the `thrift_output` option.

You can also pass additional options to the Thrift compiler by listing them in
the `thrift_options` option.
