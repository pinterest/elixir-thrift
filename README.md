# Thrift Utilities for Elixir

[![Build Status](https://travis-ci.org/pinterest/elixir-thrift.svg?branch=master)](https://travis-ci.org/pinterest/elixir-thrift)
[![Coverage Status](https://coveralls.io/repos/pinterest/elixir-thrift/badge.svg?branch=master&service=github)](https://coveralls.io/github/pinterest/elixir-thrift?branch=master)

This package contains a handful of useful utilities for working with
[Thrift](https://thrift.apache.org/) in Elixir.

## Setup

Start by adding this package to your project as a dependency:

```elixir
{:thrift, "~> 1.3"}
```

Or to track the GitHub master branch:

```elixir
{:thrift, github: "pinterest/elixir-thrift"}
```

## Mix

This package includes a Mix compiler task that can be used to automate Thrift
code generation. Start by adding `:thrift` to your project's `:compilers` list.
For example:

```elixir
compilers: [:thrift | Mix.compilers]
```

It's important to add `:thrift` *before* the `:elixir` entry. The Thrift
compiler will generate Elixir source files, and we currently rely on this
ordering to ensure those generated source files get compiled.

Next, define the list of `:thrift_files` that should be compiled. In this
example, we gather all of the `.thrift` files under the `thrift` directory:

```elixir
thrift_files: Mix.Utils.extract_files(["thrift"], [:thrift])
```

By default, the generated source files will be written to the `lib` directory,
but you can change that using the `thrift_output` option.


## Thrift IDL Parsing

This package also contains experimental support for parsing [Thrift IDL][idl]
files. It is built on a low-level Erlang lexer and parser:

```erlang
{:ok, tokens, _} = :thrift_lexer.string('enum Colors { RED, GREEN, BLUE}')
{:ok,
 [{:enum, 1}, {:ident, 1, 'Colors'}, {:symbol, 1, '{'}, {:ident, 1, 'RED'},
  {:symbol, 1, ','}, {:ident, 1, 'GREEN'}, {:symbol, 1, ','},
  {:ident, 1, 'BLUE'}, {:symbol, 1, '}'}], 1}

{:ok, schema} = :thrift_parser.parse(tokens)
{:ok,
 %Thrift.Parser.Models.Schema{constants: %{},
  enums: %{Colors: %Thrift.Parser.Models.TEnum{name: :Colors,
     values: [RED: 1, GREEN: 2, BLUE: 3]}}, exceptions: %{}, includes: [],
  namespaces: %{}, services: %{}, structs: %{}, thrift_namespace: nil,
  typedefs: %{}, unions: %{}}}
```

But also provides a high-level Elixir parsing interface:

```elixir
Thrift.Parser.parse("enum Colors { RED, GREEN, BLUE}")
%Thrift.Parser.Models.Schema{constants: %{},
 enums: %{Colors: %Thrift.Parser.Models.TEnum{name: :Colors,
    values: [RED: 1, GREEN: 2, BLUE: 3]}}, exceptions: %{}, includes: [],
 namespaces: %{}, services: %{}, structs: %{}, thrift_namespace: nil,
 typedefs: %{}, unions: %{}}
```

[idl]: https://thrift.apache.org/docs/idl
