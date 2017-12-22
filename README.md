# A Pure Elixir Thrift Library

[![Build Status](https://travis-ci.org/pinterest/elixir-thrift.svg?branch=master)](https://travis-ci.org/pinterest/elixir-thrift)
[![Coverage Status](https://coveralls.io/repos/pinterest/elixir-thrift/badge.svg?branch=master&service=github)](https://coveralls.io/github/pinterest/elixir-thrift?branch=master)

This package contains an implementation of [Thrift](https://thrift.apache.org/) for Elixir.
It includes a Thrift IDL parser, a code generator, a binary framed client and a binary framed server.

The serialization and deserialization code that is generated by this project is highly
optimized and is between **10 and 25 times faster**<sub>[why?](#why-is-it-faster-than-the-apache-implementation)</sub> than the code generated by the
Apache Erlang implementation.

#### Binary protocol benchmark
(run `mix bench bench/binary_protocol_benchmark.exs`)

Benchmark name                              | Iterations | Average time
--------------------------------------------|------------|-------------
elixir serialization (left as IOList)       |     2000   | 810.53 µs/op
elixir deserialization                      |     1000   | 1234.69 µs/op
elixir serialization (converted to binary)  |     1000   | 1254.23 µs/op
erlang serialization left as IOList         |      100   | 10544.31 µs/op
erlang serialization (converted to binary)  |      100   | 11714.74 µs/op
erlang deserialization                      |      100   | 21671.39 µs/op


*Note: all serialization in this framework leaves its results in iolists for speed and efficiency.*

#### Framed Server Benchmark
(run `mix bench bench/framed_server_benchmark.exs`)

Benchmark name                 | Iterations   | Average time
-------------------------------|--------------|--------------
Returning a boolean in Elixir  |     50000    | 51.20 µs/op
Returning a boolean in Erlang  |     20000    | 74.46 µs/op
Echoing a struct in Elixir     |     10000    | 275.89 µs/op
Echoing a struct in Erlang     |      1000    | 1200.35 µs/op

*Note: The Erlang parts of the above benchmark utilized the generated Erlang client
and server from the Apache Thrift project*

*Benchmarks were run on a 2.8Ghz MacbookPro with 16G of ram running macOS Sierra, using Elixir 1.3.4 and Erlang 19.1*

## Setup

Start by adding this package to your project as a dependency:

```elixir
{:thrift, "~> 2.0"}
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
compiler will generate Elixir source files, which are in turn compiled by the
`:elixir` compiler.

Next, configure the compiler using a new Keyword list under the top-level
`:thrift` configuration key. The only necessary compiler option is `:files`,
which defines the list of Thrift files that should be compiled.

By default, the generated source files will be written to the `lib` directory,
but you can change that using the `output_path` option.

In this example, we gather all of the `.thrift` files under the `thrift`
directory and write our output to the `lib/thrift/` directory:

```elixir
# example mix.exs
defmodule MyProject.Mixfile do
  # ...

  def project do
    [
      # other settings...
      thrift: [
        files: Path.wildcard("thrift/**/*.thrift"),
        output_path: "lib/generated"
      ]
    ]
  end
end
```

## Working with Thrift

The examples below use the following thrift definition:

```thrift
namespace elixir Thrift.Test
exception UserNotFound {
  1: string message
}

struct User {
  1: i64 id,
  2: string username,
  3: string first_name,
  4: string last_name
}

service UserService {
  1: bool ping(),
  2: User get_user_by_id(1: i64 user_id) throws (1: UserNotFound unf),
  3: boolean deleteUser(1: i64 userId),
}
```

#### The generated code will be placed in the following modules:

Generated Code | Path            |Output Module
---------------|-----------------|--------------
User Struct    | lib/thrift/test/user.ex |`Thrift.Test.User`
UserNotFound Exception| lib/thrift/test/user_not_found.ex | `Thrift.Test.UserNotFound`
User Binary Protocol | lib/thrift/test/user.ex | `Thrift.Test.User.BinaryProtocol`
UserNotFound Binary Protocol | lib/thrift/test/user_not_found.ex | `Thrift.Test.UserNotFound.BinaryProtocol`
UserService Framed Binary Client | lib/thrift/test/user_service.ex |`Thrift.Test.UserService.Binary.Framed.Client`
UserService Framed Binary Server | lib/thrift/test/user_service.ex | `Thrift.Test.UserService.Binary.Framed.Server`
UserService Handler Behviour (Used for writing servers) | lib/thrift/test_user_service/handler.ex  | `Thrift.Test.UserService.Handler`


## Using the Client
The client includes a static module that does most of the work, and a generated
interface module that performs some conversions and makes calling remote
functions easier. You will not directly interface with the static module,
but it is the one that's started when `start_link` is called.

The static client module uses [James Fish](http://github.com/fishcakez)'s
excellent [connection](http://github.com/fishcakez/connection) behaviour.

For each function defined in the service, the generated module has
four functions.

Function name  | Description
---------------|----------
`get_user_by_id/2` | Makes a request to the remote `get_user_by_id` RPC. Returns `{:ok, response}` or `{:error, reason}` tuples.
`get_user_by_id!/2`  | Same as above, but raises an exception if something goes wrong. The type of exception can be one of the exceptions defined in the service or `Thrift.TApplicationException`.
`get_user_by_id_with_options/3` | Allows you to pass `gen_tcp` and `GenServer` options to your client. This is useful for setting the `GenServer` timeout if you expect your RPC to take longer than the default of 5 seconds. Like `get_user_by_id/2`, this function returns `{:ok, response}` or `{:error, reason}` tuples.
`get_user_by_id_with_options!/3` | Allows you to pass `gen_tcp` and `GenServer` options and raises an exception if an error occurs.

**Note:** in the above example, the function `deleteUser` will be converted to `delete_user` to comply with Elixir's [naming conventions](http://elixir-lang.org/docs/stable/elixir/naming-conventions.html).

To use the client, simply call `start_link`, supplying the host and port.

```elixir
iex> alias Thrift.Test.UserService.Binary.Framed.Client
iex> {:ok, client} = Client.start_link("localhost", 2345, [])
iex> {:ok, user} = Client.get_user_by_id(client, 22451)
{:ok, %Thrift.Test.User{id: 22451, username: "stinky", first_name: "Stinky", last_name: "Stinkman"}}
```

The client supports the following options, which are passed in as the
third argument to `start_link`:

Option name      |  Type | Description
-----------------|-------|-------------
`:tcp_opts` | keyword | A keyword list of tcp options (see below)
`:gen_server_opts` | keyword | A keyword list of options for the gen server (see below)

##### TCP Opts

Name             | Type | Description
-----------------|------|---------------
`:timeout`       | positive integer | The default timeout for reading from, writing to, and connecting to sockets.
`send_timeout`   | positive integer | The amount of time in milliseconds to wait before sending data fails.

##### GenServer Opts
Name             | Type | Description
-----------------|------|---------------
`timeout`        | A positive integer | The amount of time in milliseconds the Client's GenServer waits for a reply. After this, the GenServer will exit with `{:error, :timeout}`.


### Example of using options

```elixir
alias Thrift.Test.UserService.Binary.Framed.Client
{:ok, client} = Client.start_link("localhost", 2345,
                tcp_opts: [], gen_server_opts: [timeout: 10_000])

```
These options set the GenServer timeout to be ten seconds, which means the remote
side can take its time to reply.


## Using The Server
Creating a thrift server is slightly more involved than creating the client, because
you need to create a module to handle the work. Fortunately, Elixir Thrift
creates a [Behaviour](http://elixir-lang.org/docs/stable/elixir/behaviours.html#content),
complete with correct success typing, for this module. To implement this behaviour,
use the `@behaviour` module attribute. The compiler will now inform you about
any missed functions.

Here is an implementation for the server defined above:

```elixir
defmodule UserServiceHandler do
  @behaviour Thrift.Test.UserService.Handler

  def ping, do: true

  def get_user_by_id(user_id) do
    case Backend.find_user_by_id(user_id) do
      {:ok, user} ->
        user
      {:error, _} ->
        raise Thrift.Test.UserNotFound.exception message: "could not find user with id #{user_id}"
    end
  end

  def delete_user(user_id) do
    Backend.delete_user(user_id) == :ok
  end
end
```

To start a server with UserServiceHandler as the callback module:

```elixir
{:ok, server_pid} = Thrift.Test.UserService.Binary.Framed.Server.start_link(UserServiceHandler, 2345, [])
```

...and your server is up and running. RPC calls to the server are delegated to UserServiceHandler.

Like the client, the server takes several options. They are:

Name           |  Type | Description
---------------|-------|-------------
`worker_count`   | positive integer | The number of acceptor workers available to take requests
`name`  | atom | (Optional) The name of the server. The server's pid becomes registered to this name. If not specified, the handler module's name is used.
`max_restarts` | non negative integer | The number of times to restart (see the next option)
`max_seconds`  | non negative integer | The number of seconds. This is used by the supervisor to determine when to crash. If a server restarts `max_restarts` times in `max_seconds` then the supervisor crashes.

The server defines a Supervisor, which can be added to your application's supervision tree. When adding the server to your applications supervision tree, use the `supervisor` function rather than the `worker` function.

## Using the binary protocol directly

Each thrift struct, union and exception also has a `BinaryProtocol` module generated for
it. This module lets you serialize and deserialize its own type easily.

For example:

```elixir
iex(1)> serialized = %User{username: "stinky" id: 1234, first_name: "Stinky", last_name: "Stinkman"}
|> User.BinaryProtocol.serialize
|> IO.iodata_to_binary
iex(2)> User.BinaryProtocol.deserialize(serialized)
{%User{username: "stinky" id: 1234, first_name: "Stinky", last_name: "Stinkman"}, ""}
```

The return value of the `serialize` function is an [iodata]. You can pass it through `IO.iodata_to_binary` to convert it to a binary. You also can write the iodata directly to a file or socket without converting it.

## Other Features
### Thrift IDL Parsing

This package also contains support for parsing [Thrift IDL][idl]
files. It is built on a low-level Erlang lexer and parser:

```elixir
{:ok, tokens, _} = :thrift_lexer.string('enum Colors { RED, GREEN, BLUE }')
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
Thrift.Parser.parse("enum Colors { RED, GREEN, BLUE }")
%Thrift.Parser.Models.Schema{constants: %{},
 enums: %{Colors: %Thrift.Parser.Models.TEnum{name: :Colors,
    values: [RED: 1, GREEN: 2, BLUE: 3]}}, exceptions: %{}, includes: [],
 namespaces: %{}, services: %{}, structs: %{}, thrift_namespace: nil,
 typedefs: %{}, unions: %{}}
```

You can use these features to support additional languages, protocols, and servers.

[idl]: https://thrift.apache.org/docs/idl
[iodata]: http://elixir-lang.org/getting-started/io-and-the-file-system.html#iodata-and-chardata

## Quick and dirty FAQ

### Why is it faster than the Apache implementation?
The Apache Thrift implementation uses C++ to write Erlang modules that describe Thrift data
structures, then uses that description to turn your Thrift data into bytes. It consults this
description every time Thrift data is serialized/deserialized. This on-the-fly conversion
costs CPU time.

Additionally, the separation of concerns in Thrift prevent the Erlang VM from doing the best
job that it can do during serialization.

On the other hand, this implementation uses Elixir to write Elixir code that's specific
 to _your_ thrift data. This serialization logic is then compiled, and that compiled code
is what converts your data to/from bytes. We've spent a lot of time making sure
that the generated serialization code takes advantage of several of the optimizations
that the Erlang VM provides.

### What tradeoffs have you made to get this performance?

Thrift has the following concepts:

   1. **Protocols** Define a conversion of data into bytes.
   2. **Transports** Define how bytes move; across a network or in and out of a file.
   3. **Processors** Encapsulate reading from streams and doing something with the data. Processors are generated by the Thrift compiler.

In Apache Thrift, Protocols and Transports can be mixed and matched. However, our implementation
does the mixing and matching for you and generates a combination of (Protocol + Transport +
Processor). This means that if you need to create a new Protocol or Transport, you need to
integrate it into this project.

Presently, we implement:

* Binary Protocol, Framed Client
* Binary Protocol, Framed Server

We are more than willing to take PRs to add more. We intend to also add TMux and Finagle servers
in the future.
