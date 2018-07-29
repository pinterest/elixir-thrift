# Contributing

First off, thanks for taking the time to contribute! We welcome contributions
from everyone, and this guide will answer some common questions about how this
project works.

## Making Changes

1. Fork this repository to your own account
2. Make your changes and verify that `mix test` passes
3. Commit your work and push to a topic branch on your fork
4. Submit a [pull request](https://github.com/pinterest/elixir-thrift/compare/)

To increase changes that your pull request will be accepted:

- Follow our [style guide](#Style)
- Write tests for your changes
- Write a good commit message

## Style

We plan to adopt [`mix format`][]-based code formatting soon. The format will
be enforced by Travis CI, so please make sure your code is well formatted
*before* you push your branch so those checks will pass.

We also use [Credo][] for static analysis and code consistency. You can run it
locally via `mix credo`. [Ebert][] will also comment on any issues it finds in
pull requests.

[Credo]: https://github.com/rrrene/credo
[Ebert]: https://ebertapp.io/github/pinterest/elixir-thrift
[`mix format`]: https://hexdocs.pm/mix/Mix.Tasks.Format.html

## Testing

### Static Test Data

Some serialization tests use static test data to verify their correctness.
These data files live under `test/data/` and have `.thriftdata` extensions.

If you're changing any of the serialization routines, or adding some new code
paths, you may need to (re)generate some of these data files. If that's the
case, check out [`test/data/README.md`](test/data/README.md) for instructions.
