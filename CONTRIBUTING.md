# Contributing

First off, thanks for taking the time to contribute! This guide will answer
some common questions about how this project works.

While this is a Pinterest open source project, we welcome contributions from
everyone. Several regular outside contributors are also project maintainers.

## Making Changes

1. Fork this repository to your own account
2. Make your changes and verify that `mix test` passes
3. Commit your work and push to a topic branch on your fork
4. Submit a [pull request](https://github.com/pinterest/elixir-thrift/compare/)
5. Participate in the code review process by responding to feedback

Once there is agreement that the code is in good shape, one of the project's
maintainers will merge your contribution.

To increase the chances that your pull request will be accepted:

- Follow our [style guide](#Style)
- Write tests for your changes
- Write a good commit message

## Style

We use [`mix format`][]-based code formatting. The format will be enforced by
Travis CI, so please make sure your code is well formatted *before* you push
your branch so those checks will pass.

We also use [Credo][] for static analysis and code consistency. You can run it
locally via `mix credo`. [Ebert][] will also comment on any issues it finds in
pull requests.

[Credo]: https://github.com/rrrene/credo
[Ebert]: https://ebertapp.io/github/pinterest/elixir-thrift
[`mix format`]: https://hexdocs.pm/mix/Mix.Tasks.Format.html

## Testing

### Test Coverage

[![Coverage Status](https://coveralls.io/repos/pinterest/elixir-thrift/badge.svg?branch=master&service=github)](https://coveralls.io/github/pinterest/elixir-thrift?branch=master)

We think test coverage is important because it makes it less likely that
future changes will break existing functionality. Changes that drop the
project's overall test coverage below 90% will fail to pass CI.

Test coverage is measured using [Coveralls][]. You can generate a local
coverage report using `mix coveralls.detail` or `mix coveralls.html`.

[Coveralls]: https://coveralls.io/github/pinterest/elixir-thrift

### Static Test Data

Some serialization tests use static test data to verify their correctness.
These data files live under `test/data/` and have `.thriftbin` extensions.

If you're changing any of the serialization routines, or adding some new code
paths, you may need to (re)generate some of these data files. If that's the
case, check out [`test/data/README.md`](test/data/README.md) for instructions.
